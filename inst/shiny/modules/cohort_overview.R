# cohort_overview.R
# Shiny module: reactive quilt plot for cohort-level bird's-eye overview.
#
# The quilt is a patient × time-window × domain heatmap. Each column group
# is one clinical domain; within each group, columns are time windows relative
# to the cohort index date; rows are patients sorted by cluster, rank, or ID.
#
# Module arguments:
#   id                 — Shiny module ID
#   quilt_base         — reactiveVal() holding the full build_quilt_data() tibble
#   selected_patient   — reactiveVal(NULL) shared with trajectory_viewer + anomaly_explorer
#   time_window_labels — character vector of all window labels (for slider init)

# ---------------------------------------------------------------------------
# Domain colour palettes (sequential: white → mid → high)
# ---------------------------------------------------------------------------

.DOMAIN_COLORSCALES <- list(
  condition   = list(c(0, "#FFFFFF"), c(0.5, "#FDAE61"), c(1, "#D73027")),
  drug        = list(c(0, "#FFFFFF"), c(0.5, "#74ADD1"), c(1, "#313695")),
  procedure   = list(c(0, "#FFFFFF"), c(0.5, "#A6D96A"), c(1, "#1A9641")),
  measurement = list(c(0, "#FFFFFF"), c(0.5, "#CAB2D6"), c(1, "#6A3D9A")),
  observation = list(c(0, "#FFFFFF"), c(0.5, "#FDB863"), c(1, "#B35806")),
  visit       = list(c(0, "#FFFFFF"), c(0.5, "#C7E9B4"), c(1, "#41B6C4")),
  death       = list(c(0, "#FFFFFF"), c(0.5, "#888888"), c(1, "#111111"))
)

.DOMAIN_HIGH_COLORS <- c(
  condition   = "#D73027",
  drug        = "#313695",
  procedure   = "#1A9641",
  measurement = "#6A3D9A",
  observation = "#B35806",
  visit       = "#41B6C4"
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

#' Cohort overview (quilt plot) module UI
#'
#' @param id Shiny module namespace ID.
#' @export
cohort_overviewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # ── Control row ─────────────────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(3,
        shinyWidgets::pickerInput(
          ns("filter_clusters"),
          label    = "Clusters",
          choices  = NULL,
          multiple = TRUE,
          options  = shinyWidgets::pickerOptions(
            actionsBox = TRUE, liveSearch = TRUE,
            selectedTextFormat = "count > 2",
            countSelectedText  = "{0} clusters selected"
          )
        )
      ),
      shiny::column(3,
        shinyWidgets::checkboxGroupButtons(
          ns("filter_domains"),
          label     = "Domains",
          choices   = c("condition","drug","procedure","measurement",
                         "observation","visit","death"),
          selected  = c("condition","drug","procedure","measurement",
                         "observation","visit","death"),
          justified = TRUE,
          size      = "xs",
          direction = "horizontal"
        )
      ),
      shiny::column(2,
        shinyWidgets::pickerInput(
          ns("filter_tiers"),
          label    = "Priority Tier",
          choices  = c("All","high","medium","low","unranked"),
          selected = "All"
        )
      ),
      shiny::column(2,
        shiny::selectInput(
          ns("sort_by"),
          label    = "Sort Patients By",
          choices  = c("cluster","rank","subject_id"),
          selected = "cluster"
        )
      ),
      shiny::column(2,
        shiny::selectInput(
          ns("value_encoding"),
          label    = "Cell Value",
          choices  = c("log1p_count","binary","count"),
          selected = "log1p_count"
        )
      )
    ),
    # ── Time window slider ───────────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(12,
        shinyWidgets::sliderTextInput(
          ns("window_range"),
          label       = "Time Windows",
          choices     = "",
          selected    = "",
          grid        = TRUE,
          force_edges = TRUE
        )
      )
    ),
    # ── Main quilt plot ──────────────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(12,
        plotly::plotlyOutput(ns("quilt_plot"), height = "600px", width = "100%")
      )
    ),
    # ── Selection info bar ───────────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(12,
        shiny::uiOutput(ns("selection_info"))
      )
    ),
    # ── Download buttons ─────────────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(6,
        shiny::downloadButton(ns("download_png"), "Download PNG",
                              class = "btn-sm btn-default"),
        shiny::downloadButton(ns("download_svg"), "Download SVG",
                              class = "btn-sm btn-default")
      ),
      shiny::column(6,
        shiny::actionButton(ns("clear_selection"), "Clear Selection",
                            icon  = shiny::icon("times"),
                            class = "btn-warning btn-sm")
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

#' Cohort overview (quilt plot) module server
#'
#' @param id Shiny module namespace ID.
#' @param quilt_base `reactiveVal` (or `reactive`) holding the full
#'   pre-computed tibble from [build_quilt_data()]. Owned by `server.R`.
#' @param selected_patient `reactiveVal(NULL)` shared with the trajectory
#'   viewer and anomaly explorer modules. Written here on cell click.
#' @param time_window_labels Character vector of all window labels in order.
#'   Used to initialise the time-window slider.
#' @export
cohort_overviewServer <- function(id,
                                   quilt_base,
                                   selected_patient) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Derive window labels from data so they stay in sync with the loaded cohort
    win_labels_rv <- shiny::reactive({
      shiny::req(quilt_base())
      sort(unique(quilt_base()$window_label))
    })

    # ── 1. Initialise controls once data is available ────────────────────────
    shiny::observe({
      shiny::req(quilt_base())
      base       <- quilt_base()
      win_labels <- win_labels_rv()

      clusters <- sort(unique(base$cluster_id))
      cluster_choices <- stats::setNames(
        as.character(clusters),
        dplyr::case_when(
          clusters == -1L ~ "Noise (-1)",
          clusters == 0L  ~ "Unassigned (0)",
          TRUE            ~ paste("Cluster", clusters)
        )
      )
      shinyWidgets::updatePickerInput(
        session, "filter_clusters",
        choices  = cluster_choices,
        selected = as.character(clusters)
      )

      if (length(win_labels) >= 2L) {
        shinyWidgets::updateSliderTextInput(
          session, "window_range",
          choices  = win_labels,
          selected = c(win_labels[[1L]], win_labels[[length(win_labels)]])
        )
      }
    })

    # ── 2. Filtered + re-encoded quilt reactive ──────────────────────────────
    quilt_filtered <- shiny::reactive({
      # Only require quilt_base -- all filter inputs are handled with
      # safe defaults so the quilt renders immediately on cohort load
      # without waiting for controls to be initialised.
      shiny::req(quilt_base())

      d <- quilt_base()

      # Cluster filter (NULL / character(0) = show all)
      clusters_sel <- input$filter_clusters
      if (length(clusters_sel) > 0L) {
        d <- dplyr::filter(d, as.character(cluster_id) %in% clusters_sel)
      }

      # Domain filter (NULL = show all)
      domains_sel <- input$filter_domains
      if (length(domains_sel) > 0L) {
        d <- dplyr::filter(d, domain %in% domains_sel)
      }

      # Tier filter (NULL or "All" = show all)
      tier_sel <- input$filter_tiers %||% "All"
      if (nzchar(tier_sel) && tier_sel != "All") {
        d <- dplyr::filter(d, priority_tier == tier_sel)
      }

      # Window range filter (skip when slider not yet initialised)
      wr     <- input$window_range
      labels <- win_labels_rv()
      if (length(wr) >= 2L && all(nzchar(wr)) && length(labels) >= 2L) {
        win_min <- match(wr[1], labels)
        win_max <- match(wr[2], labels)
        if (!is.na(win_min) && !is.na(win_max)) {
          d <- dplyr::filter(d, window_idx >= win_min, window_idx <= win_max)
        }
      }

      # Re-encode fill value (default log1p_count)
      encoding <- input$value_encoding %||% "log1p_count"
      d <- dplyr::mutate(d,
        fill_value = switch(encoding,
          log1p_count = log1p(event_count),
          binary      = as.numeric(event_count > 0),
          count       = as.numeric(event_count),
          log1p(event_count)   # fallback
        )
      )

      # Re-sort patient rows (default cluster)
      .reorder_patient_rows(d, sort_by = input$sort_by %||% "cluster")
    })

    # ── 3. Render quilt ──────────────────────────────────────────────────────
    output$quilt_plot <- plotly::renderPlotly({
      shiny::req(quilt_filtered())
      .render_quilt_plotly(quilt_filtered(), selected_id = selected_patient())
    })

    # ── 4. Click → patient selection ────────────────────────────────────────
    shiny::observe({
      click <- plotly::event_data("plotly_click", source = "quilt",
                                   session = session)
      shiny::req(!is.null(click), !is.null(click$customdata))
      pid <- suppressWarnings(as.integer(click$customdata))
      if (!is.na(pid)) selected_patient(pid)
    })

    # ── 5. Selection info bar ────────────────────────────────────────────────
    output$selection_info <- shiny::renderUI({
      pid <- selected_patient()
      if (is.null(pid)) {
        return(shiny::p(
          "Click any cell to select a patient.",
          style = "color: #999; font-style: italic; margin-top: 6px;"
        ))
      }
      row <- dplyr::filter(quilt_base(), subject_id == pid)
      if (nrow(row) == 0L) return(NULL)
      row <- dplyr::slice(row, 1L)
      shiny::div(
        class = "alert alert-info",
        style = "margin-top: 8px; padding: 8px 12px;",
        shiny::tags$b("Selected patient: "), as.character(pid), "  |  ",
        shiny::tags$b("Cluster: "), row$cluster_id, "  |  ",
        shiny::tags$b("Priority: "), row$priority_tier, "  |  ",
        shiny::tags$b("Rank: "), row$rank_position,
        shiny::actionLink(
          ns("view_trajectory"),
          label = shiny::tagList(shiny::icon("chart-line"), " View Trajectory"),
          style = "margin-left: 20px;"
        )
      )
    })

    # ── 6. View trajectory → notify parent via selected_patient signal ────────
    shiny::observeEvent(input$view_trajectory, {
      shiny::req(selected_patient())
      # Parent server.R watches for this and switches tab
      shiny::showNotification(
        paste0("Loading trajectory for patient ", selected_patient()),
        type = "message", duration = 2
      )
    })

    # ── 7. Clear selection ────────────────────────────────────────────────────
    shiny::observeEvent(input$clear_selection, {
      selected_patient(NULL)
    })

    # ── 8. Downloads ──────────────────────────────────────────────────────────
    output$download_png <- shiny::downloadHandler(
      filename = function() paste0("cohort_quilt_", Sys.Date(), ".png"),
      content  = function(file) {
        shiny::req(quilt_filtered())
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
          stop("ggplot2 required for PNG export.")
        }
        p <- .render_quilt_ggplot(quilt_filtered())
        ggplot2::ggsave(file, p, width = 14, height = 9, dpi = 150, device = "png")
      }
    )

    output$download_svg <- shiny::downloadHandler(
      filename = function() paste0("cohort_quilt_", Sys.Date(), ".svg"),
      content  = function(file) {
        shiny::req(quilt_filtered())
        if (!requireNamespace("svglite", quietly = TRUE)) {
          stop("svglite required for SVG export.")
        }
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
          stop("ggplot2 required for SVG export.")
        }
        p <- .render_quilt_ggplot(quilt_filtered())
        svglite::svglite(file, width = 14, height = 9)
        print(p)
        grDevices::dev.off()
      }
    )

    # Return the filtered reactive so parent can observe it if needed
    invisible(quilt_filtered)
  })
}

# ---------------------------------------------------------------------------
# Internal: plotly quilt renderer
# ---------------------------------------------------------------------------

#' Render the interactive plotly quilt (internal)
#' @noRd
.render_quilt_plotly <- function(quilt_df, selected_id = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    rlang::abort("Package 'plotly' is required.")
  }

  domains     <- unique(quilt_df$domain)
  n_domains   <- length(domains)
  if (n_domains == 0L) {
    return(empty_plotly("No data. Adjust filters."))
  }

  # Build one heatmap per domain
  trace_list <- lapply(seq_along(domains), function(i) {
    d_name <- domains[[i]]
    df_d   <- dplyr::filter(quilt_df, domain == d_name)

    win_labels <- df_d |>
      dplyr::distinct(window_idx, window_label) |>
      dplyr::arrange(window_idx) |>
      dplyr::pull(window_label)

    # Pivot to matrix: rows = patients (by patient_row), columns = windows
    mat_wide <- df_d |>
      dplyr::select(patient_row, display_label, subject_id, window_label, fill_value) |>
      tidyr::pivot_wider(
        id_cols     = c(patient_row, display_label, subject_id),
        names_from  = window_label,
        values_from = fill_value,
        values_fill = 0
      ) |>
      dplyr::arrange(patient_row)

    win_labels  <- intersect(win_labels, names(mat_wide))
    z_mat       <- as.matrix(dplyr::select(mat_wide, dplyr::all_of(win_labels)))
    y_labels    <- mat_wide$display_label
    custom_ids  <- mat_wide$subject_id

    cs <- .DOMAIN_COLORSCALES[[d_name]] %||%
      list(c(0,"#FFFFFF"), c(0.5,"#999999"), c(1,"#333333"))

    plotly::plot_ly(
      z           = z_mat,
      x           = win_labels,
      y           = y_labels,
      customdata  = custom_ids,
      type        = "heatmap",
      colorscale  = cs,
      showscale   = (i == 1L),
      colorbar    = list(title = "Activity", len = 0.5, y = 0.5),
      hovertemplate = paste0(
        "<b>Patient:</b> %{customdata}<br>",
        "<b>Window:</b> %{x}<br>",
        "<b>Domain:</b> ", d_name, "<br>",
        "<b>Value:</b> %{z:.3f}",
        "<extra></extra>"
      ),
      source = "quilt"
    )
  })

  # Subplot with shared y-axis
  p <- do.call(plotly::subplot, c(
    trace_list,
    list(
      nrows       = 1L,
      shareY      = TRUE,
      titleX      = TRUE,
      titleY      = FALSE,
      margin      = 0.02
    )
  ))

  p <- plotly::layout(p,
    yaxis   = list(
      autorange = "reversed",
      tickfont  = list(size = 8),
      title     = ""
    ),
    margin  = list(l = 130, r = 60, t = 50, b = 60),
    paper_bgcolor = "#FAFAFA",
    plot_bgcolor  = "#FFFFFF",
    annotations  = lapply(seq_along(domains), function(i) {
      list(
        text      = toupper(domains[[i]]),
        xref      = "paper",
        yref      = "paper",
        x         = (i - 0.5) / n_domains,
        y         = 1.04,
        showarrow = FALSE,
        font      = list(size = 11, color = "#444")
      )
    })
  )

  p <- plotly::event_register(p, "plotly_click")

  # Highlight selected patient row
  if (!is.null(selected_id)) {
    sel_rows <- unique(quilt_df$patient_row[quilt_df$subject_id == selected_id])
    if (length(sel_rows) > 0L) {
      p <- plotly::layout(p, shapes = list(list(
        type      = "rect",
        xref      = "paper", x0 = 0, x1 = 1,
        yref      = "y",
        y0        = sel_rows[[1L]] - 0.5,
        y1        = sel_rows[[1L]] + 0.5,
        line      = list(color = "black", width = 2),
        fillcolor = "rgba(0,0,0,0.08)"
      )))
    }
  }

  p
}
