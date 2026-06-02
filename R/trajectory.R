# trajectory.R
# Per-patient clinical timeline (swim-lane) plots for CohortIntelligence.

DOMAIN_COLORS_TRAJ <- c(
  condition   = "#D73027",
  drug        = "#2166AC",
  procedure   = "#1A9641",
  measurement = "#6A3D9A",
  observation = "#B35806",
  visit       = "#41B6C4",
  death       = "#252525"
)

# Domain display order (clinical grouping: diagnoses -> meds -> procedures -> labs)
DOMAIN_ORDER <- c("condition","drug","procedure","measurement","observation","visit","death")

# ---------------------------------------------------------------------------
# build_patient_timeline
# ---------------------------------------------------------------------------

#' Build a swim-lane timeline tibble for a single patient
#'
#' @param subject_id Integer. The patient to build a timeline for.
#' @param domain_data Named list from [extract_omop_domains()].
#' @param cohort_members tibble(subject_id, cohort_start_date).
#' @param domains Character vector of domains to include.
#' @param top_n Integer. Top N most frequent concepts per domain. Default 10.
#'
#' @return tibble(subject_id, domain, concept_name, concept_name_short,
#'   event_date, value_as_number, days_from_index, domain_rank)
#'   or `NULL` if patient not found.
#' @export
build_patient_timeline <- function(subject_id,
                                    domain_data,
                                    cohort_members,
                                    domains = c("condition","drug","procedure",
                                                "measurement","visit","observation"),
                                    top_n   = 10L) {
  subject_id <- as.integer(subject_id)

  idx_row <- cohort_members[cohort_members$subject_id == subject_id, , drop = FALSE]
  if (nrow(idx_row) == 0L) {
    message("Patient ", subject_id, " not found in cohort_members.")
    return(NULL)
  }
  index_date <- idx_row$cohort_start_date[[1L]]

  domain_spec <- list(
    condition   = list(date_col = "condition_start_date",
                       name_col = "condition_name",     val_col = NULL),
    drug        = list(date_col = "drug_exposure_start_date",
                       name_col = "drug_name",           val_col = NULL),
    procedure   = list(date_col = "procedure_date",
                       name_col = "procedure_name",      val_col = NULL),
    measurement = list(date_col = "measurement_date",
                       name_col = "measurement_name",    val_col = "value_as_number"),
    observation = list(date_col = "observation_date",
                       name_col = "observation_name",    val_col = "value_as_number"),
    visit       = list(date_col = "visit_start_date",
                       name_col = "visit_type",          val_col = NULL)
  )

  active_domains <- intersect(domains, names(domain_spec))

  rows <- purrr::map_dfr(active_domains, function(d) {
    spec <- domain_spec[[d]]
    df   <- domain_data[[d]]
    if (is.null(df) || nrow(df) == 0L) return(tibble::tibble())
    df <- df[df$person_id == subject_id, , drop = FALSE]
    if (nrow(df) == 0L) return(tibble::tibble())
    if (!spec$date_col %in% names(df)) return(tibble::tibble())
    if (!spec$name_col %in% names(df)) df[[spec$name_col]] <- paste0(d, "_unknown")

    # Remove null / "No matching concept" rows
    valid_name <- !is.na(df[[spec$name_col]]) &
                  df[[spec$name_col]] != "No matching concept" &
                  nzchar(df[[spec$name_col]])
    df <- df[valid_name, , drop = FALSE]
    if (nrow(df) == 0L) return(tibble::tibble())

    # Keep top_n most frequent concepts
    if (is.finite(top_n)) {
      top_concepts <- df |>
        dplyr::count(.data[[spec$name_col]], sort = TRUE) |>
        dplyr::slice_head(n = as.integer(top_n)) |>
        dplyr::pull(spec$name_col)
      df <- df[df[[spec$name_col]] %in% top_concepts, , drop = FALSE]
    }

    full_name <- as.character(df[[spec$name_col]])

    tibble::tibble(
      subject_id      = subject_id,
      domain          = d,
      concept_name    = full_name,                         # full, for tooltip
      concept_name_short = .truncate_label(full_name, 45), # truncated, for y-axis
      event_date      = df[[spec$date_col]],
      value_as_number = if (!is.null(spec$val_col) && spec$val_col %in% names(df))
                          as.numeric(df[[spec$val_col]])
                        else NA_real_,
      days_from_index = as.integer(df[[spec$date_col]] - index_date),
      domain_rank     = match(d, DOMAIN_ORDER) %||% 99L
    )
  })

  if (is.null(rows) || nrow(rows) == 0L) return(NULL)

  # Sort by domain order, then alphabetically within domain for consistent layout
  rows |>
    dplyr::arrange(domain_rank, concept_name_short, days_from_index)
}

# Truncate a character vector to max_chars, appending "..." if needed
.truncate_label <- function(x, max_chars = 45L) {
  long <- nchar(x) > max_chars
  out  <- x
  out[long] <- paste0(substr(x[long], 1L, max_chars - 3L), "...")
  out
}

# ---------------------------------------------------------------------------
# plot_patient_timeline
# ---------------------------------------------------------------------------

#' Plot a human-readable patient clinical timeline
#'
#' Concepts are grouped by clinical domain and sorted consistently.
#' Long concept names are truncated on the y-axis but shown in full on hover.
#' The left margin adapts to the longest visible label. The plot height
#' scales with the number of distinct concept rows.
#'
#' @param timeline_df tibble from [build_patient_timeline()].
#' @param interactive Logical. `TRUE` = plotly; `FALSE` = ggplot2.
#' @param show_index Logical. Dashed vertical line at day 0.
#' @param date_range Optional `c(days_start, days_end)`.
#'
#' @return A plotly widget or ggplot2 object.
#' @export
plot_patient_timeline <- function(timeline_df,
                                   interactive = TRUE,
                                   color_by    = c("domain","concept"),
                                   show_index  = TRUE,
                                   date_range  = NULL) {
  color_by <- match.arg(color_by)

  if (is.null(timeline_df) || nrow(timeline_df) == 0L) {
    if (interactive && requireNamespace("plotly", quietly = TRUE)) {
      return(plotly::plot_ly(type = "scatter", mode = "markers") |>
        plotly::layout(
          annotations = list(list(
            text = "No clinical data available for this patient.",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 14, color = "#666")
          )),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        ) |>
        plotly::config(displayModeBar = FALSE))
    }
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0, y = 0,
                                label = "No data for this patient.") +
             ggplot2::theme_void())
  }

  df <- timeline_df
  if (!is.null(date_range) && length(date_range) == 2L) {
    df <- df[df$days_from_index >= date_range[[1]] &
               df$days_from_index <= date_range[[2]], , drop = FALSE]
    if (nrow(df) == 0L) {
      df <- timeline_df  # fall back to all data if filter removes everything
    }
  }

  if (!interactive) return(.plot_timeline_ggplot(df, show_index))

  if (!requireNamespace("plotly", quietly = TRUE)) {
    rlang::abort("Package 'plotly' is required for interactive trajectory plots.")
  }

  # ── Build ordered y-axis: group by domain, sorted within each group ──────
  # concept_name_short determines the y-axis label; concept_name is for tooltip
  y_order <- df |>
    dplyr::distinct(domain, concept_name_short, domain_rank) |>
    dplyr::arrange(domain_rank, concept_name_short) |>
    dplyr::pull(concept_name_short)

  # Adaptive left margin: base on longest label (~7px per character)
  max_label_chars <- max(nchar(y_order), na.rm = TRUE)
  left_margin     <- min(max(max_label_chars * 7L, 160L), 380L)

  # Adaptive plot height
  n_rows  <- length(unique(y_order))
  p_height <- max(320L, n_rows * 28L + 80L)

  pid <- unique(df$subject_id)[[1L]]

  # Domain group separator lines (horizontal rules between domain groups)
  domain_groups <- df |>
    dplyr::distinct(domain, concept_name_short, domain_rank) |>
    dplyr::arrange(domain_rank, concept_name_short) |>
    dplyr::mutate(row_idx = dplyr::row_number())

  separator_shapes <- list()
  prev_domain <- ""
  for (i in seq_len(nrow(domain_groups))) {
    d <- domain_groups$domain[i]
    if (d != prev_domain && prev_domain != "") {
      separator_shapes[[length(separator_shapes) + 1L]] <- list(
        type = "line",
        x0   = 0, x1 = 1, xref = "paper",
        y0   = domain_groups$concept_name_short[i],
        y1   = domain_groups$concept_name_short[i],
        yref = "y",
        line = list(color = "#e2e8f0", width = 1, dash = "solid")
      )
    }
    prev_domain <- d
  }

  # Index-date vertical line
  shapes <- separator_shapes
  if (show_index) {
    shapes[[length(shapes) + 1L]] <- list(
      type  = "line",
      x0 = 0, x1 = 0,
      y0 = 0, y1 = 1, yref = "paper",
      line  = list(color = "#1a1a2e", dash = "dash", width = 2)
    )
  }

  # Hover text: full concept name + date + value
  hover_text <- paste0(
    "<b>", df$concept_name, "</b><br>",
    "Date: ", format(df$event_date, "%d %b %Y"),
    " (day ", df$days_from_index, ")",
    dplyr::if_else(
      !is.na(df$value_as_number),
      paste0("<br>Value: ", round(df$value_as_number, 2)),
      ""
    )
  )

  p <- plotly::plot_ly(
    data         = df,
    x            = ~days_from_index,
    y            = ~concept_name_short,
    color        = ~domain,
    colors       = DOMAIN_COLORS_TRAJ,
    type         = "scatter",
    mode         = "markers",
    marker       = list(size = 10, opacity = 0.85,
                         line = list(color = "white", width = 1)),
    text         = hover_text,
    hoverinfo    = "text",
    customdata   = ~concept_name,  # full name available in customdata
    source       = "trajectory"
  ) |>
    plotly::layout(
      title  = list(
        text = paste0("Patient ", pid, " -- Clinical Timeline"),
        font = list(size = 14, color = "#0f3460"),
        x    = 0
      ),
      xaxis  = list(
        title       = "Days from cohort index date  (0 = index)",
        zeroline    = TRUE,
        zerolinecolor = "#94a3b8",
        zerolinewidth = 1,
        tickmode    = "auto",
        nticks      = 10,
        showgrid    = TRUE,
        gridcolor   = "#f1f5f9"
      ),
      yaxis  = list(
        title        = "",
        categoryorder = "array",
        categoryarray = rev(y_order),  # top = first domain group
        automargin   = TRUE,
        tickfont     = list(size = 11),
        showgrid     = FALSE
      ),
      shapes = shapes,
      legend = list(
        title       = list(text = "<b>Domain</b>"),
        orientation = "v",
        x           = 1.01,
        xanchor     = "left",
        y           = 1,
        yanchor     = "top",
        bgcolor     = "rgba(255,255,255,0.9)",
        bordercolor = "#e2e8f0",
        borderwidth = 1
      ),
      margin = list(l = left_margin, r = 120, t = 50, b = 60),
      height = p_height,
      paper_bgcolor = "#FAFAFA",
      plot_bgcolor  = "#FFFFFF",
      annotations  = list(list(
        x         = 0,
        y         = -0.08,
        xref      = "x",
        yref      = "paper",
        text      = "Index date",
        showarrow = FALSE,
        font      = list(size = 10, color = "#475569"),
        xanchor   = "center"
      ))
    ) |>
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d","pan2d","select2d","lasso2d",
        "zoomIn2d","zoomOut2d","autoScale2d",
        "hoverClosestCartesian","hoverCompareCartesian",
        "toggleSpikelines"
      ),
      displaylogo = FALSE,
      toImageButtonOptions = list(
        format   = "png",
        filename = paste0("trajectory_patient_", pid),
        width    = 1200,
        height   = p_height
      )
    )

  p
}

# ── Static ggplot2 version (for export) ──────────────────────────────────

.plot_timeline_ggplot <- function(df, show_index = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package 'ggplot2' is required.")
  }

  pid    <- unique(df$subject_id)[[1L]]
  y_order <- df |>
    dplyr::distinct(domain, concept_name_short, domain_rank) |>
    dplyr::arrange(domain_rank, concept_name_short) |>
    dplyr::pull(concept_name_short)

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x     = days_from_index,
    y     = factor(concept_name_short, levels = rev(y_order)),
    color = domain
  )) +
    ggplot2::geom_point(size = 3, alpha = 0.85) +
    ggplot2::scale_color_manual(values = DOMAIN_COLORS_TRAJ, name = "Domain") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.y      = ggplot2::element_text(size = 9),
      panel.grid.major.x = ggplot2::element_line(colour = "#f1f5f9"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position  = "right"
    ) +
    ggplot2::labs(
      x     = "Days from cohort index date  (0 = index)",
      y     = NULL,
      title = paste0("Patient ", pid, " -- Clinical Timeline")
    )

  if (show_index) {
    p <- p +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                           colour = "#1a1a2e", linewidth = 0.7) +
      ggplot2::annotate("text", x = 0, y = Inf, label = "Index",
                         hjust = -0.2, vjust = 1.5,
                         size = 3, colour = "#475569")
  }
  p
}
