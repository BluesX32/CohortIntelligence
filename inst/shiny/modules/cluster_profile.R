# cluster_profile.R
# Shiny module: clinical characterisation of each ML cluster.
# Shows one summary card per cluster + a side-by-side prevalence bar chart.

# Palette that matches the UMAP scatter (plotly default colour cycle)
.CLUSTER_COLORS <- c(
  "#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A",
  "#19D3F3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52"
)

#' Cluster profile module UI
#' @param id Shiny module namespace ID.
#' @export
cluster_profileUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # ── Domain picker for comparison chart ───────────────────────────────
    shiny::fluidRow(
      shiny::column(4,
        shinyWidgets::pickerInput(
          ns("compare_domain"),
          label    = "Compare domain across clusters",
          choices  = c("condition", "drug", "procedure"),
          selected = "condition"
        )
      ),
      shiny::column(8,
        shiny::p(
          style = "color:#666; font-size:0.88em; margin-top:28px;",
          "Prevalence = % of patients in that cluster with",
          " ≥1 recorded event for that concept."
        )
      )
    ),
    # ── Cluster cards ─────────────────────────────────────────────────────
    shiny::uiOutput(ns("cluster_cards")),
    # ── Comparison bar chart ──────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(12,
        shiny::h4("Top Concepts by Cluster — Side-by-Side Comparison"),
        plotly::plotlyOutput(ns("compare_chart"), height = "500px")
      )
    )
  )
}

#' Cluster profile module server
#' @param id Module ID.
#' @param rank_df `reactive` returning tibble from [rank_patients()].
#' @param domain_data `reactive` returning named list of domain tibbles.
#' @param cohort_members `reactive` returning cohort member tibble.
#' @param person_data `reactive` returning demographics tibble.
#' @export
cluster_profileServer <- function(id, rank_df, domain_data,
                                   cohort_members, person_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # Pre-compute profiles when data is available
    profiles <- shiny::reactive({
      shiny::req(rank_df(), domain_data(), cohort_members())
      build_cluster_profiles(
        rank_df       = rank_df(),
        domain_data   = domain_data(),
        cohort_members = cohort_members(),
        person_data   = person_data(),
        top_n         = 10L,
        domains       = c("condition", "drug", "procedure",
                          "measurement", "observation")
      )
    })

    # ── Cluster summary cards ─────────────────────────────────────────────
    output$cluster_cards <- shiny::renderUI({
      p <- profiles()
      s <- p$summary
      if (nrow(s) == 0L) {
        return(shiny::div(
          class = "alert alert-info",
          shiny::icon("circle-info"),
          " Cluster data not available. ML pipeline may be unavailable."
        ))
      }

      cards <- lapply(seq_len(nrow(s)), function(i) {
        row      <- s[i, ]
        cid      <- row$cluster_id
        col      <- .CLUSTER_COLORS[[(i - 1L) %% length(.CLUSTER_COLORS) + 1L]]
        concepts <- p$concepts[p$concepts$cluster_id == cid, , drop = FALSE]

        # Top 5 concepts per domain for the card
        top_cond <- dplyr::filter(concepts, domain == "condition") |>
          dplyr::arrange(dplyr::desc(prevalence)) |>
          dplyr::slice_head(n = 5L)
        top_drug <- dplyr::filter(concepts, domain == "drug") |>
          dplyr::arrange(dplyr::desc(prevalence)) |>
          dplyr::slice_head(n = 5L)

        make_concept_list <- function(df) {
          if (nrow(df) == 0L) return(shiny::tags$em("—", style = "color:#999"))
          shiny::tags$ol(
            style = "padding-left:16px; margin:0;",
            lapply(seq_len(nrow(df)), function(j) {
              shiny::tags$li(
                style = "font-size:0.82em; margin-bottom:2px;",
                df$concept_name[[j]], " ",
                shiny::tags$span(
                  style = "color:#888;",
                  paste0(df$prevalence[[j]], "%")
                )
              )
            })
          )
        }

        # Format a stat value safely
        fmt_stat <- function(x, fmt = "%.0f", suffix = "") {
          if (is.null(x) || is.na(x)) return("N/A")
          paste0(sprintf(fmt, x), suffix)
        }

        shinydashboard::box(
          width  = 6,
          status = NULL,
          style  = paste0("border-top: 4px solid ", col, ";"),
          title  = shiny::div(
            shiny::tags$span(
              style = paste0("color:", col, "; font-weight:700;"),
              if (cid <= 0L) "Unassigned" else paste0("Cluster ", cid)
            ),
            shiny::tags$span(
              style = "color:#666; font-size:0.85em; margin-left:8px;",
              sprintf("n = %d  (%.1f%% of cohort)",
                      row$n_patients, row$pct_cohort)
            )
          ),
          shiny::fluidRow(
            shiny::column(5,
              shiny::tags$b("Top Conditions"),
              make_concept_list(top_cond)
            ),
            shiny::column(4,
              shiny::tags$b("Top Drugs"),
              make_concept_list(top_drug)
            ),
            shiny::column(3,
              shiny::tags$b("Summary"),
              shiny::tags$table(
                style = "font-size:0.82em; width:100%;",
                shiny::tags$tr(
                  shiny::tags$td(style = "color:#666;", "Median age"),
                  shiny::tags$td(fmt_stat(row$median_age, "%.0f", " yr"))
                ),
                shiny::tags$tr(
                  shiny::tags$td(style = "color:#666;", "Female"),
                  shiny::tags$td(fmt_stat(row$pct_female, "%.0f", "%"))
                ),
                shiny::tags$tr(
                  shiny::tags$td(style = "color:#666;", "Median F/U"),
                  shiny::tags$td(
                    fmt_stat(
                      if (!is.na(row$median_followup_days))
                        row$median_followup_days / 30.4375
                      else NA_real_,
                      "%.1f", " mo"
                    )
                  )
                ),
                shiny::tags$tr(
                  shiny::tags$td(style = "color:#666;", "Mortality"),
                  shiny::tags$td(fmt_stat(row$pct_death, "%.0f", "%"))
                )
              )
            )
          )
        )
      })

      # Pair cards into rows of 2
      pairs <- split(cards, ceiling(seq_along(cards) / 2))
      shiny::tagList(lapply(pairs, function(pair) {
        do.call(shiny::fluidRow, pair)
      }))
    })

    # ── Side-by-side comparison chart ────────────────────────────────────
    output$compare_chart <- plotly::renderPlotly({
      p       <- profiles()
      d_name  <- input$compare_domain
      shiny::req(nrow(p$summary) > 0L, nrow(p$concepts) > 0L)

      df <- dplyr::filter(p$concepts, domain == d_name)
      if (nrow(df) == 0L) {
        return(empty_plotly(paste0("No ", d_name, " data found.")))
      }

      # Take the top 10 concepts by max prevalence across all clusters
      top_concepts <- df |>
        dplyr::group_by(concept_name) |>
        dplyr::summarise(max_prev = max(prevalence), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(max_prev)) |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(concept_name)

      df_plot <- dplyr::filter(df, concept_name %in% top_concepts) |>
        dplyr::mutate(
          cluster_label = ifelse(cluster_id <= 0L, "Unassigned",
                                  paste0("Cluster ", cluster_id))
        )

      clusters    <- sort(unique(df_plot$cluster_id))
      traces      <- lapply(seq_along(clusters), function(i) {
        cid   <- clusters[[i]]
        label <- if (cid <= 0L) "Unassigned" else paste0("Cluster ", cid)
        col   <- .CLUSTER_COLORS[[(i - 1L) %% length(.CLUSTER_COLORS) + 1L]]
        sub   <- dplyr::filter(df_plot, cluster_id == cid)
        # Ensure all top_concepts appear, even if 0%
        full  <- tibble::tibble(concept_name = top_concepts) |>
          dplyr::left_join(dplyr::select(sub, concept_name, prevalence),
                           by = "concept_name") |>
          dplyr::mutate(prevalence = dplyr::coalesce(prevalence, 0))

        plotly::plot_ly(
          data        = full,
          x           = ~prevalence,
          y           = ~concept_name,
          type        = "bar",
          orientation = "h",
          name        = label,
          marker      = list(color = col, opacity = 0.8),
          hovertemplate = paste0(
            "<b>%{y}</b><br>",
            "Cluster: ", label, "<br>",
            "Prevalence: %{x:.1f}%<extra></extra>"
          )
        )
      })

      p_chart <- do.call(plotly::subplot, c(
        traces, list(nrows = 1L, shareY = TRUE, titleX = TRUE)
      ))

      plotly::layout(p_chart,
        yaxis  = list(
          categoryorder = "total ascending",
          tickfont      = list(size = 11)
        ),
        xaxis  = list(title = "Prevalence (%)"),
        legend = list(orientation = "h", x = 0, y = -0.15),
        margin = list(l = 250, r = 20, t = 30, b = 60),
        paper_bgcolor = "#FAFAFA"
      )
    })
  })
}
