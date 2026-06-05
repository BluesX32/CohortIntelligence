# outcome_explorer.R
# Shiny module: exposure/outcome temporal distribution and outcome-stratified
# clustering.  Lets the user pick any OMOP concept as an "outcome" (or
# "exposure"), see when it occurs relative to the cohort index date, overlay
# outcome status on the existing UMAP, and optionally re-cluster patients
# using only that domain's features.

# Cluster colour palette (same as cluster_profile.R)
.OC_CLUSTER_COLORS <- c(
  "#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A",
  "#19D3F3", "#FF6692", "#B6E880", "#FF97FF", "#FECB52"
)

# Domain choices for pickers (displayed label → value)
.OC_DOMAIN_CHOICES <- c(
  "Condition"   = "condition",
  "Drug"        = "drug",
  "Procedure"   = "procedure",
  "Measurement" = "measurement",
  "Observation" = "observation",
  "Visit"       = "visit",
  "Death"       = "death"
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

#' Outcome Explorer module UI
#' @param id Shiny module namespace ID.
#' @export
outcome_explorerUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    # ── Row 1: Controls ───────────────────────────────────────────────────
    shinydashboard::box(
      width = 12, collapsible = FALSE,
      title = shiny::div(
        shiny::tags$b("Select outcome / event of interest"),
        shiny::tags$span(
          style = "color:#64748b; font-size:0.82em; margin-left:10px; font-weight:400;",
          "Choose a concept to trace across the cohort timeline"
        )
      ),

      shiny::fluidRow(
        # Primary domain + concept picker
        shiny::column(2,
          shinyWidgets::pickerInput(
            ns("domain"),
            label    = "Domain",
            choices  = .OC_DOMAIN_CHOICES,
            selected = "condition",
            options  = shinyWidgets::pickerOptions(size = 8)
          )
        ),
        shiny::column(4,
          shinyWidgets::pickerInput(
            ns("concept_id"),
            label   = "Concept  (search by name)",
            choices = c("— load data first —" = ""),
            options = shinyWidgets::pickerOptions(
              liveSearch          = TRUE,
              liveSearchPlaceholder = "Type to search…",
              size                = 10,
              noneSelectedText    = "Select a concept"
            )
          )
        ),
        # Day range + bin width
        shiny::column(3,
          shiny::sliderInput(ns("day_range"), "Day range (from index)",
                             min = -1095L, max = 1095L,
                             value = c(-365L, 730L), step = 30L)
        ),
        shiny::column(2,
          shiny::sliderInput(ns("bin_width"), "Bin width (days)",
                             min = 7L, max = 180L, value = 30L, step = 7L)
        ),
        # Exposure comparison toggle
        shiny::column(1,
          shiny::div(
            style = "margin-top:28px;",
            shiny::checkboxInput(ns("show_exposure"),
                                  shiny::tags$span(
                                    style = "font-size:0.82em;",
                                    "Compare exposure → outcome"
                                  ),
                                  value = FALSE)
          )
        )
      )
    ),

    # ── Row 2: Event distribution + outcome summary ───────────────────────
    shiny::fluidRow(
      shinydashboard::box(
        width = 8, title = "Event Distribution — Days From Index",
        shiny::div(
          style = "color:#64748b; font-size:0.82em; margin-bottom:6px;",
          "Histogram: how many patients had this event in each 30-day window.",
          " Red dashed line = index date (day 0)."
        ),
        plotly::plotlyOutput(ns("event_dist_plot"), height = "320px")
      ),
      shinydashboard::box(
        width = 4, title = "Outcome Summary",
        shiny::uiOutput(ns("outcome_summary_ui"))
      )
    ),

    # ── Row 3: UMAP views ─────────────────────────────────────────────────
    shiny::fluidRow(
      shinydashboard::box(
        width = 6,
        title = "Existing UMAP — Coloured by Outcome",
        shiny::div(
          style = "color:#64748b; font-size:0.82em; margin-bottom:6px;",
          "Uses the cohort-level UMAP from the Cluster & Anomaly tab."
        ),
        shiny::fluidRow(
          shiny::column(12,
            shiny::radioButtons(
              ns("umap_color_by"), label = NULL,
              choices  = c("Has outcome (Yes/No)" = "has_outcome",
                           "Days to outcome (binned)"  = "days_bin"),
              selected = "has_outcome", inline = TRUE
            )
          )
        ),
        plotly::plotlyOutput(ns("outcome_umap_plot"), height = "360px")
      ),
      shinydashboard::box(
        width = 6,
        title = "Outcome-Specific Clustering",
        shiny::div(
          style = "color:#64748b; font-size:0.82em; margin-bottom:6px;",
          "Re-runs UMAP + clustering using only the selected domain's features.",
          " Clusters here reflect similarity in this domain only."
        ),
        shiny::fluidRow(
          shiny::column(6,
            shiny::numericInput(ns("min_freq_outcome"),
                                "Min concept frequency",
                                value = 2L, min = 1L, max = 20L, step = 1L)
          ),
          shiny::column(6,
            shiny::div(
              style = "margin-top:24px;",
              shiny::actionButton(ns("btn_outcome_cluster"),
                                   "Cluster by this outcome",
                                   icon  = shiny::icon("circle-nodes"),
                                   class = "btn-primary btn-sm btn-block")
            )
          )
        ),
        plotly::plotlyOutput(ns("outcome_specific_umap"), height = "300px")
      )
    ),

    # ── Row 4: Exposure → Outcome (conditional) ───────────────────────────
    shiny::conditionalPanel(
      condition = paste0("input['", ns("show_exposure"), "'] == true"),
      shinydashboard::box(
        width = 12,
        title = "Exposure → Outcome Timing",
        shiny::div(
          class = "alert alert-info",
          style = "font-size:0.82em; padding:8px 12px; margin-bottom:12px;",
          shiny::icon("circle-info"),
          " Shows time from first exposure event to first outcome event.",
          " Only patients with BOTH events in the selected window are included."
        ),
        shiny::fluidRow(
          shiny::column(2,
            shinyWidgets::pickerInput(
              ns("exposure_domain"),
              label    = "Exposure domain",
              choices  = .OC_DOMAIN_CHOICES,
              selected = "drug",
              options  = shinyWidgets::pickerOptions(size = 8)
            )
          ),
          shiny::column(4,
            shinyWidgets::pickerInput(
              ns("exposure_concept_id"),
              label   = "Exposure concept",
              choices = c("— load data first —" = ""),
              options = shinyWidgets::pickerOptions(
                liveSearch = TRUE,
                liveSearchPlaceholder = "Type to search…",
                size = 10
              )
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(6,
            plotly::plotlyOutput(ns("exp_outcome_violin"), height = "320px")
          ),
          shiny::column(6,
            plotly::plotlyOutput(ns("exp_outcome_scatter"), height = "320px")
          )
        )
      )
    )

  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

#' Outcome Explorer module server
#' @param id Module ID.
#' @param domain_data `reactive` returning named list from
#'   [extract_omop_domains()], or `NULL`.
#' @param cohort_members `reactive` returning tibble from
#'   [extract_cohort_members()], or `NULL`.
#' @param ml_results `reactive` returning list from
#'   [run_full_ml_pipeline()], or `NULL`.
#' @param feature_matrix `reactive` returning list from
#'   [build_feature_matrix()], or `NULL`.
#' @export
outcome_explorerServer <- function(id, domain_data, cohort_members,
                                    ml_results, feature_matrix) {
  shiny::moduleServer(id, function(input, output, session) {

    # Module-local ephemeral ML result (not pushed to shared rv)
    outcome_ml_rv <- shiny::reactiveVal(NULL)

    # ── Populate outcome concept picker ──────────────────────────────────
    shiny::observe({
      shiny::req(domain_data(), input$domain)
      .update_concept_picker(session, domain_data(), input$domain,
                              input_id = "concept_id")
    })

    # ── Populate exposure concept picker ─────────────────────────────────
    shiny::observe({
      shiny::req(domain_data(), input$exposure_domain, input$show_exposure)
      .update_concept_picker(session, domain_data(), input$exposure_domain,
                              input_id = "exposure_concept_id")
    })

    # ── Outcome labels reactive ───────────────────────────────────────────
    outcome_labels_rv <- shiny::reactive({
      shiny::req(domain_data(), cohort_members(),
                 input$concept_id, nzchar(input$concept_id))
      tryCatch(
        compute_outcome_labels(
          cohort_members  = cohort_members(),
          domain_data     = domain_data(),
          domain          = input$domain,
          concept_id      = as.integer(input$concept_id),
          post_index_only = TRUE,
          day_range       = c(0L, as.integer(input$day_range[[2L]]))
        ),
        error = function(e) NULL
      )
    })

    # ── Exposure labels reactive ──────────────────────────────────────────
    exposure_labels_rv <- shiny::reactive({
      shiny::req(input$show_exposure, domain_data(), cohort_members(),
                 input$exposure_concept_id, nzchar(input$exposure_concept_id))
      tryCatch(
        compute_outcome_labels(
          cohort_members  = cohort_members(),
          domain_data     = domain_data(),
          domain          = input$exposure_domain,
          concept_id      = as.integer(input$exposure_concept_id),
          post_index_only = FALSE,
          day_range       = input$day_range
        ),
        error = function(e) NULL
      )
    })

    # ── 1. Event distribution plot ────────────────────────────────────────
    output$event_dist_plot <- plotly::renderPlotly({
      shiny::req(domain_data(), cohort_members(),
                 input$concept_id, nzchar(input$concept_id))

      dist_df <- tryCatch(
        compute_event_distribution(
          cohort_members = cohort_members(),
          domain_data    = domain_data(),
          domain         = input$domain,
          concept_id     = as.integer(input$concept_id),
          day_range      = as.integer(input$day_range),
          bin_width      = as.integer(input$bin_width)
        ),
        error = function(e) NULL
      )

      if (is.null(dist_df) || sum(dist_df$n_patients) == 0L) {
        return(empty_plotly("No events found for this concept in the selected window."))
      }

      # Hover text
      dist_df <- dplyr::mutate(dist_df,
        hover = paste0(
          "Window: ", bin_start, " to ", bin_end, " days<br>",
          "Patients: ", n_patients, "<br>",
          "Events: ", n_events
        )
      )

      plotly::plot_ly(dist_df,
        x             = ~bin_mid,
        y             = ~n_patients,
        type          = "bar",
        marker        = list(color = "rgba(44,123,182,0.75)",
                              line  = list(color = "#fff", width = 0.5)),
        text          = ~hover,
        hoverinfo     = "text"
      ) |>
        plotly::layout(
          xaxis  = list(
            title      = "Days from index date",
            zeroline   = FALSE,
            tickformat = "d"
          ),
          yaxis  = list(title = "Patients with event"),
          shapes = list(list(
            type = "line", x0 = 0, x1 = 0,
            y0 = 0, y1 = 1, yref = "paper",
            line = list(color = "#dc2626", dash = "dash", width = 2)
          )),
          annotations = list(list(
            x = 2, y = 1, xref = "x", yref = "paper",
            text = "Index date", showarrow = FALSE,
            font = list(size = 10, color = "#dc2626"),
            xanchor = "left", yanchor = "top"
          )),
          bargap        = 0.05,
          paper_bgcolor = "#FAFAFA",
          plot_bgcolor  = "#FAFAFA"
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── 2. Outcome summary value boxes ────────────────────────────────────
    output$outcome_summary_ui <- shiny::renderUI({
      oc  <- outcome_labels_rv()
      cm  <- cohort_members()
      if (is.null(oc) || is.null(cm)) {
        return(shiny::div(
          class = "alert alert-info",
          style = "font-size:0.85em;",
          shiny::icon("circle-info"),
          " Select a concept to see outcome stats."
        ))
      }

      n_total    <- nrow(cm)
      n_outcome  <- sum(oc$has_outcome, na.rm = TRUE)
      pct        <- if (n_total > 0L) round(100 * n_outcome / n_total, 1) else 0
      med_days   <- stats::median(
        oc$days_to_first_event[!is.na(oc$days_to_first_event)],
        na.rm = TRUE
      )
      med_txt <- if (!is.na(med_days)) paste0(round(med_days), " days") else "N/A"

      # Cluster with highest outcome rate (if ml_results available)
      cluster_txt <- "N/A"
      ml <- ml_results()
      if (!is.null(ml) && !is.null(ml$merged) && nrow(ml$merged) > 0L) {
        merged <- dplyr::left_join(ml$merged, oc, by = "subject_id")
        cluster_rates <- merged |>
          dplyr::filter(!is.na(cluster_id), cluster_id > 0L) |>
          dplyr::group_by(cluster_id) |>
          dplyr::summarise(rate = mean(has_outcome, na.rm = TRUE),
                            .groups = "drop") |>
          dplyr::arrange(dplyr::desc(rate))
        if (nrow(cluster_rates) > 0L) {
          cluster_txt <- paste0(
            "Cluster ", cluster_rates$cluster_id[[1L]],
            " (", round(100 * cluster_rates$rate[[1L]], 0), "%)"
          )
        }
      }

      shiny::tagList(
        .oc_stat_row("Patients with outcome", paste0(n_outcome, " / ", n_total,
                                                       " (", pct, "%)")),
        shiny::hr(style = "margin: 6px 0;"),
        .oc_stat_row("Median days to event", med_txt),
        shiny::hr(style = "margin: 6px 0;"),
        .oc_stat_row("Highest-rate cluster", cluster_txt),
        shiny::hr(style = "margin: 6px 0;"),
        shiny::div(
          style = "margin-top:10px; font-size:0.78em; color:#64748b;",
          "Post-index window only (day 0 to ", input$day_range[[2L]], " days)."
        )
      )
    })

    # ── 3. Outcome-stratified UMAP ────────────────────────────────────────
    output$outcome_umap_plot <- plotly::renderPlotly({
      ml <- ml_results()
      oc <- outcome_labels_rv()

      if (is.null(ml) || is.null(ml$merged) || nrow(ml$merged) == 0L) {
        return(empty_plotly(
          "ML pipeline unavailable.\nInstall uwot: install.packages('uwot')"
        ))
      }
      if (is.null(oc)) {
        return(empty_plotly("Select a concept to overlay outcome on UMAP."))
      }

      df <- dplyr::left_join(ml$merged, oc, by = "subject_id")

      color_by <- input$umap_color_by %||% "has_outcome"
      if (color_by == "has_outcome") {
        df$color_label <- ifelse(df$has_outcome, "Outcome ✓", "No outcome")
        color_palette  <- c("Outcome ✓" = "#dc2626",
                             "No outcome"    = "#94a3b8")
        legend_title   <- "<b>Outcome</b>"
      } else {
        df$color_label <- as.character(df$days_bin)
        color_palette  <- c("0-90d"     = "#dc2626",
                             "91-180d"  = "#f97316",
                             "181-365d" = "#eab308",
                             ">365d"    = "#22c55e",
                             "None"     = "#94a3b8")
        legend_title   <- "<b>Days to event</b>"
      }

      plotly::plot_ly(
        data      = df,
        x         = ~umap_1,
        y         = ~umap_2,
        color     = ~color_label,
        colors    = color_palette,
        type      = "scatter",
        mode      = "markers",
        marker    = list(size = 7, opacity = 0.85,
                          line = list(width = 0.4,
                                      color = "rgba(255,255,255,0.5)")),
        text      = ~paste0(
          "<b>Patient:</b> ", subject_id,
          "<br><b>Cluster:</b> ", cluster_label,
          "<br><b>Outcome:</b> ",
          ifelse(has_outcome,
                 paste0("Yes — day ", days_to_first_event),
                 "No")
        ),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          xaxis  = list(title = "UMAP 1", zeroline = FALSE),
          yaxis  = list(title = "UMAP 2", zeroline = FALSE),
          legend = list(title         = list(text = legend_title),
                         tracegroupgap = 0,
                         itemsizing    = "constant"),
          paper_bgcolor = "#FAFAFA",
          plot_bgcolor  = "#F0F2F5"
        )
    })

    # ── 4. Outcome-specific re-clustering ─────────────────────────────────
    shiny::observeEvent(input$btn_outcome_cluster, {
      shiny::req(domain_data(), cohort_members(), feature_matrix(),
                 input$concept_id, nzchar(input$concept_id))

      shiny::withProgress(
        message = paste0("Clustering by ", input$domain, " features..."),
        value   = 0, {

          shiny::setProgress(0.2, detail = "Building domain feature matrix...")
          fm <- tryCatch(
            build_feature_matrix(
              cohort_members   = cohort_members(),
              domain_data      = domain_data(),
              time_windows     = feature_matrix()$windows,
              domains          = input$domain,
              value_mode       = "binary",
              min_concept_freq = as.integer(input$min_freq_outcome %||% 2L)
            ),
            error = function(e) {
              shiny::showNotification(
                paste0("Feature matrix error: ", conditionMessage(e)),
                type = "error", duration = 8L
              )
              NULL
            }
          )

          if (is.null(fm) || ncol(fm$wide) <= 1L) {
            shiny::showNotification(
              paste0("Not enough features in the '", input$domain,
                     "' domain to cluster. ",
                     "Try lowering Min concept frequency or selecting a different domain."),
              type = "warning", duration = 10L
            )
            shiny::setProgress(1.0)
            return()
          }

          shiny::setProgress(0.6, detail = "Running UMAP + clustering...")
          ml <- tryCatch(
            run_full_ml_pipeline(fm$wide),
            error = function(e) {
              shiny::showNotification(
                paste0("ML error: ", conditionMessage(e),
                       " — try installing uwot: install.packages('uwot')"),
                type = "warning", duration = 10L
              )
              NULL
            }
          )

          outcome_ml_rv(ml)
          shiny::setProgress(1.0)

          if (!is.null(ml)) {
            shiny::showNotification(
              paste0("Outcome clustering complete — ",
                     length(unique(ml$clusters$cluster_id[
                       ml$clusters$cluster_id > 0L])),
                     " clusters found in '", input$domain, "' features."),
              type = "message", duration = 5L
            )
          }
        }
      )
    })

    output$outcome_specific_umap <- plotly::renderPlotly({
      ml <- outcome_ml_rv()

      if (is.null(ml)) {
        return(empty_plotly(
          paste0('Click "Cluster by this outcome" to cluster patients',
                 '\nby the selected domain features only.')
        ))
      }
      if (is.null(ml$merged) || nrow(ml$merged) == 0L) {
        return(empty_plotly("Clustering did not produce results."))
      }

      df <- ml$merged
      df$cluster_label <- ifelse(df$cluster_id <= 0L, "Unassigned",
                                  paste0("Cluster ", df$cluster_id))

      plotly::plot_ly(
        data      = df,
        x         = ~umap_1,
        y         = ~umap_2,
        color     = ~cluster_label,
        type      = "scatter",
        mode      = "markers",
        marker    = list(size = 7, opacity = 0.85,
                          line = list(width = 0.4,
                                      color = "rgba(255,255,255,0.5)")),
        text      = ~paste0(
          "<b>Patient:</b> ", subject_id,
          "<br><b>Cluster:</b> ", cluster_label,
          "<br><b>Anomaly score:</b> ", round(anomaly_score, 3)
        ),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          xaxis  = list(title = "UMAP 1", zeroline = FALSE),
          yaxis  = list(title = "UMAP 2", zeroline = FALSE),
          legend = list(title         = list(text = "<b>Cluster</b>"),
                         tracegroupgap = 0,
                         itemsizing    = "constant"),
          paper_bgcolor = "#FAFAFA",
          plot_bgcolor  = "#F0F2F5"
        )
    })

    # ── 5. Exposure → Outcome timing (conditional) ────────────────────────
    timing_df_rv <- shiny::reactive({
      shiny::req(
        input$show_exposure,
        outcome_labels_rv(), exposure_labels_rv(), ml_results(),
        input$exposure_concept_id, nzchar(input$exposure_concept_id)
      )

      oc  <- outcome_labels_rv()
      exp <- exposure_labels_rv()
      ml  <- ml_results()

      if (is.null(oc) || is.null(exp) || is.null(ml$merged)) return(NULL)

      df <- dplyr::left_join(
        oc,
        dplyr::select(exp, subject_id,
                       exposure_days = days_to_first_event),
        by = "subject_id"
      ) |>
        dplyr::left_join(
          dplyr::select(ml$merged, subject_id, cluster_id, cluster_label),
          by = "subject_id"
        ) |>
        dplyr::filter(!is.na(exposure_days), !is.na(days_to_first_event)) |>
        dplyr::mutate(
          days_exp_to_outcome = days_to_first_event - exposure_days,
          cluster_label       = dplyr::coalesce(cluster_label, "Unassigned")
        )

      if (nrow(df) == 0L) return(NULL)
      df
    })

    output$exp_outcome_violin <- plotly::renderPlotly({
      df <- timing_df_rv()
      if (is.null(df)) {
        return(empty_plotly(
          "No patients had both exposure and outcome in the selected window."
        ))
      }

      clusters <- sort(unique(df$cluster_label))
      traces   <- lapply(seq_along(clusters), function(i) {
        sub   <- dplyr::filter(df, cluster_label == clusters[[i]])
        col   <- .OC_CLUSTER_COLORS[[(i - 1L) %% length(.OC_CLUSTER_COLORS) + 1L]]
        plotly::plot_ly(
          y    = ~sub$days_exp_to_outcome,
          type = "violin",
          name = clusters[[i]],
          box  = list(visible = TRUE),
          meanline = list(visible = TRUE),
          line   = list(color = col),
          fillcolor = paste0(col, "55"),
          hovertemplate = paste0(
            clusters[[i]], "<br>Days: %{y:.0f}<extra></extra>"
          )
        )
      })

      p <- do.call(plotly::subplot, c(traces, list(nrows = 1L, shareY = TRUE)))
      plotly::layout(p,
        title  = list(text = "Days: exposure → outcome", font = list(size = 13)),
        yaxis  = list(title = "Days (exposure → outcome)", zeroline = TRUE),
        legend = list(orientation = "h", x = 0, y = -0.15),
        paper_bgcolor = "#FAFAFA",
        plot_bgcolor  = "#FAFAFA"
      )
    })

    output$exp_outcome_scatter <- plotly::renderPlotly({
      df <- timing_df_rv()
      if (is.null(df)) {
        return(empty_plotly(
          "No patients had both exposure and outcome in the selected window."
        ))
      }

      # Reference line y = x (outcome at same time as exposure)
      x_range <- range(df$exposure_days, na.rm = TRUE)

      df$cluster_label_f <- factor(df$cluster_label)
      plotly::plot_ly(
        data      = df,
        x         = ~exposure_days,
        y         = ~days_to_first_event,
        color     = ~cluster_label_f,
        type      = "scatter",
        mode      = "markers",
        marker    = list(size = 7, opacity = 0.8),
        text      = ~paste0(
          "<b>Patient:</b> ", subject_id,
          "<br><b>Cluster:</b> ", cluster_label,
          "<br><b>Exposure day:</b> ", exposure_days,
          "<br><b>Outcome day:</b> ", days_to_first_event,
          "<br><b>Lag:</b> ", days_exp_to_outcome, " days"
        ),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          xaxis  = list(title = "Exposure: days from index"),
          yaxis  = list(title = "Outcome: days from index"),
          shapes = list(list(
            type = "line",
            x0 = x_range[[1L]], x1 = x_range[[2L]],
            y0 = x_range[[1L]], y1 = x_range[[2L]],
            line = list(color = "#64748b", dash = "dot", width = 1)
          )),
          legend = list(title = list(text = "<b>Cluster</b>"),
                         tracegroupgap = 0),
          paper_bgcolor = "#FAFAFA",
          plot_bgcolor  = "#F0F2F5"
        )
    })
  })
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Populate a shinyWidgets::pickerInput with concepts from a domain tibble.
.update_concept_picker <- function(session, domain_data, domain, input_id) {
  name_col    <- CohortIntelligence:::.oc_domain_name_col[[domain]]
  concept_col <- CohortIntelligence:::.oc_domain_concept_col[[domain]]
  dd          <- domain_data[[domain]]

  if (is.null(dd) || nrow(dd) == 0L ||
      !concept_col %in% names(dd) || !name_col %in% names(dd)) {
    shinyWidgets::updatePickerInput(session, input_id,
                                    choices  = c("(no data in domain)" = ""),
                                    selected = "")
    return()
  }

  choices_df <- dd |>
    dplyr::distinct(.data[[concept_col]], .data[[name_col]]) |>
    dplyr::filter(!is.na(.data[[concept_col]]),
                  !is.na(.data[[name_col]]),
                  !grepl("^(Unmapped concept|No matching concept)$",
                          .data[[name_col]], ignore.case = TRUE)) |>
    dplyr::arrange(.data[[name_col]])

  if (nrow(choices_df) == 0L) {
    shinyWidgets::updatePickerInput(session, input_id,
                                    choices  = c("(no named concepts)" = ""),
                                    selected = "")
    return()
  }

  named_vec <- stats::setNames(
    as.character(choices_df[[concept_col]]),
    paste0(choices_df[[name_col]],
           " [", choices_df[[concept_col]], "]")
  )
  shinyWidgets::updatePickerInput(session, input_id,
                                   choices  = named_vec,
                                   selected = named_vec[[1L]])
}

# Simple key–value row for the outcome summary panel.
.oc_stat_row <- function(label, value) {
  shiny::div(
    style = "display:flex; justify-content:space-between; padding:4px 0;",
    shiny::span(style = "color:#64748b; font-size:0.85em;", label),
    shiny::span(style = "font-weight:700; font-size:0.88em;", value)
  )
}
