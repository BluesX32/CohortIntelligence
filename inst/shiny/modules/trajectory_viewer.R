# trajectory_viewer.R
# Shiny module: per-patient clinical trajectory (swim-lane) viewer.
# The plot height adapts to the number of concept rows; the left margin
# adapts to the longest truncated concept label.

#' Trajectory viewer module UI
#' @param id Shiny module namespace ID.
#' @export
trajectory_viewerUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      # Domain checkboxes
      shiny::column(5,
        shinyWidgets::checkboxGroupButtons(
          ns("domains"),
          label     = "Domains",
          choices   = c("condition","drug","procedure",
                         "measurement","observation","visit"),
          selected  = c("condition","drug","procedure",
                         "measurement","observation","visit"),
          justified = FALSE,
          size      = "xs",
          direction = "horizontal"
        )
      ),
      # Top N
      shiny::column(2,
        shiny::numericInput(ns("top_n"), "Top N per domain",
                            value = 8L, min = 1L, max = 50L)
      ),
      # Day range slider
      shiny::column(3,
        shiny::sliderInput(
          ns("day_range"),
          "Day range (from index)",
          min   = -1095L,
          max   = 1095L,
          value = c(-730L, 365L),
          step  = 30L,
          ticks = FALSE
        )
      ),
      # Patient badge
      shiny::column(2,
        shiny::uiOutput(ns("patient_badge"))
      )
    ),
    shiny::fluidRow(
      shiny::column(12,
        # Height is set to "auto" and controlled by the plot's own height
        # parameter so the timeline expands with more concept rows.
        plotly::plotlyOutput(ns("traj_plot"), height = "auto",
                             width = "100%")
      )
    )
  )
}

#' Trajectory viewer module server
#' @param id Module ID.
#' @param selected_patient `reactiveVal(NULL)`.
#' @param domain_data `reactive` returning named list of domain tibbles.
#' @param cohort_members `reactive` returning cohort member tibble.
#' @param rank_df `reactive` tibble from [rank_patients()]. Optional — used
#'   for the Review Context card.
#' @param temporal_flags `reactive` tibble from [detect_temporal_flags()].
#'   Optional — shown in Review Context card.
#' @export
trajectory_viewerServer <- function(id, selected_patient,
                                     domain_data, cohort_members,
                                     rank_df        = shiny::reactive(NULL),
                                     temporal_flags = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    output$patient_badge <- shiny::renderUI({
      pid <- selected_patient()
      if (is.null(pid)) {
        return(shiny::p(
          shiny::icon("arrow-pointer"),
          " Select a patient to inspect their structured evidence timeline.",
          style = "color:#94a3b8; margin-top:26px; font-size:0.86em;"
        ))
      }

      # ── Review context card ──────────────────────────────────────────────
      rd  <- rank_df()
      tf  <- temporal_flags()

      pat_row   <- if (!is.null(rd)) rd[rd$subject_id == pid, , drop = FALSE] else NULL
      pat_flags <- if (!is.null(tf)) tf[tf$subject_id == pid, , drop = FALSE] else NULL

      tier      <- if (!is.null(pat_row) && nrow(pat_row) > 0L) pat_row$priority_tier[[1L]] else NULL
      anom      <- if (!is.null(pat_row) && nrow(pat_row) > 0L) round(pat_row$anomaly_score[[1L]], 2) else NULL
      n_flags   <- if (!is.null(pat_flags)) nrow(pat_flags) else 0L
      hi_flags  <- if (!is.null(pat_flags) && nrow(pat_flags) > 0L)
        sum(pat_flags$severity == "high") else 0L

      tier_color <- switch(tier %||% "unranked",
        high     = "#dc2626",
        medium   = "#d97706",
        low      = "#16a34a",
        "#64748b"
      )

      shiny::div(
        style = paste0(
          "background:#f8fafc; border:1px solid #e2e8f0;",
          "border-left:4px solid #2563eb;",
          "border-radius:6px; padding:10px 14px; margin-bottom:8px;"
        ),
        shiny::div(
          style = "display:flex; justify-content:space-between; align-items:center;",
          shiny::tags$b(style = "color:#0f3460; font-size:0.9em;",
                         shiny::icon("stethoscope"), " Review Context"),
          shiny::tags$span(
            style = paste0("background:", tier_color,
                           "; color:#fff; border-radius:10px;",
                           "padding:1px 9px; font-size:0.75em;"),
            toupper(tier %||% "—")
          )
        ),
        shiny::div(
          style = "font-size:0.82em; color:#334155; margin-top:6px;",
          shiny::tags$b("Patient: "), as.character(pid), "  |  ",
          shiny::tags$b("Anomaly: "), anom %||% "N/A", "  |  ",
          shiny::tags$b("Flags: "),
          if (hi_flags > 0L)
            shiny::tags$span(style = "color:#dc2626;",
                              hi_flags, " high")
          else "none",
          if (n_flags > hi_flags && n_flags > 0L)
            shiny::tags$span(style = "color:#64748b;",
                              paste0(" (+", n_flags - hi_flags, " lower)"))
        ),
        shiny::div(
          style = "font-size:0.74em; color:#64748b; margin-top:4px; font-style:italic;",
          "This timeline shows structured OMOP records only.",
          "It does not replace full chart review."
        )
      )
    })

    timeline <- shiny::reactive({
      shiny::req(selected_patient(), domain_data(), cohort_members())
      tryCatch(
        build_patient_timeline(
          subject_id     = selected_patient(),
          domain_data    = domain_data(),
          cohort_members = cohort_members(),
          domains        = input$domains %||%
                             c("condition","drug","procedure",
                               "measurement","observation","visit"),
          top_n          = as.integer(input$top_n %||% 8L)
        ),
        error = function(e) {
          message("[CI Trajectory] build_patient_timeline error: ",
                  conditionMessage(e))
          NULL
        }
      )
    })

    output$traj_plot <- plotly::renderPlotly({
      tl <- timeline()
      if (is.null(tl) || nrow(tl) == 0L) {
        return(empty_plotly("No clinical data found for this patient."))
      }

      dr <- input$day_range
      date_range <- if (!is.null(dr) && length(dr) == 2L &&
                        all(!is.na(dr))) dr else NULL

      plot_patient_timeline(
        timeline_df = tl,
        interactive = TRUE,
        show_index  = TRUE,
        date_range  = date_range
      )
    })
  })
}
