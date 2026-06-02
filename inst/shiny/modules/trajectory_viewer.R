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
#' @export
trajectory_viewerServer <- function(id, selected_patient,
                                     domain_data, cohort_members) {
  shiny::moduleServer(id, function(input, output, session) {

    output$patient_badge <- shiny::renderUI({
      pid <- selected_patient()
      if (is.null(pid)) {
        return(shiny::p(
          shiny::icon("arrow-pointer"),
          " Select a patient",
          style = "color:#94a3b8; margin-top:26px; font-size:0.86em;"
        ))
      }
      shiny::div(
        class = "alert alert-info",
        style = paste0("margin-top:18px; padding:5px 10px;",
                       "font-size:0.88em; text-align:center;"),
        shiny::tags$b("Patient: "), as.character(pid)
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
