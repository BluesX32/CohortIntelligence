# trajectory_viewer.R
# Shiny module: per-patient clinical trajectory (swim-lane) viewer.
# Renders whenever selected_patient changes.

#' Trajectory viewer module UI
#' @param id Shiny module namespace ID.
#' @export
trajectory_viewerUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        shinyWidgets::checkboxGroupButtons(
          ns("domains"),
          "Domains",
          choices   = c("condition","drug","procedure","measurement","observation","visit"),
          selected  = c("condition","drug","procedure","measurement","observation","visit"),
          justified = TRUE, size = "xs"
        )
      ),
      shiny::column(2,
        shiny::numericInput(ns("top_n"), "Top N concepts", value = 10L, min = 1L, max = 100L)
      ),
      shiny::column(3,
        shinyWidgets::sliderTextInput(
          ns("day_range"),
          "Day range (from index)",
          choices     = as.character(seq(-720, 360, by = 30)),
          selected    = c("-360", "180"),
          grid        = FALSE,
          force_edges = TRUE
        )
      ),
      shiny::column(3,
        shiny::uiOutput(ns("patient_badge"))
      )
    ),
    shiny::fluidRow(
      shiny::column(12,
        plotly::plotlyOutput(ns("traj_plot"), height = "550px")
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
trajectory_viewerServer <- function(id, selected_patient, domain_data, cohort_members) {
  shiny::moduleServer(id, function(input, output, session) {

    output$patient_badge <- shiny::renderUI({
      pid <- selected_patient()
      if (is.null(pid)) return(shiny::p("No patient selected.", style = "color:#999; margin-top:26px;"))
      shiny::div(
        class = "alert alert-info",
        style = "margin-top: 20px; padding: 6px 10px;",
        shiny::tags$b("Patient: "), as.character(pid)
      )
    })

    timeline <- shiny::reactive({
      shiny::req(selected_patient(), domain_data(), cohort_members())
      build_patient_timeline(
        subject_id     = selected_patient(),
        domain_data    = domain_data(),
        cohort_members = cohort_members(),
        domains        = input$domains %||% c("condition","drug","procedure",
                                               "measurement","observation","visit"),
        top_n          = as.integer(input$top_n %||% 10L)
      )
    })

    output$traj_plot <- plotly::renderPlotly({
      shiny::req(timeline())
      day_range_num <- suppressWarnings(as.integer(input$day_range))
      if (any(is.na(day_range_num))) day_range_num <- NULL
      plot_patient_timeline(
        timeline_df = timeline(),
        interactive = TRUE,
        color_by    = "domain",
        show_index  = TRUE,
        date_range  = day_range_num
      )
    })
  })
}
