# patient_selector.R
# Shiny module: sortable patient review queue with priority-tier filtering.

#' Patient selector module UI
#' @param id Shiny module namespace ID.
#' @export
patient_selectorUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
        shinyWidgets::pickerInput(
          ns("tier_filter"),
          "Priority Tier",
          choices  = c("All","high","medium","low","unranked"),
          selected = "All"
        )
      ),
      shiny::column(3,
        shiny::numericInput(ns("top_n"), "Show top N", value = 100L, min = 1L, max = 10000L)
      ),
      shiny::column(6,
        shiny::p("Click a row to select a patient for trajectory review.",
                 style = "margin-top: 26px; color: #666;")
      )
    ),
    shiny::fluidRow(
      shiny::column(12,
        DT::DTOutput(ns("ranking_table"))
      )
    )
  )
}

#' Patient selector module server
#' @param id Module ID.
#' @param rank_df `reactive` returning tibble from [rank_patients()].
#' @param selected_patient `reactiveVal(NULL)` shared across modules.
#' @export
patient_selectorServer <- function(id, rank_df, selected_patient) {
  shiny::moduleServer(id, function(input, output, session) {

    filtered_rank <- shiny::reactive({
      shiny::req(rank_df())
      df <- rank_df()
      if (input$tier_filter != "All") {
        df <- dplyr::filter(df, priority_tier == input$tier_filter)
      }
      df |>
        dplyr::arrange(rank_position) |>
        dplyr::slice_head(n = as.integer(input$top_n %||% 100L))
    })

    output$ranking_table <- DT::renderDT({
      shiny::req(filtered_rank())
      df <- filtered_rank() |>
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
      DT::datatable(
        df,
        selection = "single",
        rownames  = FALSE,
        options   = list(pageLength = 20, scrollX = TRUE),
        class     = "compact stripe hover"
      ) |>
        DT::formatStyle(
          "priority_tier",
          backgroundColor = DT::styleEqual(
            c("high","medium","low","unranked"),
            c("#ffcccc","#ffe0b2","#e8f5e9","#f5f5f5")
          )
        )
    })

    shiny::observeEvent(input$ranking_table_rows_selected, {
      idx <- input$ranking_table_rows_selected
      if (!is.null(idx) && length(idx) > 0L) {
        pid <- filtered_rank()$subject_id[[idx]]
        selected_patient(pid)
      }
    })
  })
}
