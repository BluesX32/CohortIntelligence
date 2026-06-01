# patient_selector.R
# Shiny module: priority-ranked patient review queue.
# Works with and without ML results (sparsity-only ranking as fallback).

#' Patient selector module UI
#' @param id Shiny module namespace ID.
#' @export
patient_selectorUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
        shinyWidgets::pickerInput(
          ns("tier_filter"), "Priority Tier",
          choices  = c("All", "high", "medium", "low", "unranked"),
          selected = "All"
        )
      ),
      shiny::column(3,
        shiny::numericInput(ns("top_n"), "Show top N",
                            value = 100L, min = 1L, max = 10000L)
      ),
      shiny::column(6,
        shiny::uiOutput(ns("rank_basis_badge"))
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
#'   Always non-NULL after cohort load (sparsity-only when ML unavailable).
#' @param selected_patient `reactiveVal(NULL)` shared across modules.
#' @export
patient_selectorServer <- function(id, rank_df, selected_patient) {
  shiny::moduleServer(id, function(input, output, session) {

    # Badge showing whether ML-based or sparsity-only ranking is active
    output$rank_basis_badge <- shiny::renderUI({
      rd <- rank_df()
      if (is.null(rd)) return(NULL)
      has_ml <- any(rd$anomaly_score > 0, na.rm = TRUE)
      if (has_ml) {
        shiny::tags$span(
          class = "label label-primary",
          style = "padding:4px 8px; font-size:0.82em; margin-top:28px; display:inline-block;",
          shiny::icon("robot"), " ML-ranked  (anomaly + sparsity)"
        )
      } else {
        shiny::tags$span(
          class = "label label-default",
          style = paste0("padding:4px 8px; font-size:0.82em; margin-top:28px;",
                         " display:inline-block;"),
          shiny::icon("sort-amount-down"),
          " Sparsity-ranked  (ML unavailable — ",
          shiny::tags$code("install.packages('uwot')"), ")"
        )
      }
    })

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
        dplyr::mutate(
          rank_score    = round(rank_score, 3),
          anomaly_score = round(anomaly_score, 3),
          sparsity_score = round(sparsity_score, 3)
        ) |>
        dplyr::select(rank_position, subject_id, priority_tier,
                      anomaly_score, sparsity_score, cluster_id)

      DT::datatable(
        df,
        colnames  = c("Rank", "Patient ID", "Priority",
                      "Anomaly Score", "Sparsity", "Cluster"),
        selection = "single",
        rownames  = FALSE,
        options   = list(
          pageLength = 20, scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = c(0, 5))
          )
        ),
        class = "compact stripe hover"
      ) |>
        DT::formatStyle(
          "priority_tier",
          backgroundColor = DT::styleEqual(
            c("high", "medium", "low", "unranked"),
            c("#ffe0e0", "#fff3cd", "#d4edda", "#f5f5f5")
          ),
          fontWeight = DT::styleEqual(c("high"), c("bold"))
        ) |>
        DT::formatStyle(
          "anomaly_score",
          background = DT::styleColorBar(c(0, 1), "#fdae61"),
          backgroundSize     = "98% 60%",
          backgroundRepeat   = "no-repeat",
          backgroundPosition = "center"
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
