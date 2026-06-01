# hypothesis_panel.R
# Shiny module: hypothesis generation and display panel.
# Auto-generates on first cohort load; button re-runs with new parameters.

#' Hypothesis panel module UI
#' @param id Shiny module namespace ID.
#' @export
hypothesis_panelUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3,
        shiny::numericInput(ns("max_hypotheses"), "Max hypotheses",
                            value = 20L, min = 1L, max = 100L)
      ),
      shiny::column(3,
        shiny::numericInput(ns("min_effect"), "Min effect size",
                            value = 0.3, min = 0, max = 1, step = 0.05)
      ),
      shiny::column(3,
        shiny::actionButton(ns("run_hypotheses"), "Re-run",
                            icon  = shiny::icon("rotate"),
                            class = "btn-default btn-sm",
                            style = "margin-top: 24px;")
      )
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::uiOutput(ns("hyp_status")))
    ),
    shiny::fluidRow(
      shiny::column(12, DT::DTOutput(ns("hyp_table")))
    ),
    shiny::fluidRow(
      shiny::column(12,
        shiny::downloadButton(ns("download_hyp"), "Download CSV",
                              class = "btn-sm btn-default")
      )
    )
  )
}

#' Hypothesis panel module server
#' @param id Module ID.
#' @param feature_matrix `reactive` returning list from [build_feature_matrix()].
#' @param ml_results `reactive` returning list from [run_full_ml_pipeline()].
#' @export
hypothesis_panelServer <- function(id, feature_matrix, ml_results) {
  shiny::moduleServer(id, function(input, output, session) {

    hypotheses_rv <- shiny::reactiveVal(NULL)

    .run_hypotheses <- function() {
      shiny::req(feature_matrix(), ml_results())
      shiny::withProgress(message = "Generating hypotheses...", value = 0.5, {
        hyp <- tryCatch(
          generate_hypotheses(
            feature_matrix  = feature_matrix(),
            ml_results      = ml_results(),
            min_effect_size = input$min_effect     %||% 0.3,
            max_hypotheses  = as.integer(input$max_hypotheses %||% 20L)
          ),
          error = function(e) {
            shiny::showNotification(
              paste("Hypothesis error:", conditionMessage(e)), type = "error")
            NULL
          }
        )
        hypotheses_rv(hyp)
      })
    }

    # Auto-run the first time ml_results becomes available
    shiny::observeEvent(ml_results(), {
      shiny::req(ml_results())
      if (is.null(hypotheses_rv())) .run_hypotheses()
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Re-run button (lets user change parameters and re-generate)
    shiny::observeEvent(input$run_hypotheses, {
      .run_hypotheses()
    })

    output$hyp_status <- shiny::renderUI({
      h <- hypotheses_rv()
      if (is.null(h)) {
        return(shiny::p("Generating hypotheses...",
                        style = "color: #666; font-style: italic;"))
      }
      shiny::div(
        class = "alert alert-success",
        style = "padding: 8px 12px;",
        sprintf("%d hypothesis candidate%s found.",
                nrow(h), if (nrow(h) == 1L) "" else "s")
      )
    })

    output$hyp_table <- DT::renderDT({
      h <- hypotheses_rv()
      if (is.null(h) || nrow(h) == 0L) {
        return(DT::datatable(
          data.frame(Message = "No significant hypotheses found."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      DT::datatable(
        dplyr::select(h, hypothesis_id, cluster_a, cluster_b, domain,
                      concept_name, window_label, effect_size,
                      p_value_adjusted, direction),
        selection = "none",
        rownames  = FALSE,
        options   = list(pageLength = 10, scrollX = TRUE),
        class     = "compact stripe hover"
      ) |>
        DT::formatRound(c("effect_size", "p_value_adjusted"), digits = 3)
    })

    output$download_hyp <- shiny::downloadHandler(
      filename = function() paste0("hypotheses_", Sys.Date(), ".csv"),
      content  = function(file) {
        h <- hypotheses_rv()
        if (!is.null(h)) utils::write.csv(h, file, row.names = FALSE)
      }
    )
  })
}
