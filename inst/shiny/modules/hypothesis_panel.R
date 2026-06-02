# hypothesis_panel.R
# Shiny module: hypothesis generation and display panel.
# Auto-generates on first cohort load; Re-run button for parameter changes.

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
                            value = 0.1, min = 0, max = 1, step = 0.05)
      ),
      shiny::column(3,
        shiny::actionButton(ns("run_hypotheses"), "Re-run",
                            icon  = shiny::icon("rotate"),
                            class = "btn-default btn-sm",
                            style = "margin-top: 24px;")
      )
    ),
    # Exploratory-inference warning — always visible above the table
    shiny::fluidRow(
      shiny::column(12,
        shiny::div(
          class = "alert alert-warning",
          style = paste0("padding: 10px 14px; margin-bottom: 12px;",
                         " font-size: 0.88em;"),
          shiny::icon("triangle-exclamation"),
          shiny::tags$b(" These comparisons are exploratory."),
          " Clusters are derived from the same feature matrix being compared,",
          " so p-values rank candidate signals only — they do not support",
          " confirmatory inference.",
          shiny::tags$b(" All hypotheses require independent validation and clinical review.")
        )
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
#' @param ml_results `reactive` returning list from [run_full_ml_pipeline()],
#'   or `NULL` when ML was unavailable.
#' @export
hypothesis_panelServer <- function(id, feature_matrix, ml_results) {
  shiny::moduleServer(id, function(input, output, session) {

    hypotheses_rv <- shiny::reactiveVal(NULL)
    status_rv     <- shiny::reactiveVal("pending")  # pending | computing | done | error

    .run <- function() {
      fm <- feature_matrix()
      ml <- ml_results()

      # Need both feature matrix and at least 2 clusters to test hypotheses
      n_clusters <- if (!is.null(ml) && !is.null(ml$clusters)) {
        length(unique(ml$clusters$cluster_id[ml$clusters$cluster_id > 0L]))
      } else {
        0L
      }

      if (is.null(fm) || is.null(ml)) {
        status_rv("no_ml")
        hypotheses_rv(NULL)
        return()
      }
      if (n_clusters < 2L) {
        status_rv("one_cluster")
        hypotheses_rv(NULL)
        return()
      }

      status_rv("computing")
      shiny::withProgress(message = "Generating hypotheses...", value = 0.5, {
        hyp <- tryCatch(
          generate_hypotheses(
            feature_matrix  = fm,
            ml_results      = ml,
            min_effect_size = input$min_effect     %||% 0.1,
            max_hypotheses  = as.integer(input$max_hypotheses %||% 20L)
          ),
          error = function(e) {
            message("[CohortIntelligence] Hypothesis error: ", conditionMessage(e))
            status_rv("error")
            NULL
          }
        )
        if (status_rv() != "error") {
          hypotheses_rv(hyp)
          status_rv("done")
        }
      })
    }

    # Auto-run first time ml_results becomes available
    shiny::observeEvent(ml_results(), {
      shiny::req(ml_results())
      if (is.null(hypotheses_rv()) && status_rv() == "pending") .run()
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Manual re-run with new parameters
    shiny::observeEvent(input$run_hypotheses, { .run() })

    output$hyp_status <- shiny::renderUI({
      st <- status_rv()
      h  <- hypotheses_rv()

      if (st == "pending" || st == "computing") {
        return(shiny::div(
          class = "alert alert-info",
          style = "padding: 8px 14px;",
          shiny::icon("spinner"), " Generating hypotheses..."
        ))
      }
      if (st == "no_ml") {
        return(shiny::div(
          class = "alert alert-warning",
          style = "padding: 8px 14px;",
          shiny::icon("triangle-exclamation"),
          shiny::tags$b(" ML pipeline unavailable."),
          " Hypothesis generation requires UMAP + clustering results. ",
          "Install ", shiny::tags$code("uwot"),
          " and reload the cohort: ",
          shiny::tags$code("install.packages('uwot')")
        ))
      }
      if (st == "one_cluster") {
        return(shiny::div(
          class = "alert alert-warning",
          style = "padding: 8px 14px;",
          shiny::icon("circle-info"),
          shiny::tags$b(" All patients are in one cluster."),
          " Hypothesis generation compares cluster pairs. ",
          "This usually means the ML pipeline assigned everyone to the same group ",
          "because the feature matrix is too sparse. Try lowering ",
          shiny::tags$code("min_concept_freq"),
          " or using a larger cohort."
        ))
      }
      if (st == "error") {
        return(shiny::div(
          class = "alert alert-danger",
          style = "padding: 8px 14px;",
          shiny::icon("circle-xmark"),
          " Hypothesis generation failed. Check the R console for details."
        ))
      }
      if (!is.null(h) && nrow(h) == 0L) {
        return(shiny::div(
          class = "alert alert-warning",
          style = "padding: 8px 14px;",
          shiny::icon("magnifying-glass"),
          shiny::tags$b(" No significant hypotheses found"),
          " at the current effect-size threshold (",
          round(input$min_effect %||% 0.1, 2), "). ",
          "Try lowering ", shiny::tags$b("Min effect size"), " to 0.05 and clicking Re-run."
        ))
      }
      if (!is.null(h)) {
        return(shiny::div(
          class = "alert alert-success",
          style = "padding: 8px 14px;",
          shiny::icon("lightbulb"),
          sprintf(" %d hypothesis candidate%s — sorted by adjusted p-value.",
                  nrow(h), if (nrow(h) == 1L) "" else "s")
        ))
      }
      NULL
    })

    output$hyp_table <- DT::renderDT({
      h <- hypotheses_rv()
      if (is.null(h) || nrow(h) == 0L) {
        return(DT::datatable(
          data.frame(Message = "No hypotheses to display."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }

      # Detect which p-value column name is present (handles both old and new)
      p_col <- if ("exploratory_p_adjusted" %in% names(h)) {
        "exploratory_p_adjusted"
      } else if ("p_value_adjusted" %in% names(h)) {
        "p_value_adjusted"
      } else {
        NULL
      }

      df <- dplyr::mutate(h,
        direction = dplyr::case_when(
          direction == "higher_in_cluster_a" ~
            paste0(shiny::icon("arrow-up"), " Cluster ", cluster_a),
          direction == "higher_in_cluster_b" ~
            paste0(shiny::icon("arrow-up"), " Cluster ", cluster_b),
          TRUE ~ direction
        )
      )

      select_cols <- c("hypothesis_id","cluster_a","cluster_b",
                       "domain","concept_name","window_label",
                       "effect_size", p_col, "direction")
      df <- dplyr::select(df, dplyr::all_of(select_cols[!is.null(select_cols)]))

      col_names <- c("#","Cluster A","Cluster B","Domain",
                     "Concept","Window",
                     "Effect Size","Exploratory adj. p-value","Higher In")

      DT::datatable(
        df,
        colnames  = col_names[seq_len(ncol(df))],
        selection = "none",
        rownames  = FALSE,
        options   = list(
          pageLength = 10, scrollX = TRUE,
          order      = list(list(7, "asc"))
        ),
        class = "compact stripe hover"
      ) |>
        DT::formatRound(c("effect_size"), digits = 3) |>
        DT::formatStyle(
          "effect_size",
          background = DT::styleColorBar(c(0, 1), "#2c7bb6"),
          backgroundSize     = "98% 60%",
          backgroundRepeat   = "no-repeat",
          backgroundPosition = "center"
        )
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
