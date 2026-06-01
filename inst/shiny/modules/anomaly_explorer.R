# anomaly_explorer.R
# Shiny module: UMAP scatter + isolation forest anomaly ranking.

#' Anomaly explorer module UI
#' @param id Shiny module namespace ID.
#' @export
anomaly_explorerUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8,
        shiny::h4("UMAP Projection",
                  shiny::tags$small(
                    style = "font-size:0.7em; color:#666; margin-left:8px;",
                    "Points = patients, colour = cluster, proximity = clinical similarity"
                  )),
        plotly::plotlyOutput(ns("umap_plot"), height = "480px")
      ),
      shiny::column(4,
        shiny::h4("Anomaly Score Distribution",
                  shiny::tags$small(
                    style = "font-size:0.7em; color:#666; margin-left:8px;",
                    "Higher = more unusual"
                  )),
        plotly::plotlyOutput(ns("anomaly_hist"), height = "220px"),
        shiny::hr(),
        shiny::h4("Top Anomalous Patients"),
        DT::DTOutput(ns("anomaly_table"))
      )
    )
  )
}

#' Anomaly explorer module server
#' @param id Module ID.
#' @param ml_results `reactive` returning list from [run_full_ml_pipeline()],
#'   or `NULL` when the ML pipeline was unavailable.
#' @param selected_patient `reactiveVal(NULL)` shared across modules.
#' @export
anomaly_explorerServer <- function(id, ml_results, selected_patient) {
  shiny::moduleServer(id, function(input, output, session) {

    output$umap_plot <- plotly::renderPlotly({
      ml <- ml_results()

      if (is.null(ml) || is.null(ml$merged) || nrow(ml$merged) == 0L) {
        return(empty_plotly(paste0(
          "ML pipeline unavailable.\n\n",
          "Install uwot for UMAP:\n",
          "install.packages('uwot')"
        )))
      }

      df          <- ml$merged
      df$selected <- df$subject_id == (selected_patient() %||% -1L)

      # Colour by cluster; selected patient gets a star marker
      plotly::plot_ly(
        data         = df,
        x            = ~umap_1,
        y            = ~umap_2,
        color        = ~as.factor(cluster_id),
        symbol       = ~ifelse(selected, "star", "circle"),
        symbols      = c("circle", "star"),
        size         = ~ifelse(selected, 12, 7),
        sizes        = c(6, 14),
        type         = "scatter",
        mode         = "markers",
        text         = ~paste0(
          "<b>Patient:</b> ", subject_id,
          "<br><b>Cluster:</b> ", cluster_id,
          "<br><b>Anomaly score:</b> ", round(anomaly_score, 3),
          "<br><i>Click to select</i>"
        ),
        hoverinfo    = "text",
        source       = "umap"
      ) |>
        plotly::layout(
          xaxis  = list(title = "UMAP 1",  zeroline = FALSE),
          yaxis  = list(title = "UMAP 2",  zeroline = FALSE),
          legend = list(title = list(text = "<b>Cluster</b>")),
          paper_bgcolor = "#FAFAFA",
          plot_bgcolor  = "#F0F2F5"
        ) |>
        plotly::event_register("plotly_click")
    })

    shiny::observe({
      click <- plotly::event_data("plotly_click", source = "umap",
                                   session = session)
      shiny::req(!is.null(click), ml_results())
      df  <- ml_results()$merged
      pid <- df$subject_id[which.min(
        (df$umap_1 - click$x)^2 + (df$umap_2 - click$y)^2
      )]
      if (length(pid) > 0L) selected_patient(pid[[1L]])
    })

    output$anomaly_hist <- plotly::renderPlotly({
      ml <- ml_results()
      if (is.null(ml) || is.null(ml$anomaly)) {
        return(empty_plotly("No anomaly scores."))
      }
      df <- ml$anomaly
      plotly::plot_ly(
        x      = ~df$anomaly_score,
        type   = "histogram",
        nbinsx = 30,
        marker = list(
          color = "rgba(44,123,182,0.75)",
          line  = list(color = "#fff", width = 0.5)
        ),
        hovertemplate = "Score: %{x:.2f}<br>Patients: %{y}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis       = list(title = "Anomaly score  (0 = typical, 1 = unusual)",
                             range = c(0, 1)),
          yaxis       = list(title = "Patients"),
          bargap      = 0.05,
          paper_bgcolor = "#FAFAFA",
          plot_bgcolor  = "#FAFAFA"
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    output$anomaly_table <- DT::renderDT({
      ml <- ml_results()
      if (is.null(ml) || is.null(ml$anomaly)) {
        return(DT::datatable(
          data.frame(Message = "ML pipeline unavailable — install uwot."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      df <- ml$anomaly |>
        dplyr::arrange(dplyr::desc(anomaly_score)) |>
        dplyr::slice_head(n = 50L) |>
        dplyr::mutate(
          anomaly_score = round(anomaly_score, 3),
          rank = dplyr::row_number()
        ) |>
        dplyr::select(rank, subject_id, anomaly_score)

      DT::datatable(
        df,
        colnames  = c("Rank", "Patient ID", "Anomaly Score"),
        selection = "single",
        rownames  = FALSE,
        options   = list(pageLength = 10, dom = "tp",
                         columnDefs = list(list(className = "dt-center",
                                                targets = c(0, 2))))
      ) |>
        DT::formatStyle(
          "anomaly_score",
          background = DT::styleColorBar(c(0, 1), "#d73027"),
          backgroundSize  = "98% 60%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })

    shiny::observeEvent(input$anomaly_table_rows_selected, {
      shiny::req(ml_results())
      idx <- input$anomaly_table_rows_selected
      df  <- ml_results()$anomaly |>
        dplyr::arrange(dplyr::desc(anomaly_score)) |>
        dplyr::slice_head(n = 50L)
      if (!is.null(idx)) selected_patient(df$subject_id[[idx]])
    })
  })
}
