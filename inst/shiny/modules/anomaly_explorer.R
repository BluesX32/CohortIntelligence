# anomaly_explorer.R
# Shiny module: UMAP scatter + isolation forest anomaly ranking.
# Highlights the selected_patient across all views.

#' Anomaly explorer module UI
#' @param id Shiny module namespace ID.
#' @export
anomaly_explorerUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(8,
        shiny::h4("UMAP Projection"),
        plotly::plotlyOutput(ns("umap_plot"), height = "500px")
      ),
      shiny::column(4,
        shiny::h4("Anomaly Score Distribution"),
        plotly::plotlyOutput(ns("anomaly_hist"), height = "240px"),
        shiny::h4("Top Anomalous Patients"),
        DT::DTOutput(ns("anomaly_table"))
      )
    )
  )
}

#' Anomaly explorer module server
#' @param id Module ID.
#' @param ml_results `reactive` returning list from [run_full_ml_pipeline()].
#' @param selected_patient `reactiveVal(NULL)` shared across modules.
#' @export
anomaly_explorerServer <- function(id, ml_results, selected_patient) {
  shiny::moduleServer(id, function(input, output, session) {

    output$umap_plot <- plotly::renderPlotly({
      shiny::req(ml_results())
      ml  <- ml_results()
      df  <- ml$merged
      if (is.null(df) || nrow(df) == 0L) return(plotly::plot_ly())

      df$selected <- df$subject_id == (selected_patient() %||% -1L)
      df$marker_size <- ifelse(df$selected, 14, 7)
      df$marker_sym  <- ifelse(df$selected, "star", "circle")

      plotly::plot_ly(
        data     = df,
        x        = ~umap_1,
        y        = ~umap_2,
        color    = ~as.factor(cluster_id),
        text     = ~paste0("Patient: ", subject_id,
                            "<br>Cluster: ", cluster_id,
                            "<br>Anomaly: ", round(anomaly_score, 3)),
        hoverinfo = "text",
        type     = "scatter",
        mode     = "markers",
        marker   = list(size = 7, opacity = 0.8),
        source   = "umap"
      ) |>
        plotly::layout(
          xaxis = list(title = "UMAP 1"),
          yaxis = list(title = "UMAP 2"),
          legend = list(title = list(text = "Cluster"))
        ) |>
        plotly::event_register("plotly_click")
    })

    shiny::observeEvent(plotly::event_data("plotly_click", source = "umap"), {
      click <- plotly::event_data("plotly_click", source = "umap")
      if (!is.null(click)) {
        ml  <- ml_results()
        df  <- ml$merged
        pid <- df$subject_id[which.min(
          (df$umap_1 - click$x)^2 + (df$umap_2 - click$y)^2
        )]
        if (length(pid) > 0L) selected_patient(pid[[1L]])
      }
    })

    output$anomaly_hist <- plotly::renderPlotly({
      shiny::req(ml_results())
      df <- ml_results()$anomaly
      if (is.null(df)) return(plotly::plot_ly())
      plotly::plot_ly(x = ~df$anomaly_score, type = "histogram",
                      marker = list(color = "#2c7bb6")) |>
        plotly::layout(xaxis = list(title = "Anomaly Score"),
                       yaxis = list(title = "Count"))
    })

    output$anomaly_table <- DT::renderDT({
      shiny::req(ml_results())
      df <- ml_results()$anomaly |>
        dplyr::arrange(dplyr::desc(anomaly_score)) |>
        dplyr::slice_head(n = 50L)
      DT::datatable(df, selection = "single", rownames = FALSE,
                    options = list(pageLength = 10, dom = "tp"))
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
