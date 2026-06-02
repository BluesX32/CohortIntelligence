# review_sets.R
# Guided patient review queue -- eight clinically-motivated patient groups.

.SET_DEFS <- list(
  list(name="Typical patients",       icon="users",            color="#16a34a",
       why="Use as calibration reference -- low anomaly, adequate data coverage."),
  list(name="Most anomalous",         icon="triangle-exclamation", color="#dc2626",
       why="Statistically unusual clinical patterns -- possible rare subtypes or data issues."),
  list(name="Sparse follow-up",       icon="calendar-xmark",  color="#d97706",
       why="Limited post-index data -- may reflect incomplete follow-up or care transfer."),
  list(name="Rare cluster",           icon="microscope",       color="#7c3aed",
       why="Small or unassigned cluster -- possible rare phenotype, warrants review."),
  list(name="High post-index activity", icon="arrow-trend-up", color="#0284c7",
       why="High event volume after index date -- possible intensive management."),
  list(name="High pre-index activity", icon="clock-rotate-left", color="#0284c7",
       why="Complex prior history before index -- may affect cohort entry validity."),
  list(name="Boundary patients",      icon="code-merge",       color="#64748b",
       why="Moderate anomaly, not clearly in one cluster -- borderline classification."),
  list(name="Temporal concern",       icon="flag",             color="#dc2626",
       why="High-severity temporal rule flag -- structured-data pattern requires review.")
)

#' Review sets module UI
#' @param id Module namespace ID.
#' @export
review_setsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Guidance banner
    shiny::div(
      style = paste0("background:#f0fdf4; border:1px solid #bbf7d0;",
                     "border-radius:6px; padding:10px 16px; margin-bottom:16px;"),
      shiny::icon("lightbulb", style = "color:#16a34a;"),
      shiny::tags$b(" Suggested order: "),
      "Start with ",
      shiny::tags$b("Typical patients"), " to calibrate expectations, then ",
      shiny::tags$b("Most Anomalous"), ", then ",
      shiny::tags$b("Sparse Follow-up"), ". ",
      "Review ", shiny::tags$b("Temporal Concern"), " patients last (flag review first).",
      shiny::tags$p(
        style = "font-size:0.78em; color:#64748b; margin:4px 0 0;",
        "All sets are hypothesis-generating. Requires clinical review. ",
        "Do not interpret as clinical conclusions."
      )
    ),
    # Set overview cards
    shiny::uiOutput(ns("set_cards")),
    shiny::hr(),
    # Active set label
    shiny::uiOutput(ns("active_set_label")),
    # Patient table
    DT::DTOutput(ns("queue_table"))
  )
}

#' Review sets module server
#' @param id Module ID.
#' @param review_sets_rv `reactive` returning tibble from [build_review_sets()].
#' @param selected_patient `reactiveVal(NULL)`.
#' @export
review_setsServer <- function(id, review_sets_rv, selected_patient) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    active_set <- shiny::reactiveVal(NULL)

    output$set_cards <- shiny::renderUI({
      rs <- review_sets_rv()
      if (is.null(rs) || nrow(rs) == 0L) {
        return(shiny::p(style = "color:#999;", "No review sets available yet."))
      }
      set_counts <- table(rs$review_set)

      cards <- lapply(seq_along(.SET_DEFS), function(i) {
        def <- .SET_DEFS[[i]]
        n   <- as.integer(set_counts[def$name] %||% 0L)
        shiny::column(3,
          shiny::div(
            style = paste0(
              "border-left:4px solid ", def$color, ";",
              "background:#fafafa; border-radius:6px;",
              "padding:10px 14px; margin-bottom:10px;",
              "cursor:pointer;"
            ),
            onclick = paste0(
              "Shiny.setInputValue('", ns("active_set_click"), "',",
              "'", def$name, "', {priority:'event'});"
            ),
            shiny::div(
              style = "display:flex; justify-content:space-between;",
              shiny::tags$b(
                style = paste0("color:", def$color, "; font-size:0.88em;"),
                shiny::icon(def$icon), " ", def$name
              ),
              shiny::tags$span(
                style = paste0("background:", def$color,
                               "; color:#fff; border-radius:10px;",
                               "padding:1px 8px; font-size:0.78em;"),
                n
              )
            ),
            shiny::tags$p(
              style = "font-size:0.78em; color:#64748b; margin:4px 0 0;",
              def$why
            )
          )
        )
      })

      # Split into rows of 4
      pairs <- split(cards, ceiling(seq_along(cards) / 4))
      shiny::tagList(lapply(pairs, function(row) do.call(shiny::fluidRow, row)))
    })

    shiny::observeEvent(input$active_set_click, {
      active_set(input$active_set_click)
    })

    output$active_set_label <- shiny::renderUI({
      aset <- active_set()
      if (is.null(aset)) return(shiny::p(
        style = "color:#64748b; font-size:0.88em;",
        shiny::icon("table-list"),
        " Click a set card above to filter, or browse all patients below."
      ))
      def <- .SET_DEFS[[which(sapply(.SET_DEFS, `[[`, "name") == aset)]]
      shiny::div(
        style = paste0("background:", def$color,
                       "11; border:1px solid ", def$color,
                       "44; border-radius:6px;",
                       "padding:8px 14px; margin-bottom:8px;"),
        shiny::tags$b(style = paste0("color:", def$color), aset),
        shiny::tags$span(
          style = "font-size:0.84em; color:#475569; margin-left:8px;",
          def$why
        ),
        shiny::actionLink(ns("clear_set"), "  Show all",
                          style = "font-size:0.82em; margin-left:12px;")
      )
    })

    shiny::observeEvent(input$clear_set, active_set(NULL))

    filtered_rs <- shiny::reactive({
      shiny::req(review_sets_rv())
      rs <- review_sets_rv()
      aset <- active_set()
      if (!is.null(aset)) dplyr::filter(rs, review_set == aset) else rs
    })

    output$queue_table <- DT::renderDT({
      shiny::req(filtered_rs())
      df <- filtered_rs() |>
        dplyr::arrange(set_priority, rank_position) |>
        dplyr::select(review_set, subject_id, reason_for_selection,
                      anomaly_score, sparsity_score, cluster_id) |>
        dplyr::mutate(
          anomaly_score  = round(anomaly_score, 3),
          sparsity_score = round(sparsity_score, 3)
        )

      DT::datatable(
        df,
        colnames  = c("Review Set", "Patient ID", "Reason", "Anomaly",
                      "Sparsity", "Cluster"),
        selection = "single", rownames = FALSE,
        options   = list(pageLength = 15, scrollX = TRUE,
                          dom = "ftp"),
        class     = "compact stripe hover"
      ) |>
        DT::formatStyle(
          "review_set",
          backgroundColor = DT::styleEqual(
            sapply(.SET_DEFS, `[[`, "name"),
            paste0(sapply(.SET_DEFS, `[[`, "color"), "22")
          )
        ) |>
        DT::formatStyle(
          "anomaly_score",
          background = DT::styleColorBar(c(0, 1), "#d73027"),
          backgroundSize = "98% 60%", backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })

    shiny::observeEvent(input$queue_table_rows_selected, {
      idx <- input$queue_table_rows_selected
      if (!is.null(idx)) {
        pid <- filtered_rs()$subject_id[[idx]]
        selected_patient(pid)
      }
    })
  })
}
