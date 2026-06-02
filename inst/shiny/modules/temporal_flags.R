# temporal_flags.R
# Temporal rule flags -- structured-data patterns that warrant clinical review.

.FLAG_COLORS <- c(high = "#dc2626", medium = "#d97706", low = "#2563eb")

#' Temporal flags module UI
#' @param id Module namespace ID.
#' @export
temporal_flagsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("box_high"),    width = 4),
      shinydashboard::valueBoxOutput(ns("box_common"),  width = 4),
      shinydashboard::valueBoxOutput(ns("box_multi"),   width = 4)
    ),
    shiny::div(
      style = paste0("background:#fef9ec; border:1px solid #fde68a;",
                     "border-radius:6px; padding:8px 14px; margin-bottom:12px;"),
      shiny::icon("triangle-exclamation", style = "color:#d97706;"),
      shiny::tags$b(" Temporal flags are review triggers, not clinical conclusions."),
      shiny::tags$span(
        style = "font-size:0.84em; color:#78716c;",
        " Use 'potential temporal inconsistency' and 'may require review' language."
      )
    ),
    shiny::fluidRow(
      shiny::column(3,
        shinyWidgets::pickerInput(
          ns("sev_filter"), "Severity",
          choices = c("All", "high", "medium", "low"), selected = "All"
        )
      ),
      shiny::column(3,
        shinyWidgets::pickerInput(
          ns("type_filter"), "Flag type",
          choices = NULL, multiple = TRUE,
          options = shinyWidgets::pickerOptions(actionsBox = TRUE)
        )
      )
    ),
    DT::DTOutput(ns("flag_table"))
  )
}

#' Temporal flags module server
#' @param id Module ID.
#' @param temporal_flags `reactive` returning tibble from [detect_temporal_flags()].
#' @param selected_patient `reactiveVal(NULL)`.
#' @export
temporal_flagsServer <- function(id, temporal_flags, selected_patient) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      tf <- temporal_flags()
      if (!is.null(tf) && nrow(tf) > 0L) {
        types <- sort(unique(tf$flag_type))
        shinyWidgets::updatePickerInput(session, "type_filter",
                                         choices = types,
                                         selected = types)
      }
    })

    output$box_high <- shinydashboard::renderValueBox({
      tf <- temporal_flags()
      n  <- if (!is.null(tf)) sum(tf$severity == "high", na.rm = TRUE) else 0L
      shinydashboard::valueBox(n, "High-severity flags",
                                icon = shiny::icon("circle-exclamation"),
                                color = "red")
    })

    output$box_common <- shinydashboard::renderValueBox({
      tf <- temporal_flags()
      lbl <- if (!is.null(tf) && nrow(tf) > 0L) {
        top <- sort(table(tf$flag_type), decreasing = TRUE)
        gsub("_", " ", names(top)[1])
      } else "None"
      shinydashboard::valueBox(lbl, "Most common flag",
                                icon = shiny::icon("flag"),
                                color = "yellow")
    })

    output$box_multi <- shinydashboard::renderValueBox({
      tf  <- temporal_flags()
      n   <- if (!is.null(tf) && nrow(tf) > 0L)
        sum(table(tf$subject_id) >= 2L) else 0L
      shinydashboard::valueBox(n, "Patients with >=2 flags",
                                icon = shiny::icon("layer-group"),
                                color = "orange")
    })

    filtered_flags <- shiny::reactive({
      tf <- temporal_flags()
      shiny::req(tf, nrow(tf) > 0L)
      if (input$sev_filter != "All") tf <- dplyr::filter(tf, severity == input$sev_filter)
      sel_types <- input$type_filter
      if (!is.null(sel_types) && length(sel_types) > 0L) {
        tf <- dplyr::filter(tf, flag_type %in% sel_types)
      }
      tf
    })

    output$flag_table <- DT::renderDT({
      shiny::req(filtered_flags())
      df <- filtered_flags() |>
        dplyr::select(subject_id, flag_label, severity,
                      domain, evidence_summary, recommended_action)

      DT::datatable(
        df,
        colnames  = c("Patient", "Flag", "Severity", "Domain",
                      "Evidence", "Action"),
        selection = "single", rownames = FALSE,
        options   = list(pageLength = 15, scrollX = TRUE, dom = "ftp"),
        class     = "compact stripe hover"
      ) |>
        DT::formatStyle(
          "severity",
          backgroundColor = DT::styleEqual(
            c("high","medium","low"),
            c("#fef2f2","#fffbeb","#eff6ff")
          ),
          color = DT::styleEqual(
            c("high","medium","low"),
            c("#dc2626","#d97706","#2563eb")
          ),
          fontWeight = DT::styleEqual(c("high"), c("bold"))
        )
    })

    shiny::observeEvent(input$flag_table_rows_selected, {
      idx <- input$flag_table_rows_selected
      if (!is.null(idx)) {
        pid <- filtered_flags()$subject_id[[idx]]
        selected_patient(pid)
      }
    })

    shiny::outputOptions(output, "box_high",   suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "box_common", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "box_multi",  suspendWhenHidden = FALSE)
  })
}
