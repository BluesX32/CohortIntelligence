# signal_explanation.R
# "Why this patient was selected for review"

.SEV_COLOR <- c(high = "#dc2626", medium = "#d97706", low = "#2563eb")
.SEV_ICON  <- c(high = "circle-exclamation", medium = "triangle-exclamation",
                low  = "circle-info")
.DOMAIN_COLOR <- c(
  condition = "#d73027", drug = "#313695", procedure = "#1a9641",
  measurement = "#6a3d9a", observation = "#b35806", visit = "#41b6c4",
  death = "#111111", multiple = "#64748b", all = "#64748b"
)

#' Signal explanation module UI
#' @param id Module namespace ID.
#' @export
signal_explanationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "margin-top: 12px;",
    shiny::uiOutput(ns("explanation_panel"))
  )
}

#' Signal explanation module server
#' @param id Module ID.
#' @param selected_patient `reactiveVal(NULL)` shared across modules.
#' @param rank_df `reactive` tibble from [rank_patients()].
#' @param feature_matrix `reactive` list from [build_feature_matrix()].
#' @param domain_activity `reactive` tibble from [build_domain_activity()].
#' @param cohort_members `reactive` tibble.
#' @param ml_results `reactive` list or NULL.
#' @export
signal_explanationServer <- function(id, selected_patient, rank_df,
                                      feature_matrix, domain_activity,
                                      cohort_members, ml_results) {
  shiny::moduleServer(id, function(input, output, session) {

    explanation <- shiny::reactive({
      pid <- selected_patient()
      shiny::req(pid, rank_df(), cohort_members())
      tryCatch(
        explain_patient_priority(
          subject_id     = pid,
          rank_df        = rank_df(),
          feature_matrix = feature_matrix(),
          domain_activity = domain_activity(),
          cohort_members = cohort_members(),
          ml_results     = ml_results(),
          top_n          = 6L
        ),
        error = function(e) NULL
      )
    })

    output$explanation_panel <- shiny::renderUI({
      pid <- selected_patient()
      if (is.null(pid)) {
        return(shiny::div(
          class = "alert alert-default",
          style = paste0("background:#f8fafc; border:1px solid #e2e8f0;",
                         " padding:10px 14px; border-radius:6px;"),
          shiny::icon("arrow-pointer"), " Select a patient to see why they were flagged."
        ))
      }

      ex <- explanation()
      if (is.null(ex) || nrow(ex) == 0L) {
        return(shiny::div(
          class = "alert alert-info",
          "No specific explanations found for this patient."
        ))
      }

      cards <- lapply(seq_len(nrow(ex)), function(i) {
        row    <- ex[i, ]
        sev    <- row$severity %||% "low"
        col    <- .SEV_COLOR[[sev]] %||% "#64748b"
        ico    <- .SEV_ICON[[sev]]  %||% "circle-info"
        dom    <- row$domain %||% ""
        dom_c  <- if (!is.na(dom) && nzchar(dom)) {
          .DOMAIN_COLOR[[dom]] %||% "#64748b"
        } else "#64748b"
        win    <- row$window_label %||% ""

        shiny::div(
          style = paste0(
            "border-left:4px solid ", col, ";",
            "background:#fafafa; border-radius:4px;",
            "padding:10px 14px; margin-bottom:8px;"
          ),
          shiny::fluidRow(
            shiny::column(9,
              shiny::tags$span(
                style = paste0("color:", col, "; font-weight:700; font-size:0.9em;"),
                shiny::icon(ico), " ", row$explanation_label
              )
            ),
            shiny::column(3,
              shiny::tags$span(
                style = paste0("background:", col, "; color:#fff;",
                               "border-radius:10px; padding:2px 8px;",
                               "font-size:0.72em; float:right;"),
                toupper(sev)
              )
            )
          ),
          shiny::tags$p(
            style = "font-size:0.84em; color:#475569; margin:6px 0 4px;",
            row$explanation_detail
          ),
          if (!is.na(dom) && nzchar(dom)) {
            shiny::tags$span(
              style = paste0("background:", dom_c,
                             "22; color:", dom_c,
                             "; border:1px solid ", dom_c,
                             "44; border-radius:10px;",
                             "padding:1px 7px; font-size:0.72em;",
                             "margin-right:4px;"),
              dom
            )
          },
          if (!is.na(win) && nzchar(win)) {
            shiny::tags$span(
              style = paste0("background:#f1f5f9; border:1px solid #cbd5e1;",
                             "border-radius:10px; padding:1px 7px;",
                             "font-size:0.72em;"),
              win
            )
          }
        )
      })

      shiny::tagList(
        shiny::div(
          style = paste0("font-weight:700; font-size:0.88em; color:#0f3460;",
                         "text-transform:uppercase; letter-spacing:0.06em;",
                         "margin-bottom:10px;"),
          shiny::icon("magnifying-glass"),
          sprintf(" Why patient %s was selected (top %d signals)", pid, nrow(ex))
        ),
        shiny::tagList(cards),
        shiny::tags$p(
          style = paste0("font-size:0.75em; color:#94a3b8;",
                         "font-style:italic; margin-top:8px;"),
          "Signals are hypothesis-generating. Use 'may indicate' and ",
          "'requires review' language. Not a clinical conclusion."
        )
      )
    })
  })
}
