# demographics.R
# Shiny module: cohort demographics summary + data density calendar heatmap.

#' Demographics module UI
#' @param id Shiny module namespace ID.
#' @export
demographicsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # ── Row 1: summary cards ─────────────────────────────────────────────────
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("box_n"),      width = 3),
      shinydashboard::valueBoxOutput(ns("box_fu"),     width = 3),
      shinydashboard::valueBoxOutput(ns("box_age"),    width = 3),
      shinydashboard::valueBoxOutput(ns("box_death"),  width = 3)
    ),
    # ── Row 2: three demographic charts ──────────────────────────────────────
    shiny::fluidRow(
      shiny::column(4,
        shiny::h4("Age at Index Date"),
        plotly::plotlyOutput(ns("age_plot"), height = "300px")
      ),
      shiny::column(4,
        shiny::h4("Sex Distribution"),
        plotly::plotlyOutput(ns("sex_plot"), height = "300px")
      ),
      shiny::column(4,
        shiny::h4("Race / Ethnicity"),
        plotly::plotlyOutput(ns("race_plot"), height = "300px")
      )
    ),
    # ── Row 3: data density heatmap ──────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(12,
        shiny::h4("Data Density — Patients with Events per Domain per Month"),
        plotly::plotlyOutput(ns("density_plot"), height = "320px")
      )
    )
  )
}

#' Demographics module server
#' @param id Module ID.
#' @param cohort_members `reactive` returning tibble(subject_id,
#'   cohort_start_date, cohort_end_date).
#' @param domain_data `reactive` returning named list of domain tibbles.
#' @param person_data `reactive` returning tibble from
#'   [extract_person_demographics()].
#' @export
demographicsServer <- function(id, cohort_members, domain_data, person_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---------------------------------------------------------------------------
    # Reactive computations
    # IMPORTANT: none of these use shiny::req() -- they always return a valid
    # object so that every output can render even before the cohort loads.
    # Using req() would silently abort the reactive and leave outputs blank.
    # ---------------------------------------------------------------------------

    # Cohort member count (always an integer, 0 when not yet loaded)
    n_patients_rv <- shiny::reactive({
      cm <- cohort_members()
      if (is.null(cm) || nrow(cm) == 0L) return(0L)
      nrow(cm)
    })

    # Full summary -- always returns a valid list (never NULL, never aborts)
    summary_rv <- shiny::reactive({
      cm <- cohort_members()

      # Fallback list used when cohort is not yet loaded or summary fails
      fallback <- list(
        n_patients      = if (is.null(cm)) 0L else nrow(cm),
        median_followup = NA_real_,
        median_age      = NA_real_,
        pct_death       = NA_real_,
        age_df          = tibble::tibble(),
        sex_df          = tibble::tibble(gender_name = character(0),
                                          n           = integer(0)),
        race_df         = tibble::tibble(race_name = character(0),
                                          n         = integer(0))
      )

      if (is.null(cm) || nrow(cm) == 0L) return(fallback)

      tryCatch(
        build_cohort_summary(cm, person_data()),
        error = function(e) {
          message("[CI Demographics] build_cohort_summary failed: ",
                  conditionMessage(e))
          fallback
        }
      )
    })

    # Data density -- always returns a tibble (empty when not ready)
    density_rv <- shiny::reactive({
      cm <- cohort_members()
      dd <- domain_data()
      if (is.null(cm) || nrow(cm) == 0L || is.null(dd)) {
        return(tibble::tibble())
      }
      tryCatch(
        build_data_density(dd, cm),
        error = function(e) {
          message("[CI Demographics] build_data_density failed: ",
                  conditionMessage(e))
          tibble::tibble()
        }
      )
    })

    # ── Value boxes (all must render regardless of data state) ───────────────
    output$box_n <- shinydashboard::renderValueBox({
      n <- n_patients_rv()
      shinydashboard::valueBox(
        value    = if (n == 0L) "Loading..." else format(n, big.mark = ","),
        subtitle = "Patients in cohort",
        icon     = shiny::icon("users"),
        color    = if (n == 0L) "light-blue" else "blue"
      )
    })

    output$box_fu <- shinydashboard::renderValueBox({
      s   <- summary_rv()
      val <- tryCatch({
        fu <- s$median_followup
        if (!is.null(fu) && !is.na(fu) && is.finite(fu))
          paste0(round(fu / 30.4375, 1), " mo")
        else "N/A"
      }, error = function(e) "N/A")
      shinydashboard::valueBox(
        value    = val,
        subtitle = "Median follow-up",
        icon     = shiny::icon("calendar"),
        color    = "teal"
      )
    })

    output$box_age <- shinydashboard::renderValueBox({
      s   <- summary_rv()
      val <- tryCatch({
        a <- s$median_age
        if (!is.null(a) && !is.na(a) && is.finite(a)) round(a, 1) else "N/A"
      }, error = function(e) "N/A")
      shinydashboard::valueBox(
        value    = val,
        subtitle = "Median age at index",
        icon     = shiny::icon("user"),
        color    = "orange"
      )
    })

    output$box_death <- shinydashboard::renderValueBox({
      dd  <- domain_data()
      cm  <- cohort_members()
      val <- tryCatch({
        if (is.null(dd) || is.null(cm) || nrow(cm) == 0L) {
          "N/A"
        } else {
          death_df <- dd[["death"]]
          if (!is.null(death_df) && nrow(death_df) > 0L) {
            n_dead <- length(unique(
              death_df$person_id[death_df$person_id %in% cm$subject_id]
            ))
            paste0(round(100 * n_dead / nrow(cm), 1), "%")
          } else {
            "0%"
          }
        }
      }, error = function(e) "N/A")
      shinydashboard::valueBox(
        value    = val,
        subtitle = "% with death recorded",
        icon     = shiny::icon("heart-pulse"),
        color    = "red"
      )
    })

    # ── Age histogram ─────────────────────────────────────────────────────────
    output$age_plot <- plotly::renderPlotly({
      s <- summary_rv()
      if (is.null(s) || nrow(s$age_df) == 0L) {
        return(empty_plotly("Age data not available."))
      }
      plotly::plot_ly(
        x    = ~s$age_df$age_at_index,
        type = "histogram",
        xbins = list(size = 5),
        marker = list(color = "#2c7bb6", line = list(color = "#fff", width = 0.5))
      ) |>
        plotly::layout(
          xaxis = list(title = "Age (years)"),
          yaxis = list(title = "Patients"),
          bargap = 0.05
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── Sex pie ───────────────────────────────────────────────────────────────
    output$sex_plot <- plotly::renderPlotly({
      s <- summary_rv()
      if (is.null(s) || nrow(s$sex_df) == 0L) {
        return(empty_plotly("Sex data not available."))
      }
      plotly::plot_ly(
        labels = ~s$sex_df$gender_name,
        values = ~s$sex_df$n,
        type   = "pie",
        hole   = 0.4,
        marker = list(
          colors = c("#2c7bb6", "#d7534e", "#74c476", "#999", "#f6a623")
        ),
        textinfo = "label+percent"
      ) |>
        plotly::layout(showlegend = FALSE) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── Race horizontal bar ───────────────────────────────────────────────────
    output$race_plot <- plotly::renderPlotly({
      s <- summary_rv()
      if (is.null(s) || nrow(s$race_df) == 0L) {
        return(empty_plotly("Race/ethnicity data not available."))
      }
      df <- dplyr::arrange(s$race_df, n)
      plotly::plot_ly(
        x    = ~df$n,
        y    = ~reorder(df$race_name, df$n),
        type = "bar",
        orientation = "h",
        marker = list(color = "#74c476")
      ) |>
        plotly::layout(
          xaxis = list(title = "Patients"),
          yaxis = list(title = "")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── Data density heatmap ──────────────────────────────────────────────────
    output$density_plot <- plotly::renderPlotly({
      dd <- density_rv()
      if (nrow(dd) == 0L) {
        return(empty_plotly("No data density information available."))
      }

      domain_colors <- c(
        condition   = "#D73027",
        drug        = "#313695",
        procedure   = "#1A9641",
        measurement = "#6A3D9A",
        observation = "#B35806",
        visit       = "#41B6C4",
        death       = "#252525"
      )

      domains <- sort(unique(dd$domain))
      traces  <- lapply(seq_along(domains), function(i) {
        d_name  <- domains[[i]]
        df_d    <- dplyr::filter(dd, domain == d_name)
        hi_col  <- domain_colors[[d_name]] %||% "#333333"
        plotly::plot_ly(
          x    = ~df_d$calendar_month,
          y    = rep(d_name, nrow(df_d)),
          z    = ~df_d$n_patients,
          type = "heatmap",
          colorscale = list(c(0, "#FFFFFF"), c(1, hi_col)),
          showscale  = (i == 1L),
          colorbar   = list(title = "Patients"),
          hovertemplate = paste0(
            "<b>Domain:</b> ", d_name, "<br>",
            "<b>Month:</b> %{x|%Y-%m}<br>",
            "<b>Patients:</b> %{z}<extra></extra>"
          )
        )
      })

      p <- do.call(plotly::subplot, c(
        traces,
        list(nrows = 1L, shareY = TRUE, titleX = TRUE, margin = 0.02)
      ))

      plotly::layout(p,
        xaxis  = list(title = "Calendar month"),
        yaxis  = list(title = ""),
        margin = list(l = 100, r = 20, t = 30, b = 50),
        paper_bgcolor = "#FAFAFA"
      )
    })

    # Render eagerly even when the Demographics tab is not visible so that
    # outputs are populated the moment the cohort loads, not on first click.
    for (out_id in c("box_n","box_fu","box_age","box_death",
                      "age_plot","sex_plot","race_plot","density_plot")) {
      shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
    }
  })
}
