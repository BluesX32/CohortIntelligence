# trajectory.R
# Per-patient clinical timeline (swim-lane) plots for CohortIntelligence.
# Distinct from TrajectoryDashboard's phase-abstracted trajectories --
# these show raw OMOP event timelines for individual patient review.

DOMAIN_COLORS_TRAJ <- c(
  condition   = "#D73027",
  drug        = "#313695",
  procedure   = "#1A9641",
  measurement = "#6A3D9A",
  observation = "#B35806",
  visit       = "#41B6C4",
  death       = "#252525"
)

#' Build a swim-lane timeline tibble for a single patient
#'
#' @param subject_id Integer. The patient to build a timeline for.
#' @param domain_data Named list from [extract_omop_domains()].
#' @param cohort_members tibble(subject_id, cohort_start_date).
#' @param domains Character vector of domains to include.
#' @param top_n Integer. Top N most frequent concepts per domain to include.
#'   Use `Inf` for all concepts.
#'
#' @return tibble(subject_id, domain, concept_name, event_date,
#'   value_as_number, days_from_index, domain_rank) or `NULL` if patient
#'   not found.
#' @export
build_patient_timeline <- function(subject_id,
                                    domain_data,
                                    cohort_members,
                                    domains = c("condition","drug","procedure",
                                                "measurement","visit","observation"),
                                    top_n   = 10L) {
  subject_id <- as.integer(subject_id)

  idx_row <- cohort_members[cohort_members$subject_id == subject_id, , drop = FALSE]
  if (nrow(idx_row) == 0L) {
    message("Patient ", subject_id, " not found in cohort_members.")
    return(NULL)
  }
  index_date <- idx_row$cohort_start_date[[1L]]

  domain_spec <- list(
    condition   = list(date_col = "condition_start_date",   name_col = "condition_name",   val_col = NULL),
    drug        = list(date_col = "drug_exposure_start_date", name_col = "drug_name",       val_col = NULL),
    procedure   = list(date_col = "procedure_date",         name_col = "procedure_name",   val_col = NULL),
    measurement = list(date_col = "measurement_date",       name_col = "measurement_name", val_col = "value_as_number"),
    observation = list(date_col = "observation_date",       name_col = "observation_name", val_col = "value_as_number"),
    visit       = list(date_col = "visit_start_date",       name_col = "visit_type",       val_col = NULL)
  )

  active_domains <- intersect(domains, names(domain_spec))

  purrr::map_dfr(active_domains, function(d) {
    spec <- domain_spec[[d]]
    df   <- domain_data[[d]]
    if (is.null(df) || nrow(df) == 0L) return(tibble::tibble())

    df <- df[df$person_id == subject_id, , drop = FALSE]
    if (nrow(df) == 0L) return(tibble::tibble())

    if (!spec$date_col %in% names(df)) return(tibble::tibble())
    if (!spec$name_col %in% names(df)) df[[spec$name_col]] <- paste0(d, "_unknown")

    # Keep only top_n concepts
    if (is.finite(top_n)) {
      top_concepts <- df |>
        dplyr::count(.data[[spec$name_col]], sort = TRUE) |>
        dplyr::slice_head(n = as.integer(top_n)) |>
        dplyr::pull(spec$name_col)
      df <- df[df[[spec$name_col]] %in% top_concepts, , drop = FALSE]
    }

    tibble::tibble(
      subject_id     = subject_id,
      domain         = d,
      concept_name   = as.character(df[[spec$name_col]]),
      event_date     = df[[spec$date_col]],
      value_as_number = if (!is.null(spec$val_col) && spec$val_col %in% names(df))
                          as.numeric(df[[spec$val_col]])
                        else NA_real_,
      days_from_index = as.integer(df[[spec$date_col]] - index_date),
      domain_rank     = match(d, active_domains)
    )
  })
}

#' Plot a patient clinical timeline
#'
#' @param timeline_df tibble from [build_patient_timeline()].
#' @param interactive Logical. `TRUE` returns a plotly widget; `FALSE` returns
#'   a ggplot2 object.
#' @param color_by `"domain"` or `"concept"`. Colour aesthetic.
#' @param show_index Logical. Draw a vertical line at day 0 (index date).
#' @param date_range Optional `c(days_start, days_end)` relative to index date.
#'
#' @return A plotly widget or ggplot2 object.
#' @export
plot_patient_timeline <- function(timeline_df,
                                   interactive = TRUE,
                                   color_by    = c("domain","concept"),
                                   show_index  = TRUE,
                                   date_range  = NULL) {
  color_by <- match.arg(color_by)

  if (is.null(timeline_df) || nrow(timeline_df) == 0L) {
    if (interactive && requireNamespace("plotly", quietly = TRUE)) {
      return(plotly::plot_ly() |>
        plotly::add_annotations(text = "No data for this patient.",
                                showarrow = FALSE, x = 0.5, y = 0.5, xref = "paper", yref = "paper"))
    }
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0, y = 0, label = "No data for this patient.") +
             ggplot2::theme_void())
  }

  df <- timeline_df
  if (!is.null(date_range)) {
    df <- df[df$days_from_index >= date_range[[1]] &
               df$days_from_index <= date_range[[2]], , drop = FALSE]
  }

  color_var  <- if (color_by == "domain") df$domain else df$concept_name
  color_vals <- if (color_by == "domain") DOMAIN_COLORS_TRAJ else NULL

  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      rlang::abort("Package 'plotly' is required for interactive trajectory plots.")
    }
    p <- plotly::plot_ly(
      data      = df,
      x         = ~days_from_index,
      y         = ~concept_name,
      color     = ~domain,
      colors    = DOMAIN_COLORS_TRAJ,
      type      = "scatter",
      mode      = "markers",
      marker    = list(size = 8, opacity = 0.8),
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Day: %{x}<br>",
        "Date: %{customdata}<extra></extra>"
      ),
      customdata = format(df$event_date, "%Y-%m-%d"),
      source    = "trajectory"
    )
    if (show_index) {
      p <- plotly::layout(p, shapes = list(list(
        type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1,
        yref = "paper", line = list(color = "black", dash = "dash", width = 1.5)
      )))
    }
    plotly::layout(p,
      xaxis = list(title = "Days from index date"),
      yaxis = list(title = ""),
      legend = list(title = list(text = "Domain")),
      title  = paste0("Patient ", unique(df$subject_id)[[1]], " -- Clinical Timeline")
    )
  } else {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      rlang::abort("Package 'ggplot2' is required for static trajectory plots.")
    }
    p <- ggplot2::ggplot(df, ggplot2::aes(
      x     = days_from_index,
      y     = reorder(concept_name, domain_rank),
      color = domain
    )) +
      ggplot2::geom_point(size = 2.5, alpha = 0.8) +
      ggplot2::scale_color_manual(values = DOMAIN_COLORS_TRAJ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::labs(
        x     = "Days from index date",
        y     = NULL,
        color = "Domain",
        title = paste0("Patient ", unique(df$subject_id)[[1]], " -- Clinical Timeline")
      )
    if (show_index) {
      p <- p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black")
    }
    p
  }
}
