# temporal_flags.R
# Rule-based temporal flag detection -- complements ML-based anomaly scoring.
# Flags are review triggers, not clinical conclusions.

#' Detect structured-data temporal patterns that warrant clinical review
#'
#' Applies ten OMOP-generic rules to identify patients with potentially
#' unusual temporal patterns in their clinical record. All flags are
#' hypothesis-generating. Language uses "potential", "may require review",
#' and "structured evidence is limited."
#'
#' @param cohort_members tibble(subject_id, cohort_start_date, cohort_end_date).
#' @param domain_data Named list from [extract_omop_domains()].
#' @param feature_matrix List from [build_feature_matrix()]. `NULL` = skip
#'   feature-based flags.
#' @param time_windows tibble from [define_time_windows()].
#' @param config A list from [temporal_flag_config()]. `NULL` = defaults.
#'
#' @return tibble(subject_id, flag_type, flag_label, flag_description,
#'   severity, domain, event_date, days_from_index,
#'   evidence_summary, recommended_action)
#' @export
detect_temporal_flags <- function(cohort_members,
                                   domain_data,
                                   feature_matrix = NULL,
                                   time_windows   = define_time_windows(),
                                   config         = NULL) {
  if (is.null(config)) config <- temporal_flag_config()

  flags <- list()

  # Helper: create a flag row
  .flag <- function(subject_id, flag_type, flag_label, flag_description,
                     severity, domain = NA_character_,
                     event_date = NA_character_,
                     days_from_index = NA_integer_,
                     evidence_summary, recommended_action) {
    tibble::tibble(
      subject_id       = as.integer(subject_id),
      flag_type        = flag_type,
      flag_label       = flag_label,
      flag_description = flag_description,
      severity         = severity,
      domain           = domain,
      event_date       = as.character(event_date),
      days_from_index  = as.integer(days_from_index),
      evidence_summary = evidence_summary,
      recommended_action = recommended_action
    )
  }

  # Pre-compute index dates
  idx <- dplyr::select(cohort_members, subject_id, cohort_start_date, cohort_end_date)

  # ── Flag 1: No post-index follow-up ──────────────────────────────────────
  post_domains <- c("condition","drug","procedure","measurement","observation","visit")
  post_events <- purrr::map_dfr(
    intersect(post_domains, names(domain_data)),
    function(d) {
      df <- domain_data[[d]]
      date_col <- switch(d,
        condition   = "condition_start_date",
        drug        = "drug_exposure_start_date",
        procedure   = "procedure_date",
        measurement = "measurement_date",
        observation = "observation_date",
        visit       = "visit_start_date"
      )
      if (is.null(df) || nrow(df) == 0L || !date_col %in% names(df)) {
        return(tibble::tibble(subject_id = integer(0)))
      }
      df |>
        dplyr::rename(subject_id = person_id) |>
        dplyr::inner_join(idx, by = "subject_id") |>
        dplyr::filter(.data[[date_col]] > cohort_start_date) |>
        dplyr::select(subject_id) |>
        dplyr::distinct() |>
        dplyr::mutate(has_post = TRUE)
    }
  ) |>
    dplyr::distinct()

  no_post_ids <- setdiff(cohort_members$subject_id,
                          post_events$subject_id %||% integer(0))
  if (length(no_post_ids) > 0L) {
    flags[["no_post_followup"]] <- purrr::map_dfr(no_post_ids, function(pid) {
      .flag(pid, "no_post_index_followup",
            "No observed post-index follow-up (data completeness concern)",
            paste0("No clinical events found after the cohort index date in any ",
                   "OMOP domain. Possible explanations: the patient left the ",
                   "observed system, care was received outside this data source, ",
                   "index date falls near the end of available data, or there is ",
                   "a data feed lag. This is a data completeness flag, not a ",
                   "clinical error."),
            severity         = "medium",
            domain           = "all",
            evidence_summary = "Zero post-index events across all available domains.",
            recommended_action = paste0(
              "Review observation period end date, data source coverage, and ",
              "whether index date occurs near the end of available data."))
    })
  }

  # ── Flag 2: Sparse post-index activity ───────────────────────────────────
  if (!is.null(domain_data)) {
    post_counts <- purrr::map_dfr(
      intersect(post_domains, names(domain_data)),
      function(d) {
        df <- domain_data[[d]]
        date_col <- switch(d,
          condition="condition_start_date", drug="drug_exposure_start_date",
          procedure="procedure_date", measurement="measurement_date",
          observation="observation_date", visit="visit_start_date")
        if (is.null(df) || nrow(df) == 0L || !date_col %in% names(df)) {
          return(tibble::tibble(subject_id = integer(0), n_post = integer(0)))
        }
        df |>
          dplyr::rename(subject_id = person_id) |>
          dplyr::inner_join(idx, by = "subject_id") |>
          dplyr::filter(.data[[date_col]] > cohort_start_date) |>
          dplyr::count(subject_id, name = "n_post")
      }
    ) |>
      dplyr::group_by(subject_id) |>
      dplyr::summarise(n_post = sum(n_post), .groups = "drop")

    med_post <- median(post_counts$n_post, na.rm = TRUE)
    p20      <- quantile(post_counts$n_post, 0.2, na.rm = TRUE)
    sparse_post <- post_counts |>
      dplyr::filter(n_post <= p20, n_post > 0L) |>
      dplyr::pull(subject_id)

    if (length(sparse_post) > 0L) {
      n_vals <- setNames(post_counts$n_post, post_counts$subject_id)
      flags[["sparse_post"]] <- purrr::map_dfr(sparse_post, function(pid) {
        .flag(pid, "sparse_post_index",
              "Sparse post-index clinical activity",
              paste0("Post-index event count (", n_vals[[as.character(pid)]],
                     ") is in the bottom 20% of the cohort. May reflect ",
                     "incomplete follow-up, transfer of care, or early ",
                     "cohort exit."),
              severity         = "medium",
              domain           = "multiple",
              evidence_summary = sprintf("%d post-index events vs cohort median %.0f",
                                         n_vals[[as.character(pid)]], med_post),
              recommended_action = "Review patient trajectory for follow-up completeness.")
      })
    }
  }

  # ── Flag 3: Death shortly after index ────────────────────────────────────
  death_df <- domain_data[["death"]]
  if (!is.null(death_df) && nrow(death_df) > 0L) {
    early_deaths <- death_df |>
      dplyr::rename(subject_id = person_id) |>
      dplyr::inner_join(idx, by = "subject_id") |>
      dplyr::mutate(days = as.integer(death_date - cohort_start_date)) |>
      dplyr::filter(days >= 0L, days <= config$death_window_days)

    if (nrow(early_deaths) > 0L) {
      flags[["early_death"]] <- purrr::map_dfr(seq_len(nrow(early_deaths)), function(i) {
        r <- early_deaths[i, ]
        .flag(r$subject_id, "death_shortly_after_index",
              sprintf("Death recorded %d days after index", r$days),
              paste0("A death event was recorded ", r$days,
                     " days after the cohort index date. This may affect ",
                     "cohort validity, follow-up completeness, or outcome ",
                     "interpretation."),
              severity        = "high",
              domain          = "death",
              event_date      = as.character(r$death_date),
              days_from_index = r$days,
              evidence_summary = sprintf("death_date = %s (%d days post-index)",
                                          r$death_date, r$days),
              recommended_action = "Review cohort definition and censoring rules.")
      })
    }
  }

  # ── Flag 4: Recurrent same-concept condition events ───────────────────────
  cond_df <- domain_data[["condition"]]
  if (!is.null(cond_df) && nrow(cond_df) > 0L &&
      "condition_concept_id" %in% names(cond_df)) {
    recurrent <- cond_df |>
      dplyr::rename(subject_id = person_id) |>
      dplyr::inner_join(idx, by = "subject_id") |>
      dplyr::arrange(subject_id, condition_concept_id, condition_start_date) |>
      dplyr::group_by(subject_id, condition_concept_id) |>
      dplyr::mutate(
        gap = as.integer(condition_start_date -
                           dplyr::lag(condition_start_date, 1))
      ) |>
      dplyr::filter(!is.na(gap), gap <= config$recurrent_gap_days) |>
      dplyr::summarise(n_recurrent = dplyr::n(), .groups = "drop") |>
      dplyr::filter(n_recurrent >= 2L) |>
      dplyr::distinct(subject_id) |>
      dplyr::pull(subject_id)

    if (length(recurrent) > 0L) {
      flags[["recurrent_cond"]] <- purrr::map_dfr(recurrent, function(pid) {
        .flag(pid, "recurrent_condition",
              "Recurrent condition events within short interval",
              paste0("Same condition concept coded multiple times within ",
                     config$recurrent_gap_days, " days. May reflect normal ",
                     "clinical re-coding, a recurrent episode, or a data ",
                     "quality pattern."),
              severity         = "low",
              domain           = "condition",
              evidence_summary = sprintf(">= 2 events for same concept within %d days",
                                          config$recurrent_gap_days),
              recommended_action = "Review condition occurrence dates in Trajectory Viewer.")
      })
    }
  }

  # ── Flag 5: Drug activity pre-index, absent post-index ────────────────────
  drug_df <- domain_data[["drug"]]
  if (!is.null(drug_df) && nrow(drug_df) > 0L) {
    drug_pre <- drug_df |>
      dplyr::rename(subject_id = person_id) |>
      dplyr::inner_join(idx, by = "subject_id") |>
      dplyr::filter(drug_exposure_start_date < cohort_start_date) |>
      dplyr::distinct(subject_id)

    drug_post <- drug_df |>
      dplyr::rename(subject_id = person_id) |>
      dplyr::inner_join(idx, by = "subject_id") |>
      dplyr::filter(drug_exposure_start_date >= cohort_start_date) |>
      dplyr::distinct(subject_id)

    drug_pre_no_post <- dplyr::setdiff(drug_pre, drug_post) |>
      dplyr::pull(subject_id)

    if (length(drug_pre_no_post) > 0L) {
      flags[["drug_dropout"]] <- purrr::map_dfr(drug_pre_no_post, function(pid) {
        .flag(pid, "drug_before_diagnosis_absent_after",
              "Drug exposure pre-index but absent post-index",
              paste0("Drug exposure was recorded before the cohort index date ",
                     "but not after. May indicate treatment discontinuation, ",
                     "loss to follow-up, or a change in prescribing practice."),
              severity         = "medium",
              domain           = "drug",
              evidence_summary = "Drug events in pre-index period only.",
              recommended_action = "Review drug exposure timeline in Trajectory Viewer.")
      })
    }
  }

  # ── Flag 6: Isolated index code (single condition, no post-index support) ──
  if (!is.null(cond_df) && nrow(cond_df) > 0L) {
    cond_counts <- cond_df |>
      dplyr::rename(subject_id = person_id) |>
      dplyr::count(subject_id, name = "n_cond")

    cond_post_support <- cond_df |>
      dplyr::rename(subject_id = person_id) |>
      dplyr::inner_join(idx, by = "subject_id") |>
      dplyr::filter(condition_start_date > cohort_start_date) |>
      dplyr::distinct(subject_id) |>
      dplyr::pull(subject_id)

    isolated_ids <- cond_counts |>
      dplyr::filter(n_cond == 1L) |>
      dplyr::filter(!subject_id %in% cond_post_support) |>
      dplyr::pull(subject_id)

    if (length(isolated_ids) > 0L) {
      flags[["isolated_code"]] <- purrr::map_dfr(isolated_ids, function(pid) {
        .flag(pid, "isolated_index_code",
              "Isolated index event, no post-index condition support",
              paste0("Only one condition occurrence recorded, at or before the ",
                     "index date, with no subsequent condition coding. May ",
                     "reflect a single administrative code entry, limited ",
                     "follow-up, or coding practice variation."),
              severity         = "medium",
              domain           = "condition",
              evidence_summary = "Single condition code; no post-index condition events.",
              recommended_action = "Review cohort phenotype validity for this patient.")
      })
    }
  }

  # ── Flag 7: Concentrated activity in one window ───────────────────────────
  if (!is.null(domain_data)) {
    all_events <- purrr::map_dfr(
      intersect(c("condition","drug","visit"), names(domain_data)),
      function(d) {
        df <- domain_data[[d]]
        date_col <- switch(d, condition="condition_start_date",
                            drug="drug_exposure_start_date",
                            visit="visit_start_date")
        if (is.null(df) || nrow(df) == 0L || !date_col %in% names(df)) return(tibble::tibble())
        df |>
          dplyr::rename(subject_id = person_id) |>
          dplyr::inner_join(idx, by = "subject_id") |>
          dplyr::mutate(days_from_index = as.integer(.data[[date_col]] - cohort_start_date)) |>
          dplyr::select(subject_id, days_from_index)
      }
    )

    if (nrow(all_events) > 0L) {
      conc <- all_events |>
        dplyr::mutate(
          window = cut(days_from_index,
                        breaks = c(-Inf, -365, -180, -90, 0, 90, 180, 365, Inf),
                        labels = FALSE)
        ) |>
        dplyr::count(subject_id, window) |>
        dplyr::group_by(subject_id) |>
        dplyr::mutate(
          total = sum(n),
          pct   = n / total
        ) |>
        dplyr::filter(total > 5L, pct > 0.7) |>
        dplyr::slice_head(n = 1L) |>
        dplyr::ungroup()

      if (nrow(conc) > 0L) {
        flags[["concentrated"]] <- purrr::map_dfr(seq_len(nrow(conc)), function(i) {
          r <- conc[i, ]
          .flag(r$subject_id, "concentrated_activity",
                sprintf("%.0f%% of events in one time window", 100 * r$pct),
                paste0(round(100 * r$pct, 0),
                       "% of this patient's events are concentrated in a single ",
                       "time window. May reflect a discrete clinical episode, ",
                       "abrupt care change, or narrow observation period."),
                severity         = "low",
                domain           = "multiple",
                evidence_summary = sprintf("%.0f%% events in one time window (n=%d total)",
                                            100 * r$pct, r$total),
                recommended_action = "Review Trajectory Viewer for temporal clustering.")
        })
      }
    }
  }

  if (length(flags) == 0L) {
    return(tibble::tibble(
      subject_id = integer(0), flag_type = character(0),
      flag_label = character(0), flag_description = character(0),
      severity = character(0), domain = character(0),
      event_date = character(0), days_from_index = integer(0),
      evidence_summary = character(0), recommended_action = character(0)
    ))
  }

  dplyr::bind_rows(flags)
}
