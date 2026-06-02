# explain.R
# Patient priority explanation -- "why is this patient high priority?"
# Returns typed, severity-tagged explanation rows for a single patient.

#' Explain why a patient received a high review priority
#'
#' Compares the selected patient's clinical features against the cohort
#' distribution and returns up to `top_n` explanation rows, each describing
#' one reason the patient stands out.
#'
#' All language is cautious: explanations use "may indicate", "requires
#' review", and "possible" to avoid overclaiming.
#'
#' @param subject_id Integer. The patient to explain.
#' @param rank_df tibble from [rank_patients()].
#' @param feature_matrix List from [build_feature_matrix()] (`$long` used).
#' @param domain_activity tibble from [build_domain_activity()].
#' @param cohort_members tibble(subject_id, cohort_start_date).
#' @param ml_results List from [run_full_ml_pipeline()]. `NULL` skips ML
#'   explanations.
#' @param top_n Integer. Maximum explanation rows to return. Default `8L`.
#'
#' @return tibble with columns: subject_id, explanation_type,
#'   explanation_label, explanation_detail, domain, window_label,
#'   importance_score, severity.
#' @export
explain_patient_priority <- function(subject_id,
                                      rank_df,
                                      feature_matrix,
                                      domain_activity,
                                      cohort_members,
                                      ml_results = NULL,
                                      top_n      = 8L) {
  subject_id <- as.integer(subject_id)
  rows <- list()

  pat_rank <- rank_df[rank_df$subject_id == subject_id, , drop = FALSE]
  if (nrow(pat_rank) == 0L) return(.empty_explanation(subject_id))

  # ── 1. Anomaly score vs cohort distribution ──────────────────────────────
  if (!is.na(pat_rank$anomaly_score) && any(rank_df$anomaly_score > 0)) {
    score    <- pat_rank$anomaly_score
    pct_rank <- mean(rank_df$anomaly_score <= score, na.rm = TRUE)
    # Clamp to avoid "top 0%" or "top 100%" due to rounding or ties.
    pct_top  <- max(1, round(100 * (1 - pct_rank), 0))
    sev      <- if (score > 0.7) "high" else if (score > 0.4) "medium" else "low"

    # Build a human-readable label that avoids misleading precision.
    label <- if (pct_top <= 5) {
      sprintf("High relative anomaly score (%.2f) -- among highest-scoring in cohort",
               score)
    } else if (pct_top <= 20) {
      sprintf("Elevated anomaly score (%.2f) -- above ~%d%% of cohort",
               score, 100 - pct_top)
    } else {
      sprintf("Anomaly score %.2f -- moderately unusual relative to cohort", score)
    }

    rows[["anomaly"]] <- tibble::tibble(
      subject_id        = subject_id,
      explanation_type  = "anomaly_score",
      explanation_label = label,
      explanation_detail = paste0(
        "This patient's structured OMOP feature pattern is less typical relative ",
        "to the current cohort (score: ", round(score, 2), "). ",
        "This threshold is a heuristic review cutoff, not a calibrated probability. ",
        "Possible causes include rare disease sub-patterns, data coding differences, ",
        "short observation periods, or genuinely atypical clinical trajectories. ",
        "Requires clinical review to interpret."
      ),
      domain            = NA_character_,
      window_label      = NA_character_,
      importance_score  = score,
      severity          = sev
    )
  }

  # ── 2. Sparsity vs cohort median ─────────────────────────────────────────
  if (!is.na(pat_rank$sparsity_score)) {
    sp      <- pat_rank$sparsity_score
    med_sp  <- median(rank_df$sparsity_score, na.rm = TRUE)
    if (sp > 0.5 || sp > med_sp * 1.5) {
      sev <- if (sp > 0.8) "high" else if (sp > 0.6) "medium" else "low"
      rows[["sparsity"]] <- tibble::tibble(
        subject_id        = subject_id,
        explanation_type  = "sparsity",
        explanation_label = sprintf("Sparse data (%.0f%% of domain-windows empty)",
                                     100 * sp),
        explanation_detail = paste0(
          "This patient has limited data across most OMOP domain-time-window cells. ",
          "Sparsity score ", round(sp, 2), " vs cohort median ",
          round(med_sp, 2), ". ",
          "May indicate incomplete follow-up, care received outside the observed ",
          "system, or short observation period. Requires review before drawing ",
          "clinical conclusions."
        ),
        domain            = NA_character_,
        window_label      = NA_character_,
        importance_score  = sp,
        severity          = sev
      )
    }
  }

  # ── 3. Cluster noise / unassigned ────────────────────────────────────────
  cid <- pat_rank$cluster_id
  if (!is.na(cid) && cid %in% c(-1L, 0L)) {
    rows[["cluster_noise"]] <- tibble::tibble(
      subject_id        = subject_id,
      explanation_type  = "cluster_noise",
      explanation_label = "Could not be assigned to any cluster",
      explanation_detail = paste0(
        "The ML clustering algorithm could not assign this patient to a ",
        "recognisable group. Their clinical history may not match any of the ",
        "dominant patterns in the cohort. Possible interpretations: rare ",
        "disease subtype, atypical care pathway, data quality issue, or ",
        "genuinely complex patient. Requires clinical review."
      ),
      domain            = NA_character_,
      window_label      = NA_character_,
      importance_score  = 0.7,
      severity          = "medium"
    )
  }

  # ── 4. High-count domain-window cells vs cohort median ───────────────────
  if (!is.null(domain_activity) && nrow(domain_activity) > 0L) {
    pat_act <- dplyr::filter(domain_activity, subject_id == !!subject_id)
    if (nrow(pat_act) > 0L) {
      cohort_medians <- domain_activity |>
        dplyr::group_by(domain, window_label) |>
        dplyr::summarise(med_count = median(event_count, na.rm = TRUE),
                          sd_count  = stats::sd(event_count, na.rm = TRUE),
                          .groups   = "drop")

      enriched <- dplyr::left_join(pat_act, cohort_medians,
                                    by = c("domain","window_label")) |>
        dplyr::filter(event_count > 0, med_count > 0) |>
        dplyr::mutate(
          z_score  = (event_count - med_count) /
                       pmax(sd_count, 0.01),
          ratio    = event_count / pmax(med_count, 0.01)
        ) |>
        dplyr::filter(z_score > 1.5) |>
        dplyr::arrange(dplyr::desc(z_score)) |>
        dplyr::slice_head(n = 3L)

      for (i in seq_len(nrow(enriched))) {
        r   <- enriched[i, ]
        sev <- if (r$z_score > 3) "high" else if (r$z_score > 2) "medium" else "low"
        key <- paste0("high_", r$domain, "_", gsub("[^a-z0-9]","",
                                                     tolower(r$window_label)))
        rows[[key]] <- tibble::tibble(
          subject_id        = subject_id,
          explanation_type  = "high_domain_activity",
          explanation_label = sprintf(
            "High %s activity in %s (%.1f times cohort median)",
            r$domain, r$window_label, r$ratio
          ),
          explanation_detail = paste0(
            "This patient had ", r$event_count, " ", r$domain,
            " events in the ", r$window_label, " window, ",
            "compared to a cohort median of ", round(r$med_count, 1),
            " (z-score: ", round(r$z_score, 1), "). ",
            "Possible explanations: intensive clinical management, ",
            "comorbidity burden, or high documentation frequency. ",
            "Requires clinical context to interpret."
          ),
          domain            = r$domain,
          window_label      = r$window_label,
          importance_score  = pmin(r$z_score / 5, 1),
          severity          = sev
        )
      }
    }
  }

  # ── 5. Temporal concentration ────────────────────────────────────────────
  if (!is.null(domain_activity) && nrow(domain_activity) > 0L) {
    pat_act <- dplyr::filter(domain_activity, subject_id == !!subject_id)
    total   <- sum(pat_act$event_count)
    if (total > 5L) {
      by_win <- pat_act |>
        dplyr::group_by(window_label) |>
        dplyr::summarise(win_total = sum(event_count), .groups = "drop") |>
        dplyr::mutate(pct = win_total / total)
      max_win <- by_win[which.max(by_win$pct), , drop = FALSE]
      if (max_win$pct > 0.6) {
        rows[["temporal_concentration"]] <- tibble::tibble(
          subject_id        = subject_id,
          explanation_type  = "temporal_concentration",
          explanation_label = sprintf(
            "Activity concentrated in %s window (%.0f%% of all events)",
            max_win$window_label, 100 * max_win$pct
          ),
          explanation_detail = paste0(
            round(100 * max_win$pct, 0),
            "% of this patient's clinical events occur in the ",
            max_win$window_label, " window. ",
            "This may reflect a discrete clinical episode, a data capture ",
            "window, or an abrupt change in care intensity. ",
            "Reviewing the Trajectory Viewer for this window is recommended."
          ),
          domain            = NA_character_,
          window_label      = max_win$window_label,
          importance_score  = max_win$pct,
          severity          = if (max_win$pct > 0.8) "medium" else "low"
        )
      }
    }
  }

  if (length(rows) == 0L) return(.empty_explanation(subject_id))

  result <- dplyr::bind_rows(rows) |>
    dplyr::arrange(dplyr::desc(importance_score)) |>
    dplyr::slice_head(n = as.integer(top_n))

  result
}

.empty_explanation <- function(subject_id) {
  tibble::tibble(
    subject_id        = as.integer(subject_id),
    explanation_type  = character(0),
    explanation_label = character(0),
    explanation_detail = character(0),
    domain            = character(0),
    window_label      = character(0),
    importance_score  = numeric(0),
    severity          = character(0)
  )
}
