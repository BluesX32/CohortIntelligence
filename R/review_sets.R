# review_sets.R
# Build guided review sets for the Review Queue panel.
# Eight clinically-motivated patient groups to structure chart review workflow.

#' Configure temporal flag detection thresholds
#'
#' @param exposure_domains Character vector. Domains treated as "exposures".
#' @param outcome_domains Character vector. Domains treated as "outcomes".
#' @param acute_visit_concepts Integer vector. OMOP concept IDs for acute care
#'   visit types. `NULL` = treat all visits as a combined measure.
#' @param recurrent_gap_days Integer. Max days between events to consider them
#'   recurrent. Default `30L`.
#' @param death_window_days Integer. Days after index that constitutes "death
#'   shortly after index". Default `90L`.
#' @param support_window_days Integer. Days to look before/after an event for
#'   supporting evidence. Default `30L`.
#'
#' @return A named list of configuration values.
#' @export
temporal_flag_config <- function(exposure_domains     = c("drug"),
                                  outcome_domains      = c("condition","visit","death"),
                                  acute_visit_concepts = NULL,
                                  recurrent_gap_days   = 30L,
                                  death_window_days    = 90L,
                                  support_window_days  = 30L) {
  list(
    exposure_domains     = exposure_domains,
    outcome_domains      = outcome_domains,
    acute_visit_concepts = acute_visit_concepts,
    recurrent_gap_days   = as.integer(recurrent_gap_days),
    death_window_days    = as.integer(death_window_days),
    support_window_days  = as.integer(support_window_days)
  )
}

#' Build guided patient review sets
#'
#' Creates eight clinically-motivated patient groups to guide structured
#' chart review. Sets are ordered from low-risk (calibration) to high-risk
#' (outlier / data-quality concern).
#'
#' @param rank_df tibble from [rank_patients()].
#' @param domain_activity tibble from [build_domain_activity()].
#' @param feature_matrix List from [build_feature_matrix()].
#' @param ml_results List from [run_full_ml_pipeline()]. `NULL` = ML-free sets.
#' @param cohort_members tibble(subject_id, cohort_start_date).
#' @param temporal_flags tibble from [detect_temporal_flags()]. `NULL` = skip
#'   the Temporal Concern set.
#' @param n_per_set Integer. Maximum patients per set. Default `10L`.
#'
#' @return tibble(review_set, subject_id, reason_for_selection, rank_score,
#'   rank_position, cluster_id, anomaly_score, sparsity_score, set_priority)
#' @export
build_review_sets <- function(rank_df,
                               domain_activity,
                               feature_matrix  = NULL,
                               ml_results      = NULL,
                               cohort_members,
                               temporal_flags  = NULL,
                               n_per_set       = 10L) {
  if (is.null(rank_df) || nrow(rank_df) == 0L) return(.empty_review_sets())
  n <- as.integer(n_per_set)

  base <- rank_df |>
    dplyr::select(subject_id, rank_score, rank_position, cluster_id,
                  anomaly_score, sparsity_score)

  # ── Pre-compute domain activity summaries per patient ─────────────────────
  if (!is.null(domain_activity) && nrow(domain_activity) > 0L) {
    windows <- sort(unique(domain_activity$window_label))
    n_win   <- length(windows)
    mid_idx <- ceiling(n_win / 2)
    pre_wins  <- windows[seq_len(mid_idx)]
    post_wins <- windows[seq(mid_idx + 1L, n_win)]

    act_summary <- domain_activity |>
      dplyr::group_by(subject_id) |>
      dplyr::summarise(
        total_events      = sum(event_count),
        post_index_events = sum(event_count[window_label %in% post_wins]),
        pre_index_events  = sum(event_count[window_label %in% pre_wins]),
        .groups           = "drop"
      )
    base <- dplyr::left_join(base, act_summary, by = "subject_id")
  } else {
    base <- dplyr::mutate(base,
      total_events = 0, post_index_events = 0, pre_index_events = 0)
  }

  cohort_medians <- list(
    post = median(base$post_index_events, na.rm = TRUE),
    pre  = median(base$pre_index_events,  na.rm = TRUE),
    anm  = median(base$anomaly_score,     na.rm = TRUE),
    sp   = median(base$sparsity_score,    na.rm = TRUE)
  )

  # ── Helper: format a set tibble ───────────────────────────────────────────
  .make_set <- function(ids, set_name, reasons, priority) {
    if (length(ids) == 0L) return(NULL)
    sub <- base[base$subject_id %in% ids, , drop = FALSE]
    sub$review_set           <- set_name
    sub$reason_for_selection <- reasons[match(sub$subject_id, ids)]
    sub$set_priority         <- priority
    dplyr::select(sub, review_set, subject_id, reason_for_selection,
                  rank_score, rank_position, cluster_id,
                  anomaly_score, sparsity_score, set_priority)
  }

  sets <- list()

  # Set 1 -- Typical patients (reference group)
  typical_ids <- base |>
    dplyr::filter(
      anomaly_score <= quantile(anomaly_score, 0.25, na.rm = TRUE),
      sparsity_score < 0.4
    ) |>
    dplyr::arrange(rank_position) |>
    dplyr::slice_tail(n = n) |>  # lowest rank_score = most typical
    dplyr::pull(subject_id)
  sets[[1]] <- .make_set(
    typical_ids, "Typical patients",
    rep("Low anomaly and adequate data -- use as calibration reference", length(typical_ids)),
    priority = 1L
  )

  # Set 2 -- Most anomalous
  anomalous_ids <- base |>
    dplyr::arrange(dplyr::desc(anomaly_score)) |>
    dplyr::slice_head(n = n) |>
    dplyr::pull(subject_id)
  sets[[2]] <- .make_set(
    anomalous_ids, "Most anomalous",
    sprintf("Anomaly score %.2f -- statistically unusual clinical pattern",
            base$anomaly_score[match(anomalous_ids, base$subject_id)]),
    priority = 2L
  )

  # Set 3 -- Sparse follow-up
  sparse_ids <- base |>
    dplyr::filter(sparsity_score > 0.6) |>
    dplyr::arrange(dplyr::desc(sparsity_score)) |>
    dplyr::slice_head(n = n) |>
    dplyr::pull(subject_id)
  sets[[3]] <- .make_set(
    sparse_ids, "Sparse follow-up",
    sprintf("Sparsity %.0f%% -- limited post-index data, possible incomplete follow-up",
            100 * base$sparsity_score[match(sparse_ids, base$subject_id)]),
    priority = 3L
  )

  # Set 4 -- Rare cluster (smallest cluster or noise)
  cluster_sizes <- table(base$cluster_id)
  small_clusters <- as.integer(names(cluster_sizes)[cluster_sizes <= 10L | names(cluster_sizes) %in% c("-1","0")])
  rare_ids <- base |>
    dplyr::filter(cluster_id %in% small_clusters) |>
    dplyr::arrange(dplyr::desc(anomaly_score)) |>
    dplyr::slice_head(n = n) |>
    dplyr::pull(subject_id)
  sets[[4]] <- .make_set(
    rare_ids, "Rare cluster",
    sprintf("Cluster %d has only %d patients -- possible rare phenotype",
            base$cluster_id[match(rare_ids, base$subject_id)],
            cluster_sizes[as.character(base$cluster_id[match(rare_ids, base$subject_id)])]),
    priority = 4L
  )

  # Set 5 -- High post-index activity
  hi_post_ids <- base |>
    dplyr::filter(post_index_events > cohort_medians$post * 2) |>
    dplyr::arrange(dplyr::desc(post_index_events)) |>
    dplyr::slice_head(n = n) |>
    dplyr::pull(subject_id)
  sets[[5]] <- .make_set(
    hi_post_ids, "High post-index activity",
    sprintf("%.0f post-index events (>2x cohort median) -- possible intensive management",
            base$post_index_events[match(hi_post_ids, base$subject_id)]),
    priority = 5L
  )

  # Set 6 -- High pre-index activity
  hi_pre_ids <- base |>
    dplyr::filter(pre_index_events > cohort_medians$pre * 2) |>
    dplyr::arrange(dplyr::desc(pre_index_events)) |>
    dplyr::slice_head(n = n) |>
    dplyr::pull(subject_id)
  sets[[6]] <- .make_set(
    hi_pre_ids, "High pre-index activity",
    sprintf("%.0f pre-index events (>2x cohort median) -- complex prior history",
            base$pre_index_events[match(hi_pre_ids, base$subject_id)]),
    priority = 6L
  )

  # Set 7 -- Boundary patients (mid-range anomaly, between clusters)
  boundary_ids <- base |>
    dplyr::filter(
      anomaly_score >= 0.35,
      anomaly_score <= 0.55,
      !cluster_id %in% c(-1L, 0L)
    ) |>
    dplyr::arrange(anomaly_score) |>
    dplyr::slice_head(n = n) |>
    dplyr::pull(subject_id)
  sets[[7]] <- .make_set(
    boundary_ids, "Boundary patients",
    "Moderate anomaly score -- may sit between two clinical patterns",
    priority = 7L
  )

  # Set 8 -- Temporal concern (requires temporal_flags input)
  if (!is.null(temporal_flags) && nrow(temporal_flags) > 0L) {
    hi_flag_ids <- temporal_flags |>
      dplyr::filter(severity == "high") |>
      dplyr::distinct(subject_id) |>
      dplyr::pull(subject_id)
    tc_ids <- hi_flag_ids[seq_len(min(n, length(hi_flag_ids)))]
    sets[[8]] <- .make_set(
      tc_ids, "Temporal concern",
      "High-severity temporal flag -- structured-data pattern requires review",
      priority = 8L
    )
  }

  result <- dplyr::bind_rows(sets)
  if (nrow(result) == 0L) return(.empty_review_sets())
  result
}

.empty_review_sets <- function() {
  tibble::tibble(
    review_set           = character(0),
    subject_id           = integer(0),
    reason_for_selection = character(0),
    rank_score           = numeric(0),
    rank_position        = integer(0),
    cluster_id           = integer(0),
    anomaly_score        = numeric(0),
    sparsity_score       = numeric(0),
    set_priority         = integer(0)
  )
}
