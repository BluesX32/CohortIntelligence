# rank.R
# Patient review prioritisation for CohortIntelligence.
# Combines anomaly score, cluster noise flag, and domain data sparsity
# into a composite review priority score.

#' Compute data sparsity score per patient
#'
#' Sparsity is the proportion of (domain x window) cells with zero events.
#' A patient with mostly empty windows receives a high sparsity score.
#'
#' @param domain_activity tibble from [build_domain_activity()].
#' @param time_windows tibble from [define_time_windows()].
#'
#' @return tibble(subject_id, sparsity_score) with scores in the range 0 to 1.
#' @export
compute_sparsity <- function(domain_activity, time_windows = define_time_windows()) {
  n_domains  <- length(unique(domain_activity$domain))
  n_windows  <- nrow(time_windows)
  total_cells <- n_domains * n_windows

  if (total_cells == 0L) {
    return(tibble::tibble(
      subject_id    = unique(domain_activity$subject_id),
      sparsity_score = 1
    ))
  }

  domain_activity |>
    dplyr::group_by(subject_id) |>
    dplyr::summarise(
      zero_cells    = sum(event_count == 0L, na.rm = TRUE),
      .groups       = "drop"
    ) |>
    dplyr::mutate(sparsity_score = pmin(zero_cells / total_cells, 1)) |>
    dplyr::select(subject_id, sparsity_score)
}

#' Rank patients by review priority
#'
#' Combines anomaly score (from isolation forest), cluster noise flag
#' (hdbscan noise points get a bonus), and data sparsity into a weighted
#' composite score. Patients are ranked descending (rank 1 = highest priority).
#'
#' @param ml_results List from [run_full_ml_pipeline()] -- uses `$merged`
#'   tibble (subject_id, cluster_id, anomaly_score). Can also be a plain
#'   tibble(subject_id, anomaly_score, cluster_id).
#' @param domain_activity tibble from [build_domain_activity()].
#' @param cohort_members tibble(subject_id, cohort_start_date).
#' @param weights Named list with elements `anomaly`, `cluster_noise`,
#'   `sparsity`. Values should sum to 1.
#' @param n_tiers Integer. Number of priority tiers to assign. Default 3.
#'
#' @return tibble(subject_id, rank_score, rank_position, priority_tier,
#'   anomaly_score, cluster_id, sparsity_score)
#' @export
rank_patients <- function(ml_results,
                           domain_activity,
                           cohort_members,
                           weights = list(anomaly = 0.50,
                                          cluster_noise = 0.25,
                                          sparsity = 0.25),
                           n_tiers = 3L) {
  # Unpack ml_results -- gracefully handle NULL (ML pipeline unavailable)
  if (is.null(ml_results)) {
    # Sparsity-only ranking when ML pipeline did not run
    message("[CI] ML results unavailable; ranking by data sparsity only.")
    all_subjects <- tibble::tibble(subject_id = cohort_members$subject_id)
    sparsity_df  <- compute_sparsity(domain_activity)
    result <- dplyr::left_join(all_subjects, sparsity_df, by = "subject_id") |>
      dplyr::mutate(
        sparsity_score = dplyr::coalesce(sparsity_score, 0),
        anomaly_score  = 0,
        cluster_id     = 0L,
        rank_score     = sparsity_score,
        rank_position  = rank(-rank_score, ties.method = "first")
      )
    n_tiers   <- max(1L, as.integer(n_tiers))
    tier_size <- ceiling(nrow(result) / n_tiers)
    tier_names <- c("high","medium","low","very low")[seq_len(n_tiers)]
    if (n_tiers > 4L) tier_names <- paste0("tier_", seq_len(n_tiers))
    result <- dplyr::mutate(result,
      priority_tier = tier_names[ceiling(rank_position / tier_size)]
    )
    return(dplyr::select(result, subject_id, rank_score, rank_position,
                          priority_tier, anomaly_score, cluster_id, sparsity_score))
  }
  if (is.list(ml_results) && "merged" %in% names(ml_results)) {
    ml_df <- ml_results$merged
  } else if (is.data.frame(ml_results)) {
    ml_df <- ml_results
  } else {
    rlang::abort("'ml_results' must be NULL, a list from run_full_ml_pipeline(), or a data frame.")
  }

  required_cols <- c("subject_id", "anomaly_score", "cluster_id")
  missing_cols  <- setdiff(required_cols, names(ml_df))
  if (length(missing_cols) > 0L) {
    # Provide placeholder columns if missing
    if (!"anomaly_score" %in% names(ml_df)) {
      ml_df$anomaly_score <- 0
    }
    if (!"cluster_id" %in% names(ml_df)) {
      ml_df$cluster_id <- 0L
    }
  }

  # Ensure all cohort members are present
  all_subjects <- tibble::tibble(subject_id = cohort_members$subject_id)
  ml_df <- dplyr::right_join(
    dplyr::select(ml_df, subject_id, anomaly_score, cluster_id),
    all_subjects,
    by = "subject_id"
  )
  ml_df <- dplyr::mutate(ml_df,
    anomaly_score = dplyr::coalesce(anomaly_score, 0),
    cluster_id    = dplyr::coalesce(cluster_id, 0L)
  )

  # Normalise anomaly score to [0, 1]
  a_min <- min(ml_df$anomaly_score, na.rm = TRUE)
  a_max <- max(ml_df$anomaly_score, na.rm = TRUE)
  if (a_max > a_min) {
    ml_df$anomaly_norm <- (ml_df$anomaly_score - a_min) / (a_max - a_min)
  } else {
    ml_df$anomaly_norm <- 0
  }

  # Cluster noise flag (hdbscan noise points = cluster_id -1 or 0)
  ml_df$noise_flag <- as.numeric(ml_df$cluster_id %in% c(-1L, 0L))

  # Sparsity
  sparsity_df <- compute_sparsity(domain_activity)
  ml_df <- dplyr::left_join(ml_df, sparsity_df, by = "subject_id")
  ml_df <- dplyr::mutate(ml_df,
    sparsity_score = dplyr::coalesce(sparsity_score, 0)
  )

  # Composite score
  w_a  <- weights$anomaly       %||% 0.50
  w_cn <- weights$cluster_noise %||% 0.25
  w_sp <- weights$sparsity      %||% 0.25

  ml_df <- dplyr::mutate(ml_df,
    rank_score = w_a * anomaly_norm + w_cn * noise_flag + w_sp * sparsity_score
  )

  # Rank (descending: highest score = rank 1)
  ml_df <- dplyr::mutate(ml_df,
    rank_position = rank(-rank_score, ties.method = "first")
  )

  # Tier assignment
  n_tiers   <- max(1L, as.integer(n_tiers))
  n_pat     <- nrow(ml_df)
  tier_size <- ceiling(n_pat / n_tiers)
  tier_names <- c("high","medium","low","very low")[seq_len(n_tiers)]
  if (n_tiers > 4L) tier_names <- paste0("tier_", seq_len(n_tiers))

  ml_df <- dplyr::mutate(ml_df,
    priority_tier = tier_names[ceiling(rank_position / tier_size)]
  )

  dplyr::select(ml_df,
    subject_id, rank_score, rank_position, priority_tier,
    anomaly_score, cluster_id, sparsity_score
  )
}
