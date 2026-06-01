# features.R
# Feature engineering pipeline for CohortIntelligence.
#
# Key outputs:
#   define_time_windows()    -> tibble of time windows relative to index date
#   build_domain_activity()  -> long tibble: subject x domain x window x count
#   build_feature_matrix()   -> concept-level wide/long matrix for ML
#   build_quilt_data()       -> plot-ready tibble for the reactive quilt plot

# ---------------------------------------------------------------------------
# Time window definition
# ---------------------------------------------------------------------------

#' Define time windows relative to cohort index date
#'
#' @param breaks_months Numeric vector of month break points relative to the
#'   cohort index date (negative = before, positive = after). Default
#'   `c(-24, -18, -12, -6, 0, 6, 12)` produces 6 windows.
#' @param labels Optional character vector of window labels. Auto-generated
#'   from breaks if `NULL`.
#'
#' @return tibble(window_idx, window_label, months_start, months_end)
#' @export
define_time_windows <- function(breaks_months = c(-24, -18, -12, -6, 0, 6, 12),
                                 labels         = NULL) {
  if (length(breaks_months) < 2L) {
    rlang::abort("'breaks_months' must have at least 2 elements.")
  }
  breaks_months <- sort(unique(breaks_months))
  n_windows     <- length(breaks_months) - 1L

  if (is.null(labels)) {
    labels <- paste0(
      "[",
      formatC(breaks_months[-length(breaks_months)], format = "f", digits = 0),
      ",",
      formatC(breaks_months[-1L], format = "f", digits = 0),
      ")"
    )
  } else if (length(labels) != n_windows) {
    rlang::abort(sprintf("'labels' must have length %d (one per window).", n_windows))
  }

  tibble::tibble(
    window_idx   = seq_len(n_windows),
    window_label = labels,
    months_start = breaks_months[-length(breaks_months)],
    months_end   = breaks_months[-1L]
  )
}

# ---------------------------------------------------------------------------
# Domain activity (quilt input)
# ---------------------------------------------------------------------------

#' Build per-domain event counts aggregated across time windows
#'
#' Counts events per patient per domain per time window. This is the primary
#' input to [build_quilt_data()]. It does NOT break out by concept -- it gives
#' a bird's-eye summary of activity volume.
#'
#' @param cohort_members tibble(subject_id, cohort_start_date) from
#'   [extract_cohort_members()].
#' @param domain_data Named list from [extract_omop_domains()].
#' @param time_windows tibble from [define_time_windows()].
#'
#' @return tibble(subject_id, domain, window_label, window_idx, event_count)
#' @export
build_domain_activity <- function(cohort_members,
                                   domain_data,
                                   time_windows = define_time_windows()) {
  if (!all(c("subject_id", "cohort_start_date") %in% names(cohort_members))) {
    rlang::abort("'cohort_members' must have columns subject_id and cohort_start_date.")
  }

  domain_date_col <- c(
    condition   = "condition_start_date",
    drug        = "drug_exposure_start_date",
    procedure   = "procedure_date",
    measurement = "measurement_date",
    observation = "observation_date",
    visit       = "visit_start_date",
    death       = "death_date"
  )

  domains <- intersect(names(domain_data), names(domain_date_col))

  purrr::map_dfr(domains, function(d) {
    df       <- domain_data[[d]]
    date_col <- domain_date_col[[d]]

    if (nrow(df) == 0L || !date_col %in% names(df)) {
      # Return a zero-count skeleton for all patients x windows
      return(.zero_activity_skeleton(cohort_members, d, time_windows))
    }

    df <- dplyr::left_join(
      dplyr::rename(df, subject_id = person_id),
      dplyr::select(cohort_members, subject_id, cohort_start_date),
      by = "subject_id"
    )
    df <- df[!is.na(df$cohort_start_date), , drop = FALSE]
    df$days_from_index <- as.integer(df[[date_col]] - df$cohort_start_date)

    df <- dplyr::mutate(df,
      window_label = .assign_window(days_from_index, time_windows),
      window_idx   = match(window_label, time_windows$window_label)
    )
    df <- df[!is.na(df$window_label), , drop = FALSE]

    counts <- df |>
      dplyr::group_by(subject_id, window_label, window_idx) |>
      dplyr::summarise(event_count = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(domain = d)

    # Fill in zeros for all patient x window combinations not in data
    skeleton <- .zero_activity_skeleton(cohort_members, d, time_windows)
    dplyr::left_join(skeleton, counts,
                     by = c("subject_id", "domain", "window_label", "window_idx"),
                     suffix = c("_skel", "")) |>
      dplyr::mutate(event_count = dplyr::coalesce(event_count, 0L)) |>
      dplyr::select(subject_id, domain, window_label, window_idx, event_count)
  })
}

.zero_activity_skeleton <- function(cohort_members, domain, time_windows) {
  tidyr::crossing(
    subject_id   = cohort_members$subject_id,
    domain       = domain,
    tibble::tibble(
      window_label = time_windows$window_label,
      window_idx   = time_windows$window_idx
    )
  ) |>
    dplyr::mutate(event_count = 0L)
}

.assign_window <- function(days, time_windows) {
  # Convert month breaks to day boundaries (approximate: 1 month = 30.4375 days)
  days_start <- time_windows$months_start * 30.4375
  days_end   <- time_windows$months_end   * 30.4375
  n          <- length(days)
  result     <- rep(NA_character_, n)

  for (i in seq_len(nrow(time_windows))) {
    in_win <- days >= days_start[i] & days < days_end[i]
    result[in_win] <- time_windows$window_label[i]
  }
  result
}

# ---------------------------------------------------------------------------
# Concept-level feature matrix (for ML)
# ---------------------------------------------------------------------------

#' Build a patient-level feature matrix across time windows and concepts
#'
#' Produces both a long-format tibble (one row per patient x concept x window)
#' and a wide numeric matrix (subjects as rows, features as columns) suitable
#' for unsupervised ML.
#'
#' @param cohort_members tibble(subject_id, cohort_start_date).
#' @param domain_data Named list from [extract_omop_domains()].
#' @param time_windows tibble from [define_time_windows()].
#' @param domains Character vector of domains to include.
#' @param value_mode `"count"`, `"binary"`, or `"log1p_count"`.
#' @param min_concept_freq Minimum number of patients a concept must appear in
#'   to be retained in the feature matrix.
#'
#' @return List with `$long`, `$wide`, `$windows`, `$meta`.
#' @export
build_feature_matrix <- function(cohort_members,
                                  domain_data,
                                  time_windows      = define_time_windows(),
                                  domains           = c("condition","drug","procedure",
                                                        "measurement","observation",
                                                        "visit","death"),
                                  value_mode        = c("count","binary","log1p_count"),
                                  min_concept_freq  = 10L) {
  value_mode <- match.arg(value_mode)

  domain_concept_col <- c(
    condition   = "condition_concept_id",
    drug        = "drug_concept_id",
    procedure   = "procedure_concept_id",
    measurement = "measurement_concept_id",
    observation = "observation_concept_id",
    visit       = "visit_concept_id",
    death       = "death_type_concept_id"
  )
  domain_name_col <- c(
    condition   = "condition_name",
    drug        = "drug_name",
    procedure   = "procedure_name",
    measurement = "measurement_name",
    observation = "observation_name",
    visit       = "visit_type",
    death       = "cause_name"
  )
  domain_date_col <- c(
    condition   = "condition_start_date",
    drug        = "drug_exposure_start_date",
    procedure   = "procedure_date",
    measurement = "measurement_date",
    observation = "observation_date",
    visit       = "visit_start_date",
    death       = "death_date"
  )

  active_domains <- intersect(domains, names(domain_data))

  long <- purrr::map_dfr(active_domains, function(d) {
    df         <- domain_data[[d]]
    date_col   <- domain_date_col[[d]]
    concept_col <- domain_concept_col[[d]]
    name_col   <- domain_name_col[[d]]

    if (nrow(df) == 0L) return(tibble::tibble())

    df <- dplyr::rename(df, subject_id = person_id)
    df <- dplyr::left_join(df,
      dplyr::select(cohort_members, subject_id, cohort_start_date),
      by = "subject_id"
    )
    df <- df[!is.na(df$cohort_start_date), , drop = FALSE]
    df$days_from_index <- as.integer(df[[date_col]] - df$cohort_start_date)
    df$window_label    <- .assign_window(df$days_from_index, time_windows)
    df$window_idx      <- match(df$window_label, time_windows$window_label)
    df <- df[!is.na(df$window_label), , drop = FALSE]

    if (!concept_col %in% names(df)) return(tibble::tibble())

    df |>
      dplyr::group_by(
        subject_id,
        domain        = d,
        concept_id    = .data[[concept_col]],
        concept_name  = if (name_col %in% names(df)) .data[[name_col]] else NA_character_,
        window_label,
        window_idx
      ) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::rename(value = n)
  })

  if (nrow(long) == 0L) {
    return(list(long = long, wide = tibble::tibble(subject_id = cohort_members$subject_id),
                windows = time_windows,
                meta    = list(value_mode = value_mode, n_patients = nrow(cohort_members),
                               n_features = 0L, domains_used = active_domains)))
  }

  # Filter by minimum concept frequency
  concept_freq <- long |>
    dplyr::distinct(subject_id, domain, concept_id) |>
    dplyr::count(domain, concept_id)
  common_concepts <- concept_freq[concept_freq$n >= min_concept_freq, , drop = FALSE]
  long <- dplyr::semi_join(long, common_concepts, by = c("domain", "concept_id"))

  # Encode values
  long <- dplyr::mutate(long,
    value = switch(value_mode,
      binary      = as.numeric(value > 0),
      log1p_count = log1p(value),
      value
    )
  )

  # Pivot wide
  long_for_wide <- dplyr::mutate(long,
    feature_col = paste0(domain, "_", concept_id, "_", window_label)
  )
  wide <- long_for_wide |>
    dplyr::select(subject_id, feature_col, value) |>
    tidyr::pivot_wider(names_from  = feature_col,
                       values_from = value,
                       values_fill = 0,
                       values_fn   = sum)

  # Ensure all cohort members appear as rows
  all_subjects <- tibble::tibble(subject_id = cohort_members$subject_id)
  wide <- dplyr::left_join(all_subjects, wide, by = "subject_id")
  wide[is.na(wide)] <- 0

  list(
    long    = long,
    wide    = wide,
    windows = time_windows,
    meta    = list(
      value_mode    = value_mode,
      n_patients    = nrow(cohort_members),
      n_features    = ncol(wide) - 1L,
      domains_used  = active_domains
    )
  )
}

# ---------------------------------------------------------------------------
# Quilt data
# ---------------------------------------------------------------------------

#' Build the pre-computed data tibble that feeds the reactive quilt plot
#'
#' Takes domain activity (aggregated event counts per patient/domain/window)
#' and patient ranking information, and returns a plot-ready tibble with one
#' row per (subject_id x domain x window_label). The quilt Shiny module caches
#' this tibble in a `reactiveVal` and only filters/re-sorts it interactively.
#'
#' @param domain_activity tibble from [build_domain_activity()].
#' @param rank_df tibble from [rank_patients()]. May be `NULL`; in that case
#'   patients are sorted by `subject_id` and get a placeholder cluster.
#' @param sort_by One of `"cluster"`, `"rank"`, `"subject_id"`.
#' @param domains Character vector. Restrict to these domains. `NULL` = all.
#' @param value_encoding `"log1p_count"`, `"binary"`, or `"count"`.
#' @param clip_max Numeric. Clip event counts at this value before encoding.
#'   `NULL` = no clipping.
#'
#' @return tibble with columns: subject_id, patient_row, display_label,
#'   domain, window_label, window_idx, event_count, fill_value,
#'   cluster_id, priority_tier, rank_position.
#' @export
build_quilt_data <- function(domain_activity,
                              rank_df        = NULL,
                              sort_by        = c("cluster","rank","subject_id"),
                              domains        = NULL,
                              value_encoding = c("log1p_count","binary","count"),
                              clip_max       = NULL) {
  sort_by        <- match.arg(sort_by)
  value_encoding <- match.arg(value_encoding)

  if (!all(c("subject_id","domain","window_label","window_idx","event_count")
           %in% names(domain_activity))) {
    rlang::abort("'domain_activity' must have columns: subject_id, domain, window_label, window_idx, event_count.")
  }

  d <- domain_activity

  if (!is.null(domains)) {
    d <- dplyr::filter(d, domain %in% domains)
  }

  # Attach rank / cluster info
  if (!is.null(rank_df) &&
      all(c("subject_id","rank_position","priority_tier","cluster_id") %in% names(rank_df))) {
    d <- dplyr::left_join(d,
      dplyr::select(rank_df, subject_id, rank_position, priority_tier, cluster_id),
      by = "subject_id"
    )
  } else {
    d <- dplyr::mutate(d,
      rank_position = match(subject_id, sort(unique(subject_id))),
      priority_tier = "unranked",
      cluster_id    = 0L
    )
  }

  d <- dplyr::mutate(d,
    cluster_id    = dplyr::coalesce(cluster_id, 0L),
    priority_tier = dplyr::coalesce(priority_tier, "unranked"),
    rank_position = dplyr::coalesce(rank_position,
                                     match(subject_id, sort(unique(subject_id))))
  )

  # Clip counts
  if (!is.null(clip_max)) {
    d <- dplyr::mutate(d, event_count = pmin(event_count, as.integer(clip_max)))
  }

  # Compute fill value
  d <- dplyr::mutate(d,
    fill_value = switch(value_encoding,
      log1p_count = log1p(event_count),
      binary      = as.numeric(event_count > 0),
      count       = as.numeric(event_count)
    )
  )

  # Compute patient row order (determines vertical position in quilt)
  d <- .reorder_patient_rows(d, sort_by)

  # Build display label
  d <- dplyr::mutate(d,
    display_label = paste0("P", subject_id,
                            ifelse(cluster_id != 0L,
                                   paste0(" [C", cluster_id, "]"),
                                   ""))
  )

  dplyr::select(d,
    subject_id, patient_row, display_label,
    domain, window_label, window_idx,
    event_count, fill_value,
    cluster_id, priority_tier, rank_position
  )
}

# ---------------------------------------------------------------------------
# Cohort summary statistics
# ---------------------------------------------------------------------------

#' Compute cohort-level summary statistics for the demographics tab
#'
#' @param cohort_members tibble(subject_id, cohort_start_date, cohort_end_date).
#' @param person_data tibble from [extract_person_demographics()]. May be a
#'   zero-row tibble if demographics were not extracted.
#'
#' @return Named list with `$n_patients`, `$median_followup`, `$median_age`,
#'   `$pct_death`, `$age_df`, `$sex_df`, `$race_df`.
#' @export
build_cohort_summary <- function(cohort_members, person_data = NULL) {
  n  <- nrow(cohort_members)
  pd <- if (!is.null(person_data) && nrow(person_data) > 0L) {
    tibble::as_tibble(person_data)
  } else {
    tibble::tibble(
      person_id        = integer(0),
      birth_year       = integer(0),
      gender_name      = character(0),
      race_name        = character(0),
      ethnicity_name   = character(0)
    )
  }

  # Median follow-up
  has_end  <- "cohort_end_date" %in% names(cohort_members)
  med_fu   <- if (has_end && any(!is.na(cohort_members$cohort_end_date))) {
    median(as.integer(cohort_members$cohort_end_date -
                        cohort_members$cohort_start_date), na.rm = TRUE)
  } else {
    NA_real_
  }

  # Age at index
  age_df <- tibble::tibble()
  med_age <- NA_real_
  if (nrow(pd) > 0L && "birth_year" %in% names(pd) &&
      !all(is.na(pd$birth_year))) {
    age_df <- dplyr::left_join(
      dplyr::select(cohort_members, subject_id, cohort_start_date),
      dplyr::rename(pd, subject_id = person_id),
      by = "subject_id"
    ) |>
      dplyr::mutate(
        age_at_index = as.integer(
          lubridate::year(cohort_start_date) - birth_year
        )
      ) |>
      dplyr::filter(!is.na(age_at_index)) |>
      dplyr::select(subject_id, age_at_index)
    med_age <- median(age_df$age_at_index, na.rm = TRUE)
  }

  # Sex distribution
  sex_df <- if (nrow(pd) > 0L && "gender_name" %in% names(pd)) {
    pd |>
      dplyr::count(gender_name = dplyr::coalesce(gender_name, "Unknown"),
                   name = "n") |>
      dplyr::arrange(dplyr::desc(n))
  } else {
    tibble::tibble(gender_name = character(0), n = integer(0))
  }

  # Race distribution (top 8 + Other)
  race_df <- if (nrow(pd) > 0L && "race_name" %in% names(pd)) {
    top8 <- pd |>
      dplyr::count(race_name = dplyr::coalesce(race_name, "Unknown"),
                   name = "n") |>
      dplyr::arrange(dplyr::desc(n))
    if (nrow(top8) > 8L) {
      other_n <- sum(top8$n[-(1:8)])
      top8 <- dplyr::bind_rows(top8[1:8, ],
                                tibble::tibble(race_name = "Other",
                                               n = other_n))
    }
    top8
  } else {
    tibble::tibble(race_name = character(0), n = integer(0))
  }

  # % death (rough: based on presence in death domain, but we check person_data here)
  pct_death <- NA_real_

  list(
    n_patients      = n,
    median_followup = med_fu,
    median_age      = med_age,
    pct_death       = pct_death,
    age_df          = age_df,
    sex_df          = sex_df,
    race_df         = race_df
  )
}

#' Build a data density tibble for the calendar heatmap
#'
#' Counts the number of distinct patients with at least one event per domain
#' per calendar month. Used by the Demographics tab data density heatmap.
#'
#' @param domain_data Named list from [extract_omop_domains()].
#' @param cohort_members tibble(subject_id, cohort_start_date).
#'
#' @return tibble(domain, calendar_month, n_patients) where
#'   `calendar_month` is a Date truncated to the first of the month.
#' @export
build_data_density <- function(domain_data, cohort_members) {
  domain_date_col <- c(
    condition   = "condition_start_date",
    drug        = "drug_exposure_start_date",
    procedure   = "procedure_date",
    measurement = "measurement_date",
    observation = "observation_date",
    visit       = "visit_start_date",
    death       = "death_date"
  )

  active_domains <- intersect(names(domain_data), names(domain_date_col))

  purrr::map_dfr(active_domains, function(d) {
    df       <- domain_data[[d]]
    date_col <- domain_date_col[[d]]
    if (nrow(df) == 0L || !date_col %in% names(df)) return(tibble::tibble())

    df |>
      dplyr::rename(subject_id = person_id) |>
      dplyr::filter(subject_id %in% cohort_members$subject_id,
                    !is.na(.data[[date_col]])) |>
      dplyr::mutate(
        calendar_month = lubridate::floor_date(.data[[date_col]], "month")
      ) |>
      dplyr::group_by(calendar_month) |>
      dplyr::summarise(n_patients = dplyr::n_distinct(subject_id),
                       .groups = "drop") |>
      dplyr::mutate(domain = d)
  }) |>
    dplyr::select(domain, calendar_month, n_patients)
}

#' Re-order patient rows in a quilt tibble (internal)
#'
#' @param d tibble with columns subject_id, cluster_id, rank_position.
#' @param sort_by One of `"cluster"`, `"rank"`, `"subject_id"`.
#' @return `d` with `patient_row` column added/updated.
#' @noRd
.reorder_patient_rows <- function(d, sort_by = c("cluster","rank","subject_id")) {
  sort_by <- match.arg(sort_by)

  patient_order <- d |>
    dplyr::distinct(subject_id, cluster_id, rank_position) |>
    dplyr::arrange(
      switch(sort_by,
        cluster    = cluster_id,
        rank       = rank_position,
        subject_id = subject_id
      ),
      subject_id
    ) |>
    dplyr::mutate(patient_row = dplyr::row_number())

  d <- dplyr::select(d, -dplyr::any_of("patient_row"))
  dplyr::left_join(d, dplyr::select(patient_order, subject_id, patient_row),
                   by = "subject_id")
}

# ---------------------------------------------------------------------------
# Cluster characterisation profiles
# ---------------------------------------------------------------------------

#' Build clinical characterisation profiles for each cluster
#'
#' For each cluster, computes:
#'  * Summary statistics (size, median age, % female, % death, follow-up)
#'  * Top-N most prevalent concepts per OMOP domain
#'
#' These are the inputs to the Cluster Profiles tab.
#'
#' @param rank_df tibble from [rank_patients()] with `subject_id` and
#'   `cluster_id` columns.
#' @param domain_data Named list from [extract_omop_domains()].
#' @param cohort_members tibble(subject_id, cohort_start_date,
#'   cohort_end_date).
#' @param person_data tibble from [extract_person_demographics()]. `NULL`
#'   skips age/sex statistics.
#' @param top_n Integer. Top concepts per domain per cluster. Default 10.
#' @param domains Character vector. Domains to include in concept ranks.
#'
#' @return Named list with `$summary` and `$concepts` tibbles.
#' @export
build_cluster_profiles <- function(rank_df,
                                    domain_data,
                                    cohort_members,
                                    person_data  = NULL,
                                    top_n        = 10L,
                                    domains      = c("condition","drug",
                                                     "procedure")) {
  if (is.null(rank_df) || nrow(rank_df) == 0L) {
    return(list(
      summary  = tibble::tibble(),
      concepts = tibble::tibble()
    ))
  }

  cluster_map <- dplyr::select(rank_df, subject_id, cluster_id)
  n_total     <- nrow(cluster_map)

  # -- Summary stats per cluster --------------------------------------------
  summary_base <- cluster_map |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(
      n_patients = dplyr::n(),
      .groups    = "drop"
    ) |>
    dplyr::mutate(
      pct_cohort = round(100 * n_patients / n_total, 1)
    )

  # Age and sex from person_data
  age_sex <- if (!is.null(person_data) && nrow(person_data) > 0L &&
                  "birth_year" %in% names(person_data)) {
    pd <- dplyr::rename(person_data, subject_id = person_id) |>
      dplyr::left_join(
        dplyr::select(cohort_members, subject_id, cohort_start_date),
        by = "subject_id"
      ) |>
      dplyr::mutate(
        age_at_index = as.integer(
          lubridate::year(cohort_start_date) - birth_year
        ),
        is_female    = tolower(gender_name %||% "") %in%
                         c("female", "f", "8532")
      )
    dplyr::left_join(cluster_map, pd, by = "subject_id") |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        median_age   = median(age_at_index, na.rm = TRUE),
        pct_female   = round(100 * mean(is_female, na.rm = TRUE), 1),
        .groups      = "drop"
      )
  } else {
    tibble::tibble(
      cluster_id = summary_base$cluster_id,
      median_age = NA_real_,
      pct_female = NA_real_
    )
  }

  # Median follow-up
  fu_df <- dplyr::left_join(
    cluster_map,
    dplyr::select(cohort_members, subject_id, cohort_start_date,
                  cohort_end_date),
    by = "subject_id"
  ) |>
    dplyr::mutate(
      followup_days = as.integer(
        dplyr::coalesce(cohort_end_date, cohort_start_date) - cohort_start_date
      )
    ) |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(
      median_followup_days = median(followup_days, na.rm = TRUE),
      .groups              = "drop"
    )

  # % death
  death_df <- if (!is.null(domain_data[["death"]]) &&
                   nrow(domain_data[["death"]]) > 0L) {
    dead_ids <- unique(domain_data[["death"]]$person_id)
    cluster_map |>
      dplyr::mutate(has_death = subject_id %in% dead_ids) |>
      dplyr::group_by(cluster_id) |>
      dplyr::summarise(
        pct_death = round(100 * mean(has_death), 1),
        .groups   = "drop"
      )
  } else {
    tibble::tibble(cluster_id = summary_base$cluster_id, pct_death = 0)
  }

  summary_df <- summary_base |>
    dplyr::left_join(age_sex, by = "cluster_id") |>
    dplyr::left_join(fu_df,   by = "cluster_id") |>
    dplyr::left_join(death_df, by = "cluster_id") |>
    dplyr::arrange(cluster_id)

  # -- Top concepts per domain per cluster ----------------------------------
  domain_concept_col <- c(
    condition   = "condition_concept_id",
    drug        = "drug_concept_id",
    procedure   = "procedure_concept_id",
    measurement = "measurement_concept_id",
    observation = "observation_concept_id"
  )
  domain_name_col <- c(
    condition   = "condition_name",
    drug        = "drug_name",
    procedure   = "procedure_name",
    measurement = "measurement_name",
    observation = "observation_name"
  )

  active_domains <- intersect(domains, names(domain_data))
  active_domains <- intersect(active_domains, names(domain_concept_col))

  concepts_df <- purrr::map_dfr(active_domains, function(d) {
    df         <- domain_data[[d]]
    name_col   <- domain_name_col[[d]]
    if (is.null(df) || nrow(df) == 0L || !name_col %in% names(df)) {
      return(tibble::tibble())
    }

    df |>
      dplyr::rename(subject_id = person_id) |>
      dplyr::inner_join(cluster_map, by = "subject_id") |>
      dplyr::group_by(cluster_id, concept_name = .data[[name_col]]) |>
      dplyr::summarise(
        n_patients = dplyr::n_distinct(subject_id),
        .groups    = "drop"
      ) |>
      dplyr::left_join(
        dplyr::select(summary_df, cluster_id, n_patients),
        by     = "cluster_id",
        suffix = c("", "_total")
      ) |>
      dplyr::mutate(
        domain     = d,
        prevalence = round(100 * n_patients / n_patients_total, 1)
      ) |>
      dplyr::select(cluster_id, domain, concept_name, n_patients, prevalence) |>
      dplyr::arrange(cluster_id, dplyr::desc(prevalence)) |>
      dplyr::group_by(cluster_id) |>
      dplyr::slice_head(n = as.integer(top_n)) |>
      dplyr::ungroup()
  })

  list(summary = summary_df, concepts = concepts_df)
}
