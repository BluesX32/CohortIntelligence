# outcomes.R
# Outcome-focused temporal analysis functions for CohortIntelligence.
#
# These functions complement the cohort exploration pipeline by letting
# researchers designate a specific concept as an "outcome" (or "exposure")
# and ask questions about its timing and distribution relative to the cohort
# index date.  All outputs are exploratory and require clinical validation.

# ---------------------------------------------------------------------------
# Internal: domain column specs (mirrors features.R / extract.R maps)
# ---------------------------------------------------------------------------

.oc_domain_date_col <- c(
  condition   = "condition_start_date",
  drug        = "drug_exposure_start_date",
  procedure   = "procedure_date",
  measurement = "measurement_date",
  observation = "observation_date",
  visit       = "visit_start_date",
  death       = "death_date"
)

.oc_domain_concept_col <- c(
  condition   = "condition_concept_id",
  drug        = "drug_concept_id",
  procedure   = "procedure_concept_id",
  measurement = "measurement_concept_id",
  observation = "observation_concept_id",
  visit       = "visit_concept_id",
  death       = "death_type_concept_id"
)

.oc_domain_name_col <- c(
  condition   = "condition_name",
  drug        = "drug_name",
  procedure   = "procedure_name",
  measurement = "measurement_name",
  observation = "observation_name",
  visit       = "visit_type",
  death       = "cause_name"
)

# Empty return schemas -------------------------------------------------------

.empty_event_dist <- function() {
  tibble::tibble(
    bin_start  = integer(0),
    bin_end    = integer(0),
    bin_mid    = integer(0),
    n_patients = integer(0),
    n_events   = integer(0)
  )
}

.empty_outcome_labels <- function(cohort_members) {
  tibble::tibble(
    subject_id          = cohort_members$subject_id,
    has_outcome         = FALSE,
    days_to_first_event = NA_integer_,
    days_bin            = factor(NA_character_,
                                  levels  = c("0-90d","91-180d",
                                              "181-365d",">365d","None"),
                                  ordered = TRUE)
  )
}

# ---------------------------------------------------------------------------
# compute_event_distribution()
# ---------------------------------------------------------------------------

#' Compute the temporal distribution of a concept's events relative to index
#'
#' Returns a histogram-ready tibble: how many patients (and events) had a
#' particular concept recorded in each day-bin relative to the cohort index
#' date.  Use this to understand *when* a concept of interest typically occurs
#' in the patient's timeline — before index, shortly after, or much later.
#'
#' @param cohort_members tibble(subject_id, cohort_start_date) from
#'   [extract_cohort_members()].
#' @param domain_data Named list from [extract_omop_domains()].
#' @param domain Character(1). One of `"condition"`, `"drug"`,
#'   `"procedure"`, `"measurement"`, `"observation"`, `"visit"`,
#'   `"death"`.
#' @param concept_id Integer(1). Concept to filter on. If `NULL` or `NA`,
#'   all concepts in the domain are included.
#' @param day_range Integer(2). Window (days from index) to include.
#'   Default `c(-730L, 730L)`.
#' @param bin_width Integer(1). Width of each histogram bin in days.
#'   Default `30L`.
#'
#' @return tibble with columns: `bin_start`, `bin_end`, `bin_mid`
#'   (all integers, days from index), `n_patients` (distinct patients in
#'   bin), `n_events` (total event rows in bin).  All bins between
#'   `day_range[1]` and `day_range[2]` are returned; zero-count bins are
#'   included.
#' @export
compute_event_distribution <- function(cohort_members,
                                        domain_data,
                                        domain,
                                        concept_id  = NULL,
                                        day_range   = c(-730L, 730L),
                                        bin_width   = 30L) {
  if (!domain %in% names(.oc_domain_date_col)) {
    rlang::abort(sprintf(
      "Unknown domain '%s'. Must be one of: %s",
      domain, paste(names(.oc_domain_date_col), collapse = ", ")
    ))
  }
  if (!domain %in% names(domain_data)) {
    rlang::abort(sprintf("Domain '%s' not present in domain_data.", domain))
  }

  date_col    <- .oc_domain_date_col[[domain]]
  concept_col <- .oc_domain_concept_col[[domain]]
  bin_width   <- as.integer(bin_width)
  day_lo      <- as.integer(day_range[[1L]])
  day_hi      <- as.integer(day_range[[2L]])

  df <- domain_data[[domain]]

  # Build complete bin grid unconditionally so zero-count bins are returned
  bin_starts <- seq(
    from = (day_lo %/% bin_width) * bin_width,
    to   = (day_hi %/% bin_width) * bin_width,
    by   = bin_width
  )
  bin_grid <- tibble::tibble(
    bin_start = as.integer(bin_starts),
    bin_end   = as.integer(bin_starts + bin_width),
    bin_mid   = as.integer(bin_starts + bin_width %/% 2L),
    n_patients = 0L,
    n_events   = 0L
  )

  if (is.null(df) || nrow(df) == 0L || !date_col %in% names(df)) {
    return(bin_grid)
  }

  # Optionally filter to specific concept
  if (!is.null(concept_id) && !is.na(concept_id[[1L]]) &&
      concept_col %in% names(df)) {
    df <- df[df[[concept_col]] %in% as.integer(concept_id), , drop = FALSE]
  }

  if (nrow(df) == 0L) return(bin_grid)

  # Join to cohort members, compute days_from_index
  df <- dplyr::rename(df, subject_id = person_id)
  df <- dplyr::left_join(
    df,
    dplyr::select(cohort_members, subject_id, cohort_start_date),
    by = "subject_id"
  )
  df <- df[!is.na(df$cohort_start_date), , drop = FALSE]
  df$days_from_index <- as.integer(df[[date_col]] - df$cohort_start_date)
  df <- df[!is.na(df$days_from_index) &
             df$days_from_index >= day_lo &
             df$days_from_index <= day_hi, , drop = FALSE]

  if (nrow(df) == 0L) return(bin_grid)

  # Assign to bin
  df$bin_start <- as.integer((df$days_from_index %/% bin_width) * bin_width)

  # Aggregate
  counts <- df |>
    dplyr::group_by(bin_start) |>
    dplyr::summarise(
      n_patients = dplyr::n_distinct(subject_id),
      n_events   = dplyr::n(),
      .groups    = "drop"
    )

  # Merge counts onto complete bin grid
  dplyr::left_join(
    dplyr::select(bin_grid, -n_patients, -n_events),
    counts,
    by = "bin_start"
  ) |>
    dplyr::mutate(
      n_patients = as.integer(dplyr::coalesce(n_patients, 0L)),
      n_events   = as.integer(dplyr::coalesce(n_events,   0L))
    ) |>
    dplyr::select(bin_start, bin_end, bin_mid, n_patients, n_events)
}

# ---------------------------------------------------------------------------
# compute_outcome_labels()
# ---------------------------------------------------------------------------

#' Assign per-patient outcome status and time-to-first-event
#'
#' For each patient, determines whether they experienced a given concept
#' within a specified window relative to the cohort index date.  Returns
#' an outcome-labelled tibble suitable for overlaying on UMAP plots or
#' stratifying any downstream analysis.
#'
#' @param cohort_members tibble(subject_id, cohort_start_date).
#' @param domain_data Named list from [extract_omop_domains()].
#' @param domain Character(1). Domain to search for the outcome.
#' @param concept_id Integer(1). Concept representing the outcome. If `NULL`
#'   or `NA`, any event in the domain counts.
#' @param post_index_only Logical. If `TRUE` (default), only events with
#'   `days_from_index >= 0` are considered (post-index outcomes).
#' @param day_range Integer(2). Further restrict the window. Defaults to
#'   `c(0L, 730L)` when `post_index_only = TRUE`.
#'
#' @return tibble with columns:
#'   * `subject_id` — all subjects from `cohort_members`
#'   * `has_outcome` — logical; `TRUE` if at least one event in window
#'   * `days_to_first_event` — integer; `NA` when `has_outcome == FALSE`
#'   * `days_bin` — ordered factor with levels
#'     `"0-90d"`, `"91-180d"`, `"181-365d"`, `">365d"`, `"None"`
#' @export
compute_outcome_labels <- function(cohort_members,
                                    domain_data,
                                    domain,
                                    concept_id      = NULL,
                                    post_index_only = TRUE,
                                    day_range       = c(0L, 730L)) {
  if (!domain %in% names(.oc_domain_date_col)) {
    rlang::abort(sprintf(
      "Unknown domain '%s'. Must be one of: %s",
      domain, paste(names(.oc_domain_date_col), collapse = ", ")
    ))
  }
  if (!domain %in% names(domain_data)) {
    return(.empty_outcome_labels(cohort_members))
  }

  date_col    <- .oc_domain_date_col[[domain]]
  concept_col <- .oc_domain_concept_col[[domain]]
  day_lo      <- as.integer(day_range[[1L]])
  day_hi      <- as.integer(day_range[[2L]])

  if (post_index_only) day_lo <- max(day_lo, 0L)

  df <- domain_data[[domain]]

  if (is.null(df) || nrow(df) == 0L || !date_col %in% names(df)) {
    return(.empty_outcome_labels(cohort_members))
  }

  # Optionally filter to specific concept
  if (!is.null(concept_id) && !is.na(concept_id[[1L]]) &&
      concept_col %in% names(df)) {
    df <- df[df[[concept_col]] %in% as.integer(concept_id), , drop = FALSE]
  }

  if (nrow(df) == 0L) return(.empty_outcome_labels(cohort_members))

  # Join, compute days_from_index, apply window
  df <- dplyr::rename(df, subject_id = person_id)
  df <- dplyr::left_join(
    df,
    dplyr::select(cohort_members, subject_id, cohort_start_date),
    by = "subject_id"
  )
  df <- df[!is.na(df$cohort_start_date), , drop = FALSE]
  df$days_from_index <- as.integer(df[[date_col]] - df$cohort_start_date)
  df <- df[!is.na(df$days_from_index) &
             df$days_from_index >= day_lo &
             df$days_from_index <= day_hi, , drop = FALSE]

  if (nrow(df) == 0L) return(.empty_outcome_labels(cohort_members))

  # First event per patient
  first_events <- df |>
    dplyr::group_by(subject_id) |>
    dplyr::summarise(
      days_to_first_event = min(days_from_index, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(days_to_first_event = as.integer(days_to_first_event))

  # Left-join to all cohort members (guarantees every patient is returned)
  result <- dplyr::left_join(
    dplyr::select(cohort_members, subject_id),
    first_events,
    by = "subject_id"
  ) |>
    dplyr::mutate(
      has_outcome = !is.na(days_to_first_event),
      days_bin    = dplyr::case_when(
        is.na(days_to_first_event)         ~ "None",
        days_to_first_event <= 90L         ~ "0-90d",
        days_to_first_event <= 180L        ~ "91-180d",
        days_to_first_event <= 365L        ~ "181-365d",
        TRUE                               ~ ">365d"
      )
    ) |>
    dplyr::mutate(
      days_bin = factor(days_bin,
                         levels  = c("0-90d","91-180d","181-365d",">365d","None"),
                         ordered = TRUE)
    )

  dplyr::select(result, subject_id, has_outcome, days_to_first_event, days_bin)
}
