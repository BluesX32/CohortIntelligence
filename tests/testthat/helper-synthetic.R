# helper-synthetic.R
# Shared synthetic fixture factory for CohortIntelligence tests.
# No database required — uses cohort_df_connector.
#
# make_test_cohort(n=20, seed=42) produces:
#   - n patients across 2 clusters (first half / second half)
#   - 3 time windows: [-12,-6), [-6,0), [0,6) months
#   - 3 domains: condition, drug, visit
#   - Cluster A (patients 1..n/2): high condition counts in [-6,0)
#   - Cluster B (patients n/2+1..n): high drug counts in [0,6)

make_test_cohort <- function(n_patients = 20L, seed = 42L) {
  set.seed(seed)
  n <- as.integer(n_patients)

  cohort_members <- tibble::tibble(
    cohort_definition_id = 1L,
    subject_id           = seq_len(n),
    cohort_start_date    = as.Date("2019-01-01"),
    cohort_end_date      = as.Date("2021-01-01")
  )

  index_date <- as.Date("2019-01-01")
  half       <- ceiling(n / 2L)

  # Condition events: cluster A heavy in [-6,0) (days -180 to 0)
  cond_events <- purrr::map_dfr(seq_len(n), function(pid) {
    n_ev <- if (pid <= half) sample(3:8, 1) else sample(0:1, 1)
    if (n_ev == 0L) return(tibble::tibble())
    offsets <- if (pid <= half)
      sample(-180L:-1L, n_ev, replace = TRUE)
    else
      sample(-365L:-181L, n_ev, replace = TRUE)
    tibble::tibble(
      condition_occurrence_id = NA_integer_,
      person_id               = pid,
      condition_start_date    = index_date + offsets,
      condition_end_date      = index_date + offsets + 30L,
      condition_concept_id    = sample(c(201820L, 316866L), n_ev, replace = TRUE),
      condition_name          = sample(c("Diabetes","Hypertension"), n_ev, replace = TRUE),
      condition_source_value  = "src"
    )
  }) |>
    dplyr::mutate(condition_occurrence_id = dplyr::row_number())

  # Drug events: cluster B heavy in [0,6) (days 0 to 180)
  drug_events <- purrr::map_dfr(seq_len(n), function(pid) {
    n_ev <- if (pid > half) sample(3:8, 1) else sample(0:1, 1)
    if (n_ev == 0L) return(tibble::tibble())
    offsets <- if (pid > half)
      sample(0L:180L, n_ev, replace = TRUE)
    else
      sample(181L:365L, n_ev, replace = TRUE)
    tibble::tibble(
      drug_exposure_id         = NA_integer_,
      person_id                = pid,
      drug_exposure_start_date = index_date + offsets,
      drug_exposure_end_date   = index_date + offsets + 90L,
      drug_concept_id          = 1503297L,
      drug_name                = "Methotrexate",
      drug_source_value        = "src"
    )
  }) |>
    dplyr::mutate(drug_exposure_id = dplyr::row_number())

  # Visit events: all patients, random distribution
  visit_events <- purrr::map_dfr(seq_len(n), function(pid) {
    n_ev <- sample(1:4, 1)
    tibble::tibble(
      visit_occurrence_id = NA_integer_,
      person_id           = pid,
      visit_start_date    = index_date + sample(-365L:365L, n_ev, replace = TRUE),
      visit_end_date      = index_date + sample(-365L:365L, n_ev, replace = TRUE),
      visit_concept_id    = 9202L,
      visit_type          = "Outpatient Visit",
      visit_source_value  = "OP"
    )
  }) |>
    dplyr::mutate(visit_occurrence_id = dplyr::row_number())

  connector <- CohortIntelligence::create_cohort_df_connector(list(
    cohort      = cohort_members,
    person      = CohortIntelligence:::.empty_cohort_domain("person"),
    condition   = cond_events,
    drug        = drug_events,
    procedure   = CohortIntelligence:::.empty_cohort_domain("procedure"),
    measurement = CohortIntelligence:::.empty_cohort_domain("measurement"),
    observation = CohortIntelligence:::.empty_cohort_domain("observation"),
    visit       = visit_events,
    death       = CohortIntelligence:::.empty_cohort_domain("death")
  ))

  time_windows  <- CohortIntelligence::define_time_windows(
    breaks_months = c(-12, -6, 0, 6)
  )
  members       <- CohortIntelligence::extract_cohort_members(connector)
  domain_data   <- CohortIntelligence::extract_omop_domains(
    connector, subject_ids = members$subject_id,
    domains = c("condition","drug","visit")
  )
  domain_act    <- CohortIntelligence::build_domain_activity(
    members, domain_data, time_windows
  )

  # Build rank_df with known cluster assignments (1 = cluster A, 2 = cluster B)
  rank_df <- tibble::tibble(
    subject_id    = seq_len(n),
    rank_score    = stats::runif(n),
    rank_position = sample(seq_len(n), n),
    priority_tier = sample(c("high","medium","low"), n, replace = TRUE),
    anomaly_score = stats::runif(n),
    cluster_id    = c(rep(1L, half), rep(2L, n - half)),
    sparsity_score = stats::runif(n, 0, 0.5)
  )

  quilt_base <- CohortIntelligence::build_quilt_data(domain_act, rank_df)

  list(
    connector      = connector,
    cohort_members = members,
    domain_data    = domain_data,
    domain_activity = domain_act,
    time_windows   = time_windows,
    rank_df        = rank_df,
    quilt_base     = quilt_base
  )
}
