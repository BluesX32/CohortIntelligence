# make_sample_packet.R
# Generates a sample clinician review packet from synthetic data and writes it
# to inst/example/sample_review_packet.html.
#
# Run from the package root:
#   Rscript inst/example/make_sample_packet.R
# or interactively after devtools::load_all("."):
#   source("inst/example/make_sample_packet.R")

devtools::load_all(".")

set.seed(42L)
n <- 60L

# ── 1. Build synthetic cohort ────────────────────────────────────────────────
index_date <- as.Date("2019-01-01")
half       <- ceiling(n / 2L)

cohort_members <- tibble::tibble(
  cohort_definition_id = 1L,
  subject_id           = seq_len(n),
  cohort_start_date    = index_date,
  cohort_end_date      = index_date + 730L
)

cond_events <- purrr::map_dfr(seq_len(n), function(pid) {
  n_ev    <- if (pid <= half) sample(4:10, 1) else sample(0:2, 1)
  if (n_ev == 0L) return(tibble::tibble())
  offsets <- if (pid <= half) sample(-180L:-1L, n_ev, replace = TRUE) else
                              sample(-365L:-181L, n_ev, replace = TRUE)
  tibble::tibble(
    condition_occurrence_id = NA_integer_,
    person_id               = pid,
    condition_start_date    = index_date + offsets,
    condition_end_date      = index_date + offsets + 30L,
    condition_concept_id    = sample(c(201820L, 316866L, 432867L), n_ev,
                                     replace = TRUE),
    condition_name          = sample(c("Type 2 diabetes mellitus",
                                       "Essential hypertension",
                                       "Chronic kidney disease"),
                                     n_ev, replace = TRUE),
    condition_source_value  = "src"
  )
}) |> dplyr::mutate(condition_occurrence_id = dplyr::row_number())

drug_events <- purrr::map_dfr(seq_len(n), function(pid) {
  n_ev    <- if (pid > half) sample(4:10, 1) else sample(0:2, 1)
  if (n_ev == 0L) return(tibble::tibble())
  offsets <- if (pid > half) sample(0L:180L, n_ev, replace = TRUE) else
                             sample(181L:365L, n_ev, replace = TRUE)
  tibble::tibble(
    drug_exposure_id         = NA_integer_,
    person_id                = pid,
    drug_exposure_start_date = index_date + offsets,
    drug_exposure_end_date   = index_date + offsets + 90L,
    drug_concept_id          = sample(c(1503297L, 40163924L), n_ev,
                                      replace = TRUE),
    drug_name                = sample(c("Metformin", "Lisinopril"),
                                      n_ev, replace = TRUE),
    drug_source_value        = "src"
  )
}) |> dplyr::mutate(drug_exposure_id = dplyr::row_number())

visit_events <- purrr::map_dfr(seq_len(n), function(pid) {
  n_ev <- sample(2:6, 1)
  tibble::tibble(
    visit_occurrence_id = NA_integer_,
    person_id           = pid,
    visit_start_date    = index_date + sample(-365L:365L, n_ev, replace = TRUE),
    visit_end_date      = index_date + sample(-365L:365L, n_ev, replace = TRUE),
    visit_concept_id    = 9202L,
    visit_type          = "Outpatient Visit",
    visit_source_value  = "OP"
  )
}) |> dplyr::mutate(visit_occurrence_id = dplyr::row_number())

connector <- create_cohort_df_connector(list(
  cohort      = cohort_members,
  person      = .empty_cohort_domain("person"),
  condition   = cond_events,
  drug        = drug_events,
  procedure   = .empty_cohort_domain("procedure"),
  measurement = .empty_cohort_domain("measurement"),
  observation = .empty_cohort_domain("observation"),
  visit       = visit_events,
  death       = .empty_cohort_domain("death")
))

# ── 2. Extract and build features ─────────────────────────────────────────────
time_windows <- define_time_windows(breaks_months = c(-12, -6, 0, 6, 12))
members      <- extract_cohort_members(connector)
domain_data  <- extract_omop_domains(connector, members$subject_id,
                                      domains = c("condition","drug","visit"))
domain_act   <- build_domain_activity(members, domain_data, time_windows)
fm           <- build_feature_matrix(members, domain_data, time_windows,
                                      min_concept_freq = 3L)

# ── 3. ML pipeline ────────────────────────────────────────────────────────────
ml <- tryCatch(run_full_ml_pipeline(fm), error = function(e) {
  message("ML pipeline unavailable (uwot/isotree not installed): ", e$message)
  NULL
})

# ── 4. Patient ranking ────────────────────────────────────────────────────────
rank_df <- rank_patients(ml, domain_act, members)

# ── 5. Supporting outputs ────────────────────────────────────────────────────
tf  <- detect_temporal_flags(members, domain_data, time_windows)
rs  <- build_review_sets(rank_df, domain_act, fm, ml, members,
                          temporal_flags = tf, n_per_set = 5L)
hyp <- generate_hypotheses(fm, ml, min_effect_size = 0.1, max_hypotheses = 10L)
cp  <- build_cluster_profiles(rank_df, domain_data, members,
                               top_n = 5L, domains = c("condition","drug"))
qb  <- build_quilt_data(domain_act, rank_df)
cs  <- build_cohort_summary(members)

# ── 6. Export packet ──────────────────────────────────────────────────────────
out_path <- file.path("inst", "example", "sample_review_packet.html")

export_clinician_review_packet(
  results = list(
    cohort_members   = members,
    quilt_base       = qb,
    person_data      = NULL,
    hypotheses       = hyp,
    temporal_flags   = tf,
    review_sets      = rs,
    cluster_profiles = cp
  ),
  path        = out_path,
  cohort_name = "Synthetic Cohort — Demo (n=60)",
  n_patients  = 5L
)

message("Sample packet written to: ", normalizePath(out_path))
