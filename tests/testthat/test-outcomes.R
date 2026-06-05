# test-outcomes.R
# Tests for compute_event_distribution() and compute_outcome_labels()
# All use make_test_cohort() from helper-synthetic.R.
#
# make_test_cohort() notes:
#   - n=20 patients, 2 clusters (half/half)
#   - Cluster A (patients 1..10): condition events in [-180:-1] days (PRE-index)
#   - Cluster B (patients 11..20): drug events in [0:180] days (POST-index)
#   - Visit events: all patients, random distribution across -365:365
#   - condition_concept_id: 201820L or 316866L ("Diabetes", "Hypertension")
#   - drug_concept_id: 1503297L ("Methotrexate")

# ---------------------------------------------------------------------------
# compute_event_distribution() -- 9 tests
# ---------------------------------------------------------------------------

test_that("compute_event_distribution returns correct column names", {
  fix  <- make_test_cohort()
  res  <- compute_event_distribution(fix$cohort_members, fix$domain_data,
                                      domain = "condition",
                                      concept_id = 201820L)
  expect_named(res, c("bin_start","bin_end","bin_mid","n_patients","n_events"))
})

test_that("compute_event_distribution n_patients <= cohort size in every bin", {
  fix <- make_test_cohort()
  res <- compute_event_distribution(fix$cohort_members, fix$domain_data,
                                     domain = "condition", concept_id = 201820L)
  expect_true(all(res$n_patients <= nrow(fix$cohort_members)))
})

test_that("compute_event_distribution n_events >= 0 in every bin", {
  fix <- make_test_cohort()
  res <- compute_event_distribution(fix$cohort_members, fix$domain_data,
                                     domain = "drug", concept_id = 1503297L)
  expect_true(all(res$n_events >= 0L))
})

test_that("compute_event_distribution absent concept_id returns all-zero tibble", {
  fix <- make_test_cohort()
  res <- compute_event_distribution(fix$cohort_members, fix$domain_data,
                                     domain = "condition",
                                     concept_id = 9999999L)  # not in data
  expect_equal(nrow(res) > 0L, TRUE)         # bin grid still returned
  expect_equal(sum(res$n_patients), 0L)
  expect_equal(sum(res$n_events),   0L)
})

test_that("compute_event_distribution respects day_range lower bound", {
  fix <- make_test_cohort()
  res <- compute_event_distribution(fix$cohort_members, fix$domain_data,
                                     domain = "condition",
                                     concept_id = 201820L,
                                     day_range = c(0L, 180L))
  expect_true(all(res$bin_start >= 0L))
})

test_that("compute_event_distribution respects day_range upper bound", {
  fix <- make_test_cohort()
  res <- compute_event_distribution(fix$cohort_members, fix$domain_data,
                                     domain = "condition",
                                     concept_id = 201820L,
                                     day_range = c(-180L, 0L))
  expect_true(all(res$bin_end <= 30L))  # last bin ends at 0 + bin_width (30)
})

test_that("compute_event_distribution bin_width is respected (bin_end - bin_start)", {
  fix <- make_test_cohort()
  bw  <- 60L
  res <- compute_event_distribution(fix$cohort_members, fix$domain_data,
                                     domain = "drug", concept_id = 1503297L,
                                     bin_width = bw)
  expect_true(all(res$bin_end - res$bin_start == bw))
})

test_that("compute_event_distribution complete bin grid (no gaps)", {
  fix <- make_test_cohort()
  bw  <- 30L
  res <- compute_event_distribution(fix$cohort_members, fix$domain_data,
                                     domain = "condition",
                                     concept_id = 201820L,
                                     day_range = c(-180L, 180L),
                                     bin_width  = bw)
  diffs <- diff(sort(unique(res$bin_mid)))
  expect_true(all(diffs == bw))
})

test_that("compute_event_distribution handles empty domain tibble gracefully", {
  fix <- make_test_cohort()
  fix$domain_data$procedure <- CohortIntelligence:::.empty_cohort_domain("procedure")
  res <- compute_event_distribution(fix$cohort_members, fix$domain_data,
                                     domain = "procedure", concept_id = NULL)
  expect_equal(sum(res$n_patients), 0L)
  expect_equal(sum(res$n_events),   0L)
})

# ---------------------------------------------------------------------------
# compute_outcome_labels() -- 8 tests
# ---------------------------------------------------------------------------

test_that("compute_outcome_labels returns correct column names", {
  fix <- make_test_cohort()
  res <- compute_outcome_labels(fix$cohort_members, fix$domain_data,
                                 domain = "drug", concept_id = 1503297L)
  expect_named(res, c("subject_id","has_outcome","days_to_first_event","days_bin"))
})

test_that("compute_outcome_labels all cohort members present in result", {
  fix <- make_test_cohort()
  res <- compute_outcome_labels(fix$cohort_members, fix$domain_data,
                                 domain = "condition", concept_id = 201820L)
  expect_equal(sort(res$subject_id), sort(fix$cohort_members$subject_id))
})

test_that("compute_outcome_labels has_outcome is logical", {
  fix <- make_test_cohort()
  res <- compute_outcome_labels(fix$cohort_members, fix$domain_data,
                                 domain = "drug", concept_id = 1503297L)
  expect_true(is.logical(res$has_outcome))
})

test_that("compute_outcome_labels days_to_first_event is NA when has_outcome == FALSE", {
  fix <- make_test_cohort()
  res <- compute_outcome_labels(fix$cohort_members, fix$domain_data,
                                 domain = "drug", concept_id = 1503297L)
  # Patients without the outcome should have NA days
  expect_true(all(is.na(res$days_to_first_event[!res$has_outcome])))
  # Patients with the outcome should have non-NA days
  if (any(res$has_outcome)) {
    expect_true(all(!is.na(res$days_to_first_event[res$has_outcome])))
  }
})

test_that("compute_outcome_labels post_index_only=TRUE excludes pre-index events", {
  # All condition events in make_test_cohort() are pre-index (days -365 to -1).
  # With post_index_only = TRUE, no condition event should count as an outcome.
  fix <- make_test_cohort()

  # post-index window: should find zero outcomes because all conditions are pre-index
  res_post <- compute_outcome_labels(fix$cohort_members, fix$domain_data,
                                      domain = "condition", concept_id = 201820L,
                                      post_index_only = TRUE, day_range = c(0L, 730L))
  expect_true(all(!res_post$has_outcome))

  # pre-index window (post_index_only = FALSE): should find some outcomes for cluster A
  res_pre <- compute_outcome_labels(fix$cohort_members, fix$domain_data,
                                     domain = "condition", concept_id = 201820L,
                                     post_index_only = FALSE,
                                     day_range = c(-365L, -1L))
  # Cluster A patients (first half) have condition events in [-180:-1]
  expect_true(any(res_pre$has_outcome))
  # Any found events must be within [-365, -1] (strictly negative)
  valid_days <- res_pre$days_to_first_event[!is.na(res_pre$days_to_first_event)]
  expect_true(all(valid_days >= -365L & valid_days <= -1L))
})

test_that("compute_outcome_labels days_bin is ordered factor with correct levels", {
  fix <- make_test_cohort()
  res <- compute_outcome_labels(fix$cohort_members, fix$domain_data,
                                 domain = "drug", concept_id = 1503297L)
  expect_true(is.factor(res$days_bin))
  expect_true(is.ordered(res$days_bin))
  expect_equal(levels(res$days_bin),
               c("0-90d","91-180d","181-365d",">365d","None"))
})

test_that("compute_outcome_labels absent concept returns all-FALSE has_outcome", {
  fix <- make_test_cohort()
  res <- compute_outcome_labels(fix$cohort_members, fix$domain_data,
                                 domain = "condition", concept_id = 9999999L)
  expect_true(all(!res$has_outcome))
  expect_true(all(is.na(res$days_to_first_event)))
})

test_that("compute_outcome_labels all patients present even with empty domain", {
  fix <- make_test_cohort()
  fix$domain_data$procedure <- CohortIntelligence:::.empty_cohort_domain("procedure")
  res <- compute_outcome_labels(fix$cohort_members, fix$domain_data,
                                 domain = "procedure", concept_id = NULL)
  expect_equal(nrow(res), nrow(fix$cohort_members))
  expect_true(all(!res$has_outcome))
})
