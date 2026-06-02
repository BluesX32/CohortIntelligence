test_that("build_review_sets returns expected columns", {
  fix    <- make_test_cohort()
  result <- build_review_sets(
    rank_df         = fix$rank_df,
    domain_activity = fix$domain_activity,
    feature_matrix  = NULL,
    ml_results      = NULL,
    cohort_members  = fix$cohort_members,
    temporal_flags  = NULL,
    n_per_set       = 5L
  )
  expected <- c("review_set","subject_id","reason_for_selection",
                 "rank_score","rank_position","cluster_id",
                 "anomaly_score","sparsity_score","set_priority")
  expect_true(all(expected %in% names(result)))
})

test_that("build_review_sets review_set values are known set names", {
  fix    <- make_test_cohort(n_patients = 30L)
  result <- build_review_sets(fix$rank_df, fix$domain_activity, NULL, NULL,
                               fix$cohort_members, n_per_set = 5L)
  valid_sets <- c(
    "Typical patients","Most anomalous","Sparse follow-up",
    "Rare cluster","High post-index activity","High pre-index activity",
    "Boundary patients","Temporal concern"
  )
  expect_true(all(result$review_set %in% valid_sets))
})

test_that("build_review_sets n_per_set is respected", {
  fix    <- make_test_cohort(n_patients = 20L)
  result <- build_review_sets(fix$rank_df, fix$domain_activity, NULL, NULL,
                               fix$cohort_members, n_per_set = 3L)
  counts <- table(result$review_set)
  expect_true(all(counts <= 3L))
})

test_that("build_review_sets all subject_ids are in cohort_members", {
  fix    <- make_test_cohort()
  result <- build_review_sets(fix$rank_df, fix$domain_activity, NULL, NULL,
                               fix$cohort_members, n_per_set = 5L)
  expect_true(all(result$subject_id %in% fix$cohort_members$subject_id))
})

test_that("temporal_flag_config returns a list with required fields", {
  cfg <- temporal_flag_config()
  expect_type(cfg, "list")
  expect_true(all(c("exposure_domains","outcome_domains",
                     "recurrent_gap_days","death_window_days") %in% names(cfg)))
})

test_that("build_review_sets handles NULL rank_df gracefully", {
  fix    <- make_test_cohort()
  result <- build_review_sets(NULL, fix$domain_activity, NULL, NULL,
                               fix$cohort_members)
  expect_equal(nrow(result), 0L)
})
