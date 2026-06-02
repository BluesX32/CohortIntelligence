test_that("explain_patient_priority returns correct columns", {
  fix <- make_test_cohort()
  pid <- fix$cohort_members$subject_id[[1L]]
  result <- explain_patient_priority(
    subject_id     = pid,
    rank_df        = fix$rank_df,
    feature_matrix = NULL,
    domain_activity = fix$domain_activity,
    cohort_members  = fix$cohort_members,
    ml_results      = NULL,
    top_n           = 5L
  )
  expected_cols <- c("subject_id","explanation_type","explanation_label",
                      "explanation_detail","domain","window_label",
                      "importance_score","severity")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("explain_patient_priority returns at most top_n rows", {
  fix    <- make_test_cohort()
  pid    <- fix$rank_df$subject_id[which.max(fix$rank_df$anomaly_score)]
  result <- explain_patient_priority(
    pid, fix$rank_df, NULL, fix$domain_activity,
    fix$cohort_members, NULL, top_n = 3L
  )
  expect_lte(nrow(result), 3L)
})

test_that("explain_patient_priority severity values are valid", {
  fix    <- make_test_cohort()
  pid    <- fix$cohort_members$subject_id[[1L]]
  result <- explain_patient_priority(
    pid, fix$rank_df, NULL, fix$domain_activity, fix$cohort_members
  )
  if (nrow(result) > 0L) {
    expect_true(all(result$severity %in% c("high","medium","low")))
  }
})

test_that("explain_patient_priority handles missing patient gracefully", {
  fix    <- make_test_cohort()
  result <- explain_patient_priority(
    999999L, fix$rank_df, NULL, fix$domain_activity, fix$cohort_members
  )
  expect_equal(nrow(result), 0L)
})

test_that("explain_patient_priority returns tibble even with sparse data", {
  fix  <- make_test_cohort(n_patients = 3L)
  zero <- dplyr::mutate(fix$domain_activity, event_count = 0L)
  pid  <- fix$cohort_members$subject_id[[1L]]
  result <- explain_patient_priority(
    pid, fix$rank_df, NULL, zero, fix$cohort_members
  )
  expect_s3_class(result, "tbl_df")
})
