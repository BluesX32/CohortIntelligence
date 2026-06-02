test_that("detect_temporal_flags returns expected columns", {
  fix    <- make_test_cohort()
  result <- detect_temporal_flags(
    cohort_members = fix$cohort_members,
    domain_data    = fix$domain_data
  )
  expected <- c("subject_id","flag_type","flag_label","flag_description",
                 "severity","domain","event_date","days_from_index",
                 "evidence_summary","recommended_action")
  expect_true(all(expected %in% names(result)))
})

test_that("detect_temporal_flags severity values are valid", {
  fix    <- make_test_cohort()
  result <- detect_temporal_flags(fix$cohort_members, fix$domain_data)
  if (nrow(result) > 0L) {
    expect_true(all(result$severity %in% c("high","medium","low")))
  }
})

test_that("detect_temporal_flags flag_type values are non-empty strings", {
  fix    <- make_test_cohort()
  result <- detect_temporal_flags(fix$cohort_members, fix$domain_data)
  if (nrow(result) > 0L) {
    expect_true(all(nzchar(result$flag_type)))
    expect_true(all(nzchar(result$flag_label)))
  }
})

test_that("detect_temporal_flags handles all-empty domain_data", {
  fix        <- make_test_cohort(n_patients = 5L)
  empty_data <- lapply(fix$domain_data, function(d) d[integer(0), ])
  result     <- detect_temporal_flags(fix$cohort_members, empty_data)
  expect_s3_class(result, "tbl_df")
  # no_post_index_followup should flag all patients
  expect_true(nrow(result) >= nrow(fix$cohort_members))
})

test_that("temporal_flag_config overrides are respected", {
  cfg <- temporal_flag_config(death_window_days = 180L,
                               recurrent_gap_days = 7L)
  expect_equal(cfg$death_window_days, 180L)
  expect_equal(cfg$recurrent_gap_days, 7L)
})

test_that("detect_temporal_flags subject_ids are in cohort_members", {
  fix    <- make_test_cohort()
  result <- detect_temporal_flags(fix$cohort_members, fix$domain_data)
  if (nrow(result) > 0L) {
    expect_true(all(result$subject_id %in% fix$cohort_members$subject_id))
  }
})
