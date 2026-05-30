test_that("define_time_windows returns correct structure", {
  tw <- define_time_windows(c(-12, -6, 0, 6))
  expect_equal(nrow(tw), 3L)
  expect_named(tw, c("window_idx","window_label","months_start","months_end"))
  expect_equal(tw$window_idx, 1:3)
})

test_that("define_time_windows auto-generates labels", {
  tw <- define_time_windows(c(-6, 0, 6))
  expect_equal(length(tw$window_label), 2L)
  expect_true(all(nzchar(tw$window_label)))
})

test_that("define_time_windows errors on too few breaks", {
  expect_error(define_time_windows(c(-6)))
})

test_that("define_time_windows errors on wrong label length", {
  expect_error(define_time_windows(c(-6, 0, 6), labels = c("only_one")))
})

test_that("build_domain_activity returns expected columns", {
  fix <- make_test_cohort()
  act <- fix$domain_activity
  expect_named(act, c("subject_id","domain","window_label","window_idx","event_count"))
})

test_that("build_domain_activity produces n_patients * n_domains * n_windows rows", {
  fix      <- make_test_cohort(n_patients = 10L)
  n_pat    <- length(unique(fix$domain_activity$subject_id))
  n_dom    <- length(unique(fix$domain_activity$domain))
  n_win    <- nrow(fix$time_windows)
  expect_equal(nrow(fix$domain_activity), n_pat * n_dom * n_win)
})

test_that("build_domain_activity event_count is non-negative integer", {
  fix <- make_test_cohort()
  expect_true(all(fix$domain_activity$event_count >= 0L))
  expect_true(is.integer(fix$domain_activity$event_count))
})

test_that("build_quilt_data returns expected columns", {
  fix <- make_test_cohort()
  q   <- fix$quilt_base
  expected <- c("subject_id","patient_row","display_label","domain",
                "window_label","window_idx","event_count","fill_value",
                "cluster_id","priority_tier","rank_position")
  expect_true(all(expected %in% names(q)))
})

test_that("build_quilt_data has one row per patient * domain * window", {
  fix   <- make_test_cohort(n_patients = 10L)
  q     <- fix$quilt_base
  n_pat <- length(unique(q$subject_id))
  n_dom <- length(unique(q$domain))
  n_win <- length(unique(q$window_label))
  expect_equal(nrow(q), n_pat * n_dom * n_win)
})

test_that("patient_row is contiguous integers 1..N", {
  fix  <- make_test_cohort()
  rows <- sort(unique(fix$quilt_base$patient_row))
  expect_equal(rows, seq_along(rows))
})

test_that("build_quilt_data binary encoding produces only 0 and 1", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df, value_encoding = "binary")
  expect_true(all(q$fill_value %in% c(0, 1)))
})

test_that("build_quilt_data log1p encoding is monotone with event_count", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df, value_encoding = "log1p_count")
  # For any domain+window: higher count => higher fill_value
  sub <- dplyr::filter(q, domain == "condition")
  expect_true(all(order(sub$event_count) == order(sub$fill_value) | sub$event_count == 0))
})

test_that("build_quilt_data clip_max works", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df,
                           value_encoding = "count", clip_max = 2L)
  expect_true(all(q$fill_value <= 2))
})

test_that("build_quilt_data domain filter works", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df, domains = c("condition"))
  expect_equal(unique(q$domain), "condition")
})

test_that("build_quilt_data handles single patient", {
  fix <- make_test_cohort(n_patients = 1L)
  expect_no_error(build_quilt_data(fix$domain_activity, fix$rank_df))
})

test_that("build_quilt_data handles all-zero domain activity", {
  fix  <- make_test_cohort()
  zero <- dplyr::mutate(fix$domain_activity, event_count = 0L)
  q    <- build_quilt_data(zero, fix$rank_df)
  expect_true(all(q$fill_value == 0))
})

test_that("build_quilt_data sort_by cluster groups clusters contiguously", {
  fix  <- make_test_cohort()
  q    <- build_quilt_data(fix$domain_activity, fix$rank_df, sort_by = "cluster")
  ord  <- dplyr::distinct(q, subject_id, patient_row, cluster_id) |>
    dplyr::arrange(patient_row)
  runs <- rle(ord$cluster_id)
  # Each cluster_id should appear exactly once as a contiguous run
  expect_equal(length(runs$lengths), length(unique(ord$cluster_id)))
})

test_that("build_quilt_data works without rank_df", {
  fix <- make_test_cohort()
  expect_no_error(build_quilt_data(fix$domain_activity))
})
