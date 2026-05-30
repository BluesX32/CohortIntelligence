test_that("quilt has one row per patient * domain * window", {
  fix   <- make_test_cohort(n_patients = 10L)
  q     <- fix$quilt_base
  n_pat <- length(unique(q$subject_id))
  n_dom <- length(unique(q$domain))
  n_win <- length(unique(q$window_label))
  expect_equal(nrow(q), n_pat * n_dom * n_win)
})

test_that("quilt patient_row is contiguous 1..N", {
  q    <- make_test_cohort()$quilt_base
  rows <- sort(unique(q$patient_row))
  expect_equal(rows, seq_along(rows))
})

test_that("quilt binary encoding fills only 0 and 1", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df, value_encoding = "binary")
  expect_true(all(q$fill_value %in% c(0, 1)))
})

test_that("quilt log1p fill_value is >= 0", {
  q <- make_test_cohort()$quilt_base
  expect_true(all(q$fill_value >= 0))
})

test_that("quilt count encoding equals event_count", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df, value_encoding = "count")
  expect_equal(q$fill_value, as.numeric(q$event_count))
})

test_that("quilt clip_max clips fill values", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df,
                           value_encoding = "count", clip_max = 3L)
  expect_true(all(q$fill_value <= 3))
})

test_that("quilt domain filter restricts domains", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df, domains = c("drug"))
  expect_equal(unique(q$domain), "drug")
})

test_that("quilt sort_by cluster produces contiguous cluster blocks", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df, sort_by = "cluster")
  ord <- dplyr::distinct(q, subject_id, patient_row, cluster_id) |>
    dplyr::arrange(patient_row)
  runs <- rle(ord$cluster_id)
  expect_equal(length(runs$lengths), length(unique(ord$cluster_id)))
})

test_that("quilt sort_by rank gives monotone rank_position", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity, fix$rank_df, sort_by = "rank")
  ord <- dplyr::distinct(q, subject_id, patient_row, rank_position) |>
    dplyr::arrange(patient_row)
  expect_equal(ord$rank_position, sort(ord$rank_position))
})

test_that("quilt handles n=1 patient", {
  fix <- make_test_cohort(n_patients = 1L)
  expect_no_error(fix$quilt_base)
  expect_equal(length(unique(fix$quilt_base$subject_id)), 1L)
})

test_that("quilt all-zero activity gives all-zero fill_value", {
  fix  <- make_test_cohort()
  zero <- dplyr::mutate(fix$domain_activity, event_count = 0L)
  q    <- build_quilt_data(zero, fix$rank_df)
  expect_true(all(q$fill_value == 0))
})

test_that("quilt works without rank_df argument", {
  fix <- make_test_cohort()
  q   <- build_quilt_data(fix$domain_activity)
  expect_true(is.data.frame(q))
  expect_true(nrow(q) > 0L)
})

test_that("quilt display_label is non-empty character", {
  q <- make_test_cohort()$quilt_base
  expect_true(all(nzchar(q$display_label)))
  expect_type(q$display_label, "character")
})

test_that("quilt window_idx matches time_windows ordering", {
  fix <- make_test_cohort()
  q   <- fix$quilt_base
  win_order <- dplyr::distinct(q, window_label, window_idx) |>
    dplyr::arrange(window_idx)
  expect_equal(win_order$window_idx, seq_along(win_order$window_idx))
})
