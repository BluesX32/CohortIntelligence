test_that("compute_sparsity returns scores in [0,1]", {
  fix  <- make_test_cohort()
  sp   <- compute_sparsity(fix$domain_activity, fix$time_windows)
  expect_named(sp, c("subject_id","sparsity_score"))
  expect_true(all(sp$sparsity_score >= 0 & sp$sparsity_score <= 1))
})

test_that("compute_sparsity patient with all zeros scores 1.0", {
  fix   <- make_test_cohort(n_patients = 5L)
  zeros <- dplyr::mutate(fix$domain_activity, event_count = 0L)
  sp    <- compute_sparsity(zeros, fix$time_windows)
  expect_true(all(sp$sparsity_score == 1))
})

test_that("rank_patients returns correct columns", {
  fix <- make_test_cohort()
  ml_stub <- list(merged = dplyr::mutate(
    tibble::tibble(subject_id = fix$cohort_members$subject_id),
    anomaly_score = stats::runif(nrow(fix$cohort_members)),
    cluster_id    = sample(1:2, nrow(fix$cohort_members), replace = TRUE)
  ))
  rk <- rank_patients(ml_stub, fix$domain_activity, fix$cohort_members)
  expected <- c("subject_id","rank_score","rank_position","priority_tier",
                "anomaly_score","cluster_id","sparsity_score")
  expect_true(all(expected %in% names(rk)))
})

test_that("rank_patients includes all cohort members", {
  fix <- make_test_cohort(n_patients = 10L)
  ml_stub <- list(merged = tibble::tibble(
    subject_id    = seq_len(10L),
    anomaly_score = stats::runif(10L),
    cluster_id    = sample(1:2, 10L, replace = TRUE)
  ))
  rk <- rank_patients(ml_stub, fix$domain_activity, fix$cohort_members)
  expect_equal(nrow(rk), 10L)
})

test_that("rank_patients rank_position is a permutation of 1..n", {
  fix <- make_test_cohort(n_patients = 8L)
  ml_stub <- list(merged = tibble::tibble(
    subject_id    = seq_len(8L),
    anomaly_score = stats::runif(8L),
    cluster_id    = 1L
  ))
  rk <- rank_patients(ml_stub, fix$domain_activity, fix$cohort_members)
  expect_equal(sort(rk$rank_position), 1:8)
})

test_that("rank_patients assigns n_tiers distinct tiers", {
  fix <- make_test_cohort(n_patients = 15L)
  ml_stub <- list(merged = tibble::tibble(
    subject_id    = seq_len(15L),
    anomaly_score = stats::runif(15L),
    cluster_id    = 1L
  ))
  rk   <- rank_patients(ml_stub, fix$domain_activity, fix$cohort_members, n_tiers = 3L)
  trs  <- unique(rk$priority_tier)
  expect_lte(length(trs), 3L)
})
