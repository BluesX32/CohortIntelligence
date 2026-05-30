test_that("generate_hypotheses returns empty tibble when fewer than 2 clusters", {
  fix <- make_test_cohort()
  fm  <- build_feature_matrix(fix$cohort_members, fix$domain_data, fix$time_windows)
  # Single cluster
  clusters_single <- tibble::tibble(
    subject_id    = fix$cohort_members$subject_id,
    cluster_id    = 1L,
    cluster_label = "Cluster 1"
  )
  res <- generate_hypotheses(fm, clusters_single)
  expect_equal(nrow(res), 0L)
})

test_that("generate_hypotheses returns correct columns", {
  fix <- make_test_cohort()
  fm  <- build_feature_matrix(fix$cohort_members, fix$domain_data, fix$time_windows,
                               min_concept_freq = 2L)
  if (nrow(fm$long) == 0L) skip("Insufficient data for hypothesis test")
  clusters <- dplyr::select(fix$rank_df, subject_id, cluster_id, cluster_label = priority_tier) |>
    dplyr::mutate(cluster_id = fix$rank_df$cluster_id)
  res <- generate_hypotheses(list(long = fm$long), clusters,
                              min_effect_size = 0, max_hypotheses = 5L)
  if (nrow(res) > 0L) {
    expected <- c("hypothesis_id","cluster_a","cluster_b","domain","concept_name",
                  "window_label","effect_size","p_value_raw","p_value_adjusted",
                  "direction","description_text")
    expect_true(all(expected %in% names(res)))
  }
})

test_that("format_hypotheses_report text format returns character", {
  fix <- make_test_cohort()
  empty_hyp <- generate_hypotheses(list(long = tibble::tibble()), NULL)
  txt <- format_hypotheses_report(empty_hyp, format = "text")
  expect_type(txt, "character")
})

test_that("format_hypotheses_report tibble format returns data.frame", {
  fix <- make_test_cohort()
  fm  <- build_feature_matrix(fix$cohort_members, fix$domain_data, fix$time_windows)
  res <- generate_hypotheses(fm, fix$rank_df, min_effect_size = 0, max_hypotheses = 3L)
  out <- format_hypotheses_report(res, format = "tibble")
  expect_true(is.data.frame(out))
})
