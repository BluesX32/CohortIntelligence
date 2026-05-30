test_that("run_isolation_forest returns subject_id and anomaly_score", {
  skip_if_not_installed("isotree")
  fix <- make_test_cohort(n_patients = 12L)
  fm  <- build_feature_matrix(fix$cohort_members, fix$domain_data,
                               fix$time_windows, domains = c("condition","drug"))
  res <- run_isolation_forest(fm$wide, n_trees = 10L)
  expect_named(res, c("subject_id","anomaly_score"))
  expect_equal(nrow(res), 12L)
  expect_true(all(res$anomaly_score >= 0 & res$anomaly_score <= 1))
})

test_that("run_isolation_forest scores are in [0,1]", {
  skip_if_not_installed("isotree")
  fix <- make_test_cohort(n_patients = 15L)
  fm  <- build_feature_matrix(fix$cohort_members, fix$domain_data, fix$time_windows)
  if (ncol(fm$wide) <= 1L) skip("No features in test data")
  res <- run_isolation_forest(fm$wide, n_trees = 5L)
  expect_true(all(is.finite(res$anomaly_score)))
})

test_that("run_clustering returns subject_id, cluster_id, cluster_label", {
  skip_if_not_installed("cluster")
  coords <- tibble::tibble(
    subject_id = 1:20,
    umap_1     = c(rnorm(10, -2), rnorm(10,  2)),
    umap_2     = c(rnorm(10, -2), rnorm(10,  2))
  )
  res <- run_clustering(coords, method = "kmeans", k = 2L)
  expect_named(res, c("subject_id","cluster_id","cluster_label"))
  expect_equal(nrow(res), 20L)
  expect_equal(length(unique(res$cluster_id)), 2L)
})

test_that(".spectral_embed returns matrix with n rows and k cols", {
  X  <- matrix(rnorm(40), nrow = 10L)
  em <- CohortIntelligence:::.spectral_embed(X, k = 2L)
  expect_equal(dim(em), c(10L, 2L))
})

test_that("run_umap spectral fallback produces 2 columns", {
  # Force spectral method directly
  fix <- make_test_cohort(n_patients = 15L)
  fm  <- build_feature_matrix(fix$cohort_members, fix$domain_data, fix$time_windows)
  if (ncol(fm$wide) <= 1L) skip("No features")
  res <- run_umap(fm$wide, method = "spectral")
  expect_named(res, c("subject_id","umap_1","umap_2"))
  expect_equal(nrow(res), 15L)
})
