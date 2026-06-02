test_that("build_cluster_profiles returns summary and concepts tibbles", {
  fix <- make_test_cohort()
  result <- build_cluster_profiles(
    rank_df        = fix$rank_df,
    domain_data    = fix$domain_data,
    cohort_members = fix$cohort_members,
    person_data    = NULL,
    top_n          = 5L,
    domains        = c("condition","drug")
  )
  expect_true(is.list(result))
  expect_true(all(c("summary","concepts") %in% names(result)))
  expect_s3_class(result$summary,  "tbl_df")
  expect_s3_class(result$concepts, "tbl_df")
})

test_that("build_cluster_profiles summary has required columns", {
  fix    <- make_test_cohort()
  result <- build_cluster_profiles(fix$rank_df, fix$domain_data,
                                    fix$cohort_members)
  required <- c("cluster_id","n_patients","pct_cohort")
  expect_true(all(required %in% names(result$summary)))
})

test_that("build_cluster_profiles n_patients sums to cohort size", {
  fix    <- make_test_cohort(n_patients = 20L)
  result <- build_cluster_profiles(fix$rank_df, fix$domain_data,
                                    fix$cohort_members)
  expect_equal(sum(result$summary$n_patients), 20L)
})

test_that("label_clusters returns named vector with valid labels", {
  fix     <- make_test_cohort()
  prof    <- build_cluster_profiles(fix$rank_df, fix$domain_data,
                                     fix$cohort_members)
  labels  <- label_clusters(prof)
  expect_type(labels, "character")
  expect_true(length(labels) == nrow(prof$summary))
  expect_true(all(nzchar(labels)))
})

test_that("summarize_cluster_profile returns non-empty string", {
  fix  <- make_test_cohort()
  prof <- build_cluster_profiles(fix$rank_df, fix$domain_data,
                                  fix$cohort_members)
  if (nrow(prof$summary) > 0L) {
    txt <- summarize_cluster_profile(prof$summary[1L, ], prof$concepts)
    expect_type(txt, "character")
    expect_true(nzchar(txt))
    expect_true(grepl("Requires clinical review", txt, ignore.case = TRUE))
  }
})

test_that("compare_cluster_profiles returns expected columns", {
  fix  <- make_test_cohort()
  prof <- build_cluster_profiles(fix$rank_df, fix$domain_data,
                                  fix$cohort_members)
  clusters <- unique(prof$summary$cluster_id)
  if (length(clusters) >= 2L) {
    result <- compare_cluster_profiles(prof$concepts,
                                        clusters[1], clusters[2], top_n = 3)
    if (nrow(result) > 0L) {
      expect_true(all(c("concept_name","domain","prev_a","prev_b",
                         "ratio","direction") %in% names(result)))
    }
  }
})

test_that("build_cluster_profiles handles empty rank_df gracefully", {
  fix    <- make_test_cohort(n_patients = 5L)
  empty  <- fix$rank_df[integer(0), ]
  result <- build_cluster_profiles(empty, fix$domain_data, fix$cohort_members)
  expect_equal(nrow(result$summary), 0L)
})
