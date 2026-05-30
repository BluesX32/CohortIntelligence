test_that("create_cohort_df_connector accepts a valid list", {
  cd <- list(
    cohort    = tibble::tibble(cohort_definition_id = 1L,
                               subject_id = 1L,
                               cohort_start_date = as.Date("2020-01-01"),
                               cohort_end_date   = as.Date("2021-01-01")),
    condition = tibble::tibble()
  )
  con <- create_cohort_df_connector(cd)
  expect_s3_class(con, "cohort_df_connector")
  expect_s3_class(con, "cohort_connector")
})

test_that("create_cohort_df_connector fills missing slots with empty tibbles", {
  con <- create_cohort_df_connector(list())
  expect_true(is.data.frame(con$cohort_data$condition))
  expect_equal(nrow(con$cohort_data$condition), 0L)
})

test_that("create_cohort_df_connector errors on non-list input", {
  expect_error(create_cohort_df_connector("not a list"))
})

test_that("create_cohort_df_connector errors on non-data-frame slot", {
  expect_error(create_cohort_df_connector(list(cohort = "not a df")))
})

test_that("with_cohort_connector.df_connector calls fn directly", {
  con <- create_cohort_df_connector(list())
  result <- with_cohort_connector(con, function(c) "called")
  expect_equal(result, "called")
})

test_that(".empty_cohort_domain returns typed zero-row tibbles", {
  domains <- c("cohort","person","condition","drug","procedure",
               "measurement","observation","visit","death")
  for (d in domains) {
    e <- CohortIntelligence:::.empty_cohort_domain(d)
    expect_true(is.data.frame(e), info = d)
    expect_equal(nrow(e), 0L, info = d)
  }
})

test_that("print.cohort_df_connector runs without error", {
  con <- create_cohort_df_connector(list())
  expect_output(print(con), "cohort_df_connector")
})
