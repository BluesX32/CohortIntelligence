test_that("extract_cohort_members returns correct columns", {
  fix <- make_test_cohort(n_patients = 5L)
  members <- extract_cohort_members(fix$connector)
  expect_true(all(c("subject_id","cohort_start_date","cohort_end_date") %in% names(members)))
})

test_that("extract_cohort_members returns correct number of patients", {
  fix <- make_test_cohort(n_patients = 10L)
  members <- extract_cohort_members(fix$connector)
  expect_equal(nrow(members), 10L)
})

test_that("extract_cohort_members filters by cohort_definition_id", {
  cd <- list(
    cohort = tibble::tibble(
      cohort_definition_id = c(1L, 1L, 2L),
      subject_id           = 1:3L,
      cohort_start_date    = as.Date("2020-01-01"),
      cohort_end_date      = as.Date("2021-01-01")
    )
  )
  con <- create_cohort_df_connector(cd)
  m1  <- extract_cohort_members(con, cohort_definition_id = 1L)
  expect_equal(nrow(m1), 2L)
  m2  <- extract_cohort_members(con, cohort_definition_id = 2L)
  expect_equal(nrow(m2), 1L)
})

test_that("extract_omop_domains returns named list with requested domains", {
  fix <- make_test_cohort(n_patients = 5L)
  domains <- c("condition","drug")
  result  <- extract_omop_domains(fix$connector, fix$cohort_members$subject_id,
                                   domains = domains)
  expect_named(result, domains)
})

test_that("extract_omop_domains df_connector filters by subject_id", {
  fix    <- make_test_cohort(n_patients = 10L)
  result <- extract_omop_domains(fix$connector, subject_ids = c(1L, 2L),
                                  domains = c("condition"))
  cond   <- result$condition
  if (nrow(cond) > 0L) {
    expect_true(all(cond$person_id %in% c(1L, 2L)))
  } else {
    expect_equal(nrow(cond), 0L)
  }
})

test_that("extract_person_demographics df_connector filters by person_id", {
  fix <- make_test_cohort(n_patients = 5L)
  pd  <- extract_person_demographics(fix$connector, subject_ids = c(1L, 2L))
  expect_true(nrow(pd) == 0L || all(pd$person_id %in% c(1L, 2L)))
})
