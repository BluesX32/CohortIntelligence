test_that("export_cohort_results writes files to disk", {
  fix    <- make_test_cohort()
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE))

  result <- list(
    feature_matrix = list(
      long = fix$domain_activity,
      wide = tibble::tibble(subject_id = fix$cohort_members$subject_id)
    ),
    rank = fix$rank_df
  )
  paths <- export_cohort_results(result, tmpdir, formats = "csv",
                                   overwrite = TRUE)
  expect_true(length(paths) > 0L)
  expect_true(all(file.exists(paths)))
})

test_that("export_quilt_plot writes a PNG file", {
  skip_if_not_installed("ggplot2")
  fix <- make_test_cohort()
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  export_quilt_plot(fix$quilt_base, path = tmp, format = "png", dpi = 72)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 1000L)
})

test_that("export_cohort_report creates an HTML file", {
  skip_if_not_installed("htmltools")
  fix  <- make_test_cohort()
  tmp  <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  export_cohort_report(
    results = list(
      cohort_members = fix$cohort_members,
      rank_df        = fix$rank_df,
      quilt_base     = fix$quilt_base,
      person_data    = NULL,
      domain_data    = fix$domain_data,
      ml_results     = NULL,
      hypotheses     = NULL
    ),
    path        = tmp,
    cohort_name = "Test Cohort"
  )
  expect_true(file.exists(tmp))
  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("Test Cohort", content))
  expect_true(grepl("DOCTYPE html", content))
})

test_that("export_clinician_review_packet creates an HTML file", {
  skip_if_not_installed("htmltools")
  fix  <- make_test_cohort()
  tmp  <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  export_clinician_review_packet(
    results = list(
      cohort_members   = fix$cohort_members,
      rank_df          = fix$rank_df,
      quilt_base       = fix$quilt_base,
      person_data      = NULL,
      domain_data      = fix$domain_data,
      ml_results       = NULL,
      hypotheses       = NULL,
      temporal_flags   = NULL,
      review_sets      = NULL,
      cluster_profiles = NULL
    ),
    path        = tmp,
    cohort_name = "Clinician Test",
    n_patients  = 3L
  )
  expect_true(file.exists(tmp))
  content <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_true(grepl("Clinician Test", content))
  expect_true(grepl("hypothesis-generating", content, ignore.case = TRUE))
})

test_that("build_clinician_review_packet returns a named list", {
  fix    <- make_test_cohort()
  packet <- build_clinician_review_packet(
    cohort_summary    = NULL,
    cluster_profiles  = NULL,
    review_sets       = NULL,
    temporal_flags    = NULL,
    hypotheses        = NULL,
    selected_patients = fix$cohort_members$subject_id[1:3],
    patient_timelines = list()
  )
  expect_type(packet, "list")
  expect_true("generated_at" %in% names(packet))
  expect_equal(length(packet$selected_patients), 3L)
})
