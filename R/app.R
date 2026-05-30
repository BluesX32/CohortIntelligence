# app.R
# Entry point for launching the CohortIntelligence Shiny dashboard.

#' Launch the CohortIntelligence Shiny dashboard
#'
#' @param connection_details A `connectionDetails` object from
#'   `DatabaseConnector::createConnectionDetails()`. `NULL` starts demo mode.
#' @param cdm_schema Schema containing OMOP CDM tables.
#' @param cohort_schema Schema containing the cohort table (results schema).
#' @param vocab_schema Vocabulary schema. Defaults to `cdm_schema`.
#' @param cohort_table Name of the cohort table. Default `"cohort"`.
#' @param cohort_definition_id Cohort definition ID. Default `1L`.
#' @param ... Additional arguments forwarded to [shiny::runApp()].
#' @export
launch_cohort_intelligence <- function(connection_details   = NULL,
                                        cdm_schema           = NULL,
                                        cohort_schema        = NULL,
                                        vocab_schema         = cdm_schema,
                                        cohort_table         = "cohort",
                                        cohort_definition_id = 1L,
                                        ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required to launch the dashboard.")
  }

  app_dir <- system.file("shiny", package = "CohortIntelligence")
  if (!nzchar(app_dir)) {
    app_dir <- file.path(getwd(), "inst", "shiny")
  }

  .cohort_intel_env$connection_details   <- connection_details
  .cohort_intel_env$cdm_schema           <- cdm_schema
  .cohort_intel_env$cohort_schema        <- cohort_schema
  .cohort_intel_env$vocab_schema         <- vocab_schema
  .cohort_intel_env$cohort_table         <- cohort_table
  .cohort_intel_env$cohort_definition_id <- as.integer(cohort_definition_id)

  shiny::runApp(app_dir, ...)
}

.cohort_intel_env <- new.env(parent = emptyenv())
