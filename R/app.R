# app.R
# Entry point for launching the CohortIntelligence Shiny dashboard.

#' Launch the CohortIntelligence Shiny dashboard
#'
#' The recommended workflow:
#' ```r
#' connection_details <- DatabaseConnector::createConnectionDetails(...)
#' connection         <- DatabaseConnector::connect(connection_details)
#'
#' launch_cohort_intelligence(
#'   connection = connection,
#'   cdm_schema = "cdm",
#'   vocab_schema = "vocab",
#'   json_path  = "inst/template/DM_infection.json"
#' )
#'
#' DatabaseConnector::disconnect(connection)
#' ```
#'
#' @param connection A live connection from `DatabaseConnector::connect()`.
#'   `NULL` starts demo mode with 50 synthetic patients.
#' @param cdm_schema Schema containing OMOP CDM tables.
#' @param vocab_schema Vocabulary schema. Defaults to `cdm_schema`.
#' @param json_path Path to an ATLAS cohort definition JSON file. When
#'   supplied, the cohort is instantiated from the JSON -- no `cohort_schema`
#'   or `cohort_table` is required.
#' @param cohort_schema Schema containing a pre-built cohort table. Only
#'   used when `json_path` is `NULL`.
#' @param cohort_table Name of the pre-built cohort table. Default `"cohort"`.
#' @param cohort_definition_id Cohort definition ID. Default `1L`.
#' @export
launch_cohort_intelligence <- function(
    connection           = NULL,
    cdm_schema           = NULL,
    vocab_schema         = cdm_schema,
    json_path            = NULL,
    cohort_schema        = NULL,
    cohort_table         = "cohort",
    cohort_definition_id = 1L) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required to launch the dashboard.")
  }

  app_dir <- system.file("shiny", package = "CohortIntelligence")
  if (!nzchar(app_dir)) {
    app_dir <- file.path(getwd(), "inst", "shiny")
  }

  .cohort_intel_env$connection           <- connection
  .cohort_intel_env$cdm_schema           <- cdm_schema
  .cohort_intel_env$vocab_schema         <- vocab_schema
  .cohort_intel_env$json_path            <- json_path
  .cohort_intel_env$cohort_schema        <- cohort_schema
  .cohort_intel_env$cohort_table         <- cohort_table
  .cohort_intel_env$cohort_definition_id <- as.integer(cohort_definition_id)

  shiny::runApp(app_dir)
}

.cohort_intel_env <- new.env(parent = emptyenv())
