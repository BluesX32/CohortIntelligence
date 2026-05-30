# cohort.R
# Cohort instantiation from ATLAS JSON (CirceR format).
# Requires CirceR and SqlRender; DatabaseConnector for live OMOP.

#' Instantiate a cohort from an ATLAS JSON file and return cohort members
#'
#' Reads an ATLAS cohort definition JSON, generates the cohort SQL via
#' `CirceR`, executes it against the CDM into a session-scoped temp table,
#' and returns the resulting cohort member tibble. No permanent cohort schema
#' or cohort table is required.
#'
#' @param connector A `cohort_omop_connector` from
#'   [create_cohort_omop_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param cohort_id Integer. ID assigned to this cohort in the temp table.
#'   Default `1L`.
#'
#' @return tibble(subject_id, cohort_start_date, cohort_end_date)
#' @export
fetch_cohort_from_json <- function(connector, json_path, cohort_id = 1L) {
  if (!requireNamespace("CirceR", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'CirceR' is required to instantiate cohorts from JSON.\n",
      "Install with: remotes::install_github('OHDSI/CirceR')"
    ))
  }
  if (!requireNamespace("SqlRender", quietly = TRUE)) {
    rlang::abort("Package 'SqlRender' is required.")
  }
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
    rlang::abort("Package 'DatabaseConnector' is required.")
  }
  if (!file.exists(json_path)) {
    rlang::abort(paste0("JSON file not found: ", json_path))
  }

  json_str   <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
  expression <- CirceR::cohortExpressionFromJson(json_str)
  options    <- CirceR::createGenerateOptions(generateStats = FALSE)
  cohort_sql <- CirceR::buildCohortQuery(expression, options = options)

  with_cohort_connector(connector, function(active) {
    dbms <- active$dbms %||% "sql server"

    # Render with CDM and vocabulary schemas, writing into a temp table
    sql <- SqlRender::render(
      cohort_sql,
      cdm_database_schema         = active$cdm_schema,
      vocabulary_database_schema  = active$vocab_schema,
      target_database_schema      = "#",
      target_cohort_table         = "cohort_ci_tmp",
      target_cohort_id            = as.integer(cohort_id),
      results_database_schema     = active$cdm_schema
    )
    sql <- SqlRender::translate(sql, targetDialect = dbms)
    DatabaseConnector::executeSql(active$conn, sql, progressBar = FALSE,
                                   reportOverallTime = FALSE)

    # Extract members from the temp table
    fetch_sql <- SqlRender::translate(
      SqlRender::render(
        "SELECT subject_id,
                CAST(cohort_start_date AS DATE) AS cohort_start_date,
                CAST(cohort_end_date   AS DATE) AS cohort_end_date
         FROM #cohort_ci_tmp
         WHERE cohort_definition_id = @cohort_id
         ORDER BY subject_id;",
        cohort_id = as.integer(cohort_id)
      ),
      targetDialect = dbms
    )

    df <- if (inherits(active$conn, "JDBCConnection")) {
      as.data.frame(DBI::dbGetQuery(active$conn, fetch_sql))
    } else {
      DatabaseConnector::querySql(active$conn, fetch_sql,
                                   snakeCaseToCamelCase = FALSE)
    }

    names(df) <- tolower(names(df))
    for (col in c("cohort_start_date", "cohort_end_date")) {
      if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
    }
    tibble::as_tibble(df)
  })
}

#' List ATLAS JSON cohort templates bundled with CohortIntelligence
#'
#' @return Named character vector mapping cohort names to file paths.
#' @export
list_cohort_templates <- function() {
  template_dir <- system.file("template", package = "CohortIntelligence")
  if (!nzchar(template_dir)) {
    template_dir <- file.path(getwd(), "inst", "template")
  }
  files <- list.files(template_dir, pattern = "\\.json$", full.names = TRUE)
  stats::setNames(files, tools::file_path_sans_ext(basename(files)))
}
