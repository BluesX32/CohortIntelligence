# cohort.R
# Cohort instantiation from ATLAS JSON (CirceR format).
# Requires CirceR, SqlRender, and DatabaseConnector.

# ---------------------------------------------------------------------------
# Internal: safe DBMS detection
# ---------------------------------------------------------------------------

.get_dbms <- function(active) {
  dbms <- active$dbms
  if (!is.null(dbms) && length(dbms) == 1L && nzchar(dbms)) return(dbms)
  tryCatch(
    {
      d <- DatabaseConnector::dbms(active$conn)
      if (length(d) == 1L && nzchar(d)) d else "sql server"
    },
    error = function(e) {
      if (inherits(active$conn, "JDBCConnection")) "spark" else "sql server"
    }
  )
}

# ---------------------------------------------------------------------------
# fetch_cohort_from_json
# ---------------------------------------------------------------------------

#' Instantiate a cohort from an ATLAS JSON file and return cohort members
#'
#' Reads an ATLAS cohort definition JSON, generates SQL via `CirceR`, and
#' executes it against the CDM. For SQL Server, a session temp table is used.
#' For all other platforms (Spark, Databricks, PostgreSQL, etc.) the cohort
#' is written to a permanent table in `cdm_schema` with a timestamp-based
#' name, queried, and then dropped.
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param cohort_id Integer. Cohort definition ID used in the generated table.
#'   Default `1L`.
#' @param verbose Logical. Print the generated SQL before executing. Useful
#'   for diagnosing SQL errors. Default `FALSE`.
#'
#' @return tibble(subject_id, cohort_start_date, cohort_end_date)
#' @export
fetch_cohort_from_json <- function(connector,
                                    json_path,
                                    cohort_id = 1L,
                                    verbose   = FALSE) {
  if (!requireNamespace("CirceR",           quietly = TRUE)) {
    rlang::abort(
      "Package 'CirceR' required. Install: remotes::install_github('OHDSI/CirceR')"
    )
  }
  if (!requireNamespace("SqlRender",        quietly = TRUE)) {
    rlang::abort("Package 'SqlRender' required.")
  }
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
    rlang::abort("Package 'DatabaseConnector' required.")
  }
  if (!file.exists(json_path)) {
    rlang::abort(paste0("JSON file not found: ", json_path))
  }

  json_str   <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
  expression <- CirceR::cohortExpressionFromJson(json_str)
  options    <- CirceR::createGenerateOptions(generateStats = FALSE)
  cohort_sql <- CirceR::buildCohortQuery(expression, options = options)

  with_cohort_connector(connector, function(active) {
    dbms <- .get_dbms(active)

    # For SQL Server, use a session temp table (#).
    # For all other DBMS (Spark, PostgreSQL, etc.) write to a permanent table
    # in cdm_schema with a unique name, then drop it after querying.
    use_temp <- identical(dbms, "sql server")
    if (use_temp) {
      target_schema <- "#"
      target_table  <- "cohort_ci_tmp"
    } else {
      target_schema <- active$cdm_schema
      target_table  <- paste0(
        "cohort_ci_",
        format(Sys.time(), "%Y%m%d%H%M%S"),
        "_", as.integer(cohort_id)
      )
    }

    sql <- SqlRender::render(
      cohort_sql,
      cdm_database_schema        = active$cdm_schema,
      vocabulary_database_schema = active$vocab_schema,
      target_database_schema     = target_schema,
      target_cohort_table        = target_table,
      target_cohort_id           = as.integer(cohort_id),
      results_database_schema    = active$cdm_schema
    )
    sql <- SqlRender::translate(sql, targetDialect = dbms)

    if (verbose) {
      message("--- Generated cohort SQL ---")
      message(sql)
      message("--- End SQL ---")
    }

    tryCatch(
      DatabaseConnector::executeSql(active$conn, sql,
                                     progressBar       = FALSE,
                                     reportOverallTime = FALSE),
      error = function(e) {
        rlang::abort(paste0(
          "Cohort SQL execution failed.\n",
          "DBMS: ", dbms, "\n",
          "CDM schema: ", active$cdm_schema, "\n",
          "Vocab schema: ", active$vocab_schema, "\n",
          "Use fetch_cohort_from_json(..., verbose = TRUE) to inspect the SQL.\n",
          "Original error: ", conditionMessage(e)
        ))
      }
    )

    # Query the resulting cohort table
    full_table <- if (use_temp) {
      paste0("#", target_table)
    } else {
      paste0(target_schema, ".", target_table)
    }

    fetch_sql <- SqlRender::translate(
      SqlRender::render(
        "SELECT subject_id,
                CAST(cohort_start_date AS DATE) AS cohort_start_date,
                CAST(cohort_end_date   AS DATE) AS cohort_end_date
         FROM @full_table
         WHERE cohort_definition_id = @cohort_id
         ORDER BY subject_id;",
        full_table = full_table,
        cohort_id  = as.integer(cohort_id)
      ),
      targetDialect = dbms
    )

    df <- tryCatch(
      if (inherits(active$conn, "JDBCConnection")) {
        as.data.frame(DBI::dbGetQuery(active$conn, fetch_sql))
      } else {
        DatabaseConnector::querySql(active$conn, fetch_sql,
                                     snakeCaseToCamelCase = FALSE)
      },
      error = function(e) {
        rlang::abort(paste0(
          "Failed to query cohort results from '", full_table, "'.\n",
          "Original error: ", conditionMessage(e)
        ))
      }
    )

    # Drop the permanent table (non-SQL Server only)
    if (!use_temp) {
      drop_sql <- SqlRender::translate(
        SqlRender::render(
          "DROP TABLE IF EXISTS @full_table;",
          full_table = full_table
        ),
        targetDialect = dbms
      )
      tryCatch(
        DatabaseConnector::executeSql(active$conn, drop_sql,
                                       progressBar       = FALSE,
                                       reportOverallTime = FALSE),
        error = function(e) NULL  # best-effort cleanup
      )
    }

    names(df) <- tolower(names(df))
    for (col in c("cohort_start_date", "cohort_end_date")) {
      if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
    }

    n <- nrow(df)
    message(sprintf("Cohort '%s': %d patient%s found.",
                    basename(json_path), n, if (n == 1L) "" else "s"))
    tibble::as_tibble(df)
  })
}

# ---------------------------------------------------------------------------
# check_cohort_json
# ---------------------------------------------------------------------------

#' Diagnose a cohort JSON file against the CDM
#'
#' Runs three checks without modifying the database:
#' 1. Verifies the JSON parses correctly.
#' 2. Reports which concept IDs appear in the CDM vocabulary.
#' 3. Counts qualifying patients (index events only, ignoring inclusion rules).
#'
#' Call this before [fetch_cohort_from_json()] to determine whether an empty
#' cohort is a SQL/permission error or a genuinely empty population.
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param show_sql Logical. Print the generated cohort SQL. Default `FALSE`.
#'
#' @return Named list with `$concept_check` and `$candidate_count`.
#' @export
check_cohort_json <- function(connector, json_path, show_sql = FALSE) {
  if (!requireNamespace("CirceR",    quietly = TRUE)) {
    rlang::abort(
      "Package 'CirceR' required. Install: remotes::install_github('OHDSI/CirceR')"
    )
  }
  if (!requireNamespace("SqlRender", quietly = TRUE)) {
    rlang::abort("Package 'SqlRender' required.")
  }
  if (!file.exists(json_path)) {
    rlang::abort(paste0("JSON file not found: ", json_path))
  }

  message("--- Checking: ", basename(json_path), " ---")

  # 1. Parse JSON
  json_str   <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
  expression <- tryCatch(
    CirceR::cohortExpressionFromJson(json_str),
    error = function(e) rlang::abort(paste("JSON parse failed:", conditionMessage(e)))
  )
  concept_ids <- .extract_concept_ids(json_str)
  message(sprintf("JSON parsed OK. Found %d concept ID(s): %s",
                  length(concept_ids),
                  paste(concept_ids, collapse = ", ")))

  if (show_sql) {
    options    <- CirceR::createGenerateOptions(generateStats = FALSE)
    cohort_sql <- CirceR::buildCohortQuery(expression, options = options)
    message("--- Generated SQL (unredered) ---\n", cohort_sql, "\n---")
  }

  with_cohort_connector(connector, function(active) {
    dbms <- .get_dbms(active)

    # 2. Concept check: are the concept IDs present in the vocabulary?
    if (length(concept_ids) > 0L) {
      concept_sql <- SqlRender::translate(
        SqlRender::render(
          "SELECT concept_id, concept_name, domain_id, standard_concept
           FROM @vocab_schema.concept
           WHERE concept_id IN (@concept_ids);",
          vocab_schema = active$vocab_schema,
          concept_ids  = concept_ids
        ),
        targetDialect = dbms
      )
      concept_check <- tryCatch(
        {
          df <- if (inherits(active$conn, "JDBCConnection")) {
            as.data.frame(DBI::dbGetQuery(active$conn, concept_sql))
          } else {
            DatabaseConnector::querySql(active$conn, concept_sql,
                                         snakeCaseToCamelCase = FALSE)
          }
          names(df) <- tolower(names(df))
          tibble::as_tibble(df)
        },
        error = function(e) {
          message("Concept check failed: ", conditionMessage(e))
          tibble::tibble()
        }
      )
      message(sprintf("Vocabulary: %d / %d concept(s) found in CDM.",
                      nrow(concept_check), length(concept_ids)))
    } else {
      concept_check <- tibble::tibble()
      message("No concept IDs extracted from JSON.")
    }

    # 3. Candidate count: patients with at least one qualifying condition
    include_ids <- concept_ids[!grepl("^EXCLUDE", names(concept_ids))]
    candidate_count <- NA_integer_
    if (length(include_ids) > 0L) {
      count_sql <- SqlRender::translate(
        SqlRender::render(
          "SELECT COUNT(DISTINCT person_id) AS n
           FROM @cdm_schema.condition_occurrence
           WHERE condition_concept_id IN (@concept_ids);",
          cdm_schema  = active$cdm_schema,
          concept_ids = include_ids
        ),
        targetDialect = dbms
      )
      candidate_count <- tryCatch(
        {
          df <- if (inherits(active$conn, "JDBCConnection")) {
            as.data.frame(DBI::dbGetQuery(active$conn, count_sql))
          } else {
            DatabaseConnector::querySql(active$conn, count_sql,
                                         snakeCaseToCamelCase = FALSE)
          }
          names(df) <- tolower(names(df))
          as.integer(df$n[[1L]])
        },
        error = function(e) {
          message("Candidate count query failed: ", conditionMessage(e))
          NA_integer_
        }
      )
      message(sprintf(
        "Candidate patients with >=1 qualifying condition: %s",
        if (is.na(candidate_count)) "query failed" else as.character(candidate_count)
      ))
    }

    message("--- Done. If candidate_count > 0 but fetch returns 0, ")
    message("    the inclusion rules (age, prior observation, etc.) are filtering all patients.")

    invisible(list(concept_check    = concept_check,
                   candidate_count  = candidate_count))
  })
}

# ---------------------------------------------------------------------------
# list_cohort_templates
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.extract_concept_ids <- function(json_str) {
  tryCatch(
    {
      parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      concept_sets <- parsed$ConceptSets
      if (is.null(concept_sets)) return(integer(0))
      ids <- unlist(lapply(concept_sets, function(cs) {
        lapply(cs$expression$items, function(item) item$concept$CONCEPT_ID)
      }))
      unique(as.integer(ids[!sapply(ids, is.null)]))
    },
    error = function(e) integer(0)
  )
}
