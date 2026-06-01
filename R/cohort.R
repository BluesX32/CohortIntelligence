# cohort.R
# Cohort instantiation from ATLAS JSON (CirceR format).
# Requires CirceR, SqlRender, and DatabaseConnector.
#
# Strategy: CirceR with generateStats = FALSE produces exactly one
# INSERT INTO <target> SELECT <cohort logic> per cohort definition.
# We extract just the SELECT part and execute it as a plain query,
# so the CDM never needs to be writable.

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
#' Reads an ATLAS cohort definition JSON, generates the cohort SQL via
#' `CirceR`, and runs it as a **read-only SELECT** against the CDM. No write
#' permissions, temp tables, or pre-existing cohort schema are required.
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param cohort_id Integer. Cohort definition ID (matched in the SELECT
#'   output). Default `1L`.
#' @param verbose Logical. Print the generated SQL for debugging. Default
#'   `FALSE`.
#'
#' @return tibble(subject_id, cohort_start_date, cohort_end_date)
#' @export
fetch_cohort_from_json <- function(connector,
                                    json_path,
                                    cohort_id = 1L,
                                    verbose   = FALSE) {
  for (pkg in c("CirceR", "SqlRender", "DatabaseConnector")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      rlang::abort(sprintf(
        "Package '%s' is required. Install: remotes::install_github('OHDSI/%s')",
        pkg, pkg
      ))
    }
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

    # ── Step 1: render with sentinel markers so we can extract the SELECT ──
    # Using unique strings that will never appear in real SQL.
    SENTINEL_SCHEMA <- "CI_MARKER_SCHEMA_7x9"
    SENTINEL_TABLE  <- "CI_MARKER_TABLE_7x9"

    sql_rendered <- SqlRender::render(
      cohort_sql,
      cdm_database_schema        = active$cdm_schema,
      vocabulary_database_schema = active$vocab_schema,
      target_database_schema     = SENTINEL_SCHEMA,
      target_cohort_table        = SENTINEL_TABLE,
      target_cohort_id           = as.integer(cohort_id),
      results_database_schema    = active$cdm_schema
    )

    if (verbose) {
      message("--- Rendered CirceR SQL (before SELECT extraction) ---")
      message(sql_rendered)
      message("--- End ---")
    }

    # ── Step 2: extract the SELECT from INSERT INTO sentinel (...) SELECT ──
    # CirceR with generateStats=FALSE produces one INSERT per definition.
    # Pattern: INSERT INTO SCHEMA.TABLE (col1, col2, col3, col4)\nSELECT ...
    insert_pattern <- paste0(
      "INSERT\\s+INTO\\s+",
      SENTINEL_SCHEMA, "\\.", SENTINEL_TABLE,
      "\\s*\\([^)]+\\)\\s*"
    )
    m <- regexpr(insert_pattern, sql_rendered,
                 ignore.case = TRUE, perl = TRUE)

    if (m == -1L) {
      rlang::abort(paste0(
        "Could not locate INSERT statement in CirceR-generated SQL.\n",
        "Re-run with verbose = TRUE to inspect the SQL."
      ))
    }

    select_sql <- substring(sql_rendered, m + attr(m, "match.length"))
    # Trim everything after the last semicolon that closes this statement
    select_sql <- trimws(gsub(";[\\s\\S]*$", "", select_sql, perl = TRUE))

    # ── Step 3: translate for target DBMS ────────────────────────────────────
    select_sql <- SqlRender::translate(select_sql, targetDialect = dbms)

    if (verbose) {
      message("--- Final SELECT SQL (translated to ", dbms, ") ---")
      message(select_sql)
      message("--- End ---")
    }

    # ── Step 4: execute as a plain SELECT ────────────────────────────────────
    df <- tryCatch(
      {
        if (inherits(active$conn, "JDBCConnection")) {
          as.data.frame(DBI::dbGetQuery(active$conn, select_sql))
        } else {
          DatabaseConnector::querySql(active$conn, select_sql,
                                      snakeCaseToCamelCase = FALSE)
        }
      },
      error = function(e) {
        rlang::abort(paste0(
          "Cohort SELECT failed.\n",
          "DBMS: ", dbms, "\n",
          "CDM schema: ", active$cdm_schema, "\n",
          "Vocab schema: ", active$vocab_schema, "\n",
          "Hint: run fetch_cohort_from_json(..., verbose = TRUE) to inspect SQL.\n",
          "Original error: ", conditionMessage(e)
        ))
      }
    )

    names(df) <- tolower(names(df))
    for (col in c("cohort_start_date", "cohort_end_date")) {
      if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
    }

    # subject_id is person_id in the CirceR SELECT output
    if (!"subject_id" %in% names(df) && "person_id" %in% names(df)) {
      df$subject_id <- df$person_id
    }

    n <- nrow(df)
    message(sprintf("Cohort '%s': %d patient%s found.",
                    basename(json_path), n, if (n == 1L) "" else "s"))
    tibble::as_tibble(df[, c("subject_id", "cohort_start_date", "cohort_end_date")])
  })
}

# ---------------------------------------------------------------------------
# check_cohort_json
# ---------------------------------------------------------------------------

#' Diagnose a cohort JSON file against the CDM
#'
#' Runs three checks without modifying the database:
#' 1. Verifies the JSON parses and the SQL can be extracted.
#' 2. Reports which concept IDs are present in the CDM vocabulary.
#' 3. Counts patients with at least one qualifying condition (pre-filter count).
#'
#' Call this before [fetch_cohort_from_json()] to determine whether an empty
#' cohort result is a SQL/permission error or a genuinely empty population.
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param show_sql Logical. Print the generated SELECT SQL. Default `FALSE`.
#'
#' @return Invisibly, a named list with `$concept_check` and `$candidate_count`.
#' @export
check_cohort_json <- function(connector, json_path, show_sql = FALSE) {
  for (pkg in c("CirceR", "SqlRender")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      rlang::abort(sprintf(
        "Package '%s' is required. Install: remotes::install_github('OHDSI/%s')",
        pkg, pkg
      ))
    }
  }
  if (!file.exists(json_path)) rlang::abort(paste0("JSON file not found: ", json_path))

  message("--- Checking: ", basename(json_path), " ---")

  json_str   <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
  expression <- tryCatch(
    CirceR::cohortExpressionFromJson(json_str),
    error = function(e) rlang::abort(paste("JSON parse failed:", conditionMessage(e)))
  )
  concept_ids <- .extract_concept_ids(json_str)
  message(sprintf("JSON OK. Concept IDs found: %s",
                  if (length(concept_ids)) paste(concept_ids, collapse = ", ")
                  else "(none extracted)"))

  if (show_sql) {
    options    <- CirceR::createGenerateOptions(generateStats = FALSE)
    cohort_sql <- CirceR::buildCohortQuery(expression, options = options)
    message("--- Raw CirceR SQL ---\n", cohort_sql, "\n---")
  }

  with_cohort_connector(connector, function(active) {
    dbms <- .get_dbms(active)

    # 1. Vocabulary check
    concept_check <- tibble::tibble()
    if (length(concept_ids) > 0L) {
      vocab_sql <- SqlRender::translate(
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
            as.data.frame(DBI::dbGetQuery(active$conn, vocab_sql))
          } else {
            DatabaseConnector::querySql(active$conn, vocab_sql,
                                         snakeCaseToCamelCase = FALSE)
          }
          names(df) <- tolower(names(df))
          tibble::as_tibble(df)
        },
        error = function(e) {
          message("Vocabulary check failed: ", conditionMessage(e))
          tibble::tibble()
        }
      )
      message(sprintf("Vocabulary: %d / %d concept(s) found in CDM.",
                      nrow(concept_check), length(concept_ids)))
      if (nrow(concept_check) > 0L) print(concept_check)
    }

    # 2. Candidate count (patients with >=1 qualifying condition, pre-rules)
    candidate_count <- NA_integer_
    if (length(concept_ids) > 0L) {
      count_sql <- SqlRender::translate(
        SqlRender::render(
          "SELECT COUNT(DISTINCT person_id) AS n
           FROM @cdm_schema.condition_occurrence
           WHERE condition_concept_id IN (@concept_ids);",
          cdm_schema  = active$cdm_schema,
          concept_ids = concept_ids
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
          message("Candidate count failed: ", conditionMessage(e))
          NA_integer_
        }
      )
      msg <- if (is.na(candidate_count)) {
        "Candidate count query failed."
      } else if (candidate_count == 0L) {
        "0 patients with matching concept IDs -- cohort will be empty."
      } else {
        sprintf(
          "%d patients with >=1 qualifying condition (before age/obs/inclusion rules).",
          candidate_count
        )
      }
      message(msg)
    }

    if (!is.na(candidate_count) && candidate_count > 0L) {
      message("If fetch_cohort_from_json() still returns 0, inclusion rules")
      message("(age >= 18, prior observation window, etc.) are filtering all patients.")
      message("Re-run fetch_cohort_from_json(..., verbose = TRUE) to inspect the SELECT.")
    }

    invisible(list(concept_check   = concept_check,
                   candidate_count = candidate_count))
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
      if (!requireNamespace("jsonlite", quietly = TRUE)) return(integer(0))
      parsed      <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
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
