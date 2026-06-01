# cohort.R
# Cohort instantiation from ATLAS JSON (CirceR format).
#
# CirceR with generateStats = FALSE generates SQL in two sections:
#
#   SETUP  -- CREATE TABLE #Codesets; INSERT INTO #Codesets SELECT (concept expansion)
#   COHORT -- DELETE FROM <target>; INSERT INTO <target> SELECT (the cohort logic)
#
# Strategy (read-only, no CDM write permissions required):
#   1. Execute the SETUP section -- only touches session temp space (#Codesets)
#   2. Extract the SELECT from the final INSERT and run it as a plain query
#
# All DB calls go through DatabaseConnector's standard API (no JDBC/ODBC branches).

# ---------------------------------------------------------------------------
# Internal: safe DBMS detection
# ---------------------------------------------------------------------------

.get_dbms <- function(active) {
  dbms <- active$dbms
  if (length(dbms) == 1L && nzchar(dbms %||% "")) return(dbms)
  tryCatch(
    {
      d <- DatabaseConnector::dbms(active$conn)
      if (length(d) == 1L && nzchar(d)) d else "spark"
    },
    error = function(e) "spark"
  )
}

# ---------------------------------------------------------------------------
# fetch_cohort_from_json
# ---------------------------------------------------------------------------

#' Instantiate a cohort from an ATLAS JSON file and return cohort members
#'
#' Reads an ATLAS cohort definition JSON, generates SQL via `CirceR`, then:
#'
#' 1. Executes the **setup section** (creates `#Codesets` with expanded concept
#'    IDs). Writes only to session temp space -- no CDM write permissions.
#' 2. Extracts the **SELECT** from the `INSERT INTO ... SELECT` and executes it
#'    as a plain read-only query.
#'
#' All database calls use `DatabaseConnector` regardless of connection type
#' (ODBC, JDBC, or DBI).
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param cohort_id Integer. Default `1L`.
#' @param verbose Logical. Print every SQL section to the console. Default
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
        "Package '%s' required. Install: remotes::install_github('OHDSI/%s')",
        pkg, pkg
      ))
    }
  }

  json_path <- normalizePath(json_path, mustWork = FALSE)
  if (!file.exists(json_path)) {
    rlang::abort(paste0("JSON file not found: ", json_path))
  }

  # -- Step 1: parse JSON and generate SQL -----------------------------------
  message("[CI] Parsing: ", basename(json_path))
  json_str   <- paste(readLines(json_path, warn = FALSE), collapse = "\n")

  expression <- tryCatch(
    CirceR::cohortExpressionFromJson(json_str),
    error = function(e) rlang::abort(
      paste0("CirceR failed to parse JSON: ", conditionMessage(e))
    )
  )

  opts       <- CirceR::createGenerateOptions(generateStats = FALSE)
  cohort_sql <- tryCatch(
    CirceR::buildCohortQuery(expression, options = opts),
    error = function(e) rlang::abort(
      paste0("CirceR failed to build SQL: ", conditionMessage(e))
    )
  )

  if (length(cohort_sql) == 0L || !nzchar(cohort_sql %||% "")) {
    rlang::abort(
      "CirceR::buildCohortQuery() returned empty SQL. JSON may be malformed."
    )
  }
  message("[CI] CirceR SQL generated (", nchar(cohort_sql), " chars).")

  with_cohort_connector(connector, function(active) {
    dbms <- .get_dbms(active)
    message("[CI] DBMS: ", dbms)

    # Unique sentinel markers -- will never appear in real SQL
    S_SCHEMA <- "CI_MARKER_SCHEMA_7x9"
    S_TABLE  <- "CI_MARKER_TABLE_7x9"

    # -- Step 2: render parameter substitution --------------------------------
    sql_full <- tryCatch(
      SqlRender::render(
        cohort_sql,
        cdm_database_schema        = active$cdm_schema,
        vocabulary_database_schema = active$vocab_schema,
        target_database_schema     = S_SCHEMA,
        target_cohort_table        = S_TABLE,
        target_cohort_id           = as.integer(cohort_id),
        results_database_schema    = active$cdm_schema
      ),
      error = function(e) rlang::abort(
        paste0("SqlRender::render() failed: ", conditionMessage(e))
      )
    )

    if (length(sql_full) == 0L || !nzchar(sql_full %||% "")) {
      rlang::abort(paste0(
        "SqlRender::render() returned empty SQL.\n",
        "CDM schema: ", active$cdm_schema, "\n",
        "Vocab schema: ", active$vocab_schema
      ))
    }
    message("[CI] SQL rendered (", nchar(sql_full), " chars).")

    if (verbose) {
      message("\n=== Full rendered SQL ===\n", sql_full, "\n===\n")
    }

    # -- Step 3: split at DELETE FROM sentinel --------------------------------
    # Everything BEFORE the DELETE is the #Codesets setup section.
    # Everything AFTER (and including) the DELETE is the cohort section.
    delete_pattern <- paste0(
      "DELETE\\s+FROM\\s+", S_SCHEMA, "\\.", S_TABLE
    )
    delete_pos <- regexpr(delete_pattern, sql_full,
                          perl = TRUE, ignore.case = TRUE)

    if (length(delete_pos) == 0L) {
      rlang::abort("Regex on SQL returned integer(0). SQL may be character(0).")
    }

    setup_sql <- if (delete_pos > 1L) {
      trimws(substring(sql_full, 1L, delete_pos - 1L))
    } else {
      ""
    }
    message("[CI] Setup section: ", nchar(setup_sql), " chars.")

    # -- Step 4: extract SELECT from INSERT INTO sentinel ... SELECT ----------
    insert_pattern <- paste0(
      "INSERT\\s+INTO\\s+", S_SCHEMA, "\\.", S_TABLE,
      "\\s*\\([^)]+\\)\\s*"
    )
    m <- regexpr(insert_pattern, sql_full, ignore.case = TRUE, perl = TRUE)

    if (length(m) == 0L || m == -1L) {
      rlang::abort(paste0(
        "Could not find INSERT INTO ", S_SCHEMA, ".", S_TABLE,
        " in rendered SQL.\n",
        "Re-run with verbose = TRUE to inspect the SQL."
      ))
    }

    # Extract everything after the INSERT INTO header.
    # Use perl = TRUE with [\\s\\S]* to match across newlines so that the
    # trailing semicolons AND any extra DELETE/INSERT statements after the
    # main SELECT are all removed.
    after_insert <- substring(sql_full, m + attr(m, "match.length"))
    select_sql   <- trimws(gsub(";[\\s\\S]*$", "", after_insert, perl = TRUE))

    if (!nzchar(select_sql)) {
      rlang::abort("Extracted SELECT is empty after removing trailing SQL.")
    }
    message("[CI] Extracted SELECT (", nchar(select_sql), " chars).")

    # -- Step 5: translate for target DBMS ------------------------------------
    if (nzchar(setup_sql)) {
      setup_translated <- SqlRender::translate(setup_sql, targetDialect = dbms)
      if (verbose) {
        message("\n=== Setup SQL (", dbms, ") ===\n", setup_translated, "\n===\n")
      }
      message("[CI] Executing setup SQL (#Codesets)...")
      tryCatch(
        DatabaseConnector::executeSql(
          active$conn, setup_translated,
          progressBar = FALSE, reportOverallTime = FALSE
        ),
        error = function(e) rlang::abort(paste0(
          "Setup SQL failed (#Codesets creation).\n",
          "DBMS: ", dbms, "\n",
          "Vocab schema: ", active$vocab_schema, "\n",
          "Error: ", conditionMessage(e)
        ))
      )
      message("[CI] Setup SQL done.")
    }

    select_translated <- SqlRender::translate(select_sql, targetDialect = dbms)
    if (verbose) {
      message("\n=== Cohort SELECT (", dbms, ") ===\n", select_translated, "\n===\n")
    }

    # -- Step 6: run cohort SELECT --------------------------------------------
    message("[CI] Running cohort SELECT...")
    df <- tryCatch(
      DatabaseConnector::querySql(
        active$conn, select_translated,
        snakeCaseToCamelCase = FALSE
      ),
      error = function(e) rlang::abort(paste0(
        "Cohort SELECT failed.\n",
        "DBMS: ", dbms, "\n",
        "Hint: re-run with verbose = TRUE to inspect the SELECT SQL.\n",
        "Error: ", conditionMessage(e)
      ))
    )

    names(df) <- tolower(names(df))
    for (col in c("cohort_start_date", "cohort_end_date")) {
      if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
    }
    # CirceR SELECT uses person_id; normalise to subject_id
    if (!"subject_id" %in% names(df) && "person_id" %in% names(df)) {
      df$subject_id <- df$person_id
    }

    n <- nrow(df)
    message(sprintf("[CI] Cohort '%s': %d patient%s found.",
                    basename(json_path), n, if (n == 1L) "" else "s"))
    tibble::as_tibble(df[, c("subject_id", "cohort_start_date",
                              "cohort_end_date")])
  })
}

# ---------------------------------------------------------------------------
# check_cohort_json
# ---------------------------------------------------------------------------

#' Diagnose a cohort JSON against the CDM (read-only)
#'
#' Checks that concept IDs exist in the vocabulary and counts patients with at
#' least one qualifying condition code (before any inclusion rules are applied).
#'
#' If `candidate_count > 0` but [fetch_cohort_from_json()] returns 0, re-run
#' `fetch_cohort_from_json(..., verbose = TRUE)` to inspect the SQL.
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param show_sql Logical. Print the raw CirceR SQL. Default `FALSE`.
#'
#' @return Invisibly, a named list with `$concept_check` and `$candidate_count`.
#' @export
check_cohort_json <- function(connector, json_path, show_sql = FALSE) {
  for (pkg in c("CirceR", "SqlRender")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      rlang::abort(sprintf(
        "Package '%s' required. Install: remotes::install_github('OHDSI/%s')",
        pkg, pkg
      ))
    }
  }
  json_path <- normalizePath(json_path, mustWork = FALSE)
  if (!file.exists(json_path)) rlang::abort(paste0("JSON not found: ", json_path))

  message("--- Checking: ", basename(json_path), " ---")
  json_str   <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
  expression <- tryCatch(
    CirceR::cohortExpressionFromJson(json_str),
    error = function(e) rlang::abort(paste("JSON parse failed:", conditionMessage(e)))
  )
  concept_ids <- .extract_concept_ids(json_str)
  message(sprintf("JSON OK. Concept IDs: %s",
                  if (length(concept_ids)) paste(concept_ids, collapse = ", ")
                  else "(none)"))

  if (show_sql) {
    opts <- CirceR::createGenerateOptions(generateStats = FALSE)
    message("--- Raw CirceR SQL ---\n",
            CirceR::buildCohortQuery(expression, options = opts), "\n---")
  }

  with_cohort_connector(connector, function(active) {
    dbms <- .get_dbms(active)

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
          df <- DatabaseConnector::querySql(active$conn, vocab_sql,
                                             snakeCaseToCamelCase = FALSE)
          names(df) <- tolower(names(df))
          tibble::as_tibble(df)
        },
        error = function(e) {
          message("Vocabulary check failed: ", conditionMessage(e))
          tibble::tibble()
        }
      )
      message(sprintf("Vocabulary: %d / %d concept(s) found.",
                      nrow(concept_check), length(concept_ids)))
      if (nrow(concept_check) > 0L) print(concept_check)
    }

    candidate_count <- NA_integer_
    cond_ids <- if (nrow(concept_check) > 0L) {
      concept_check$concept_id[
        tolower(concept_check$domain_id) == "condition"
      ]
    } else {
      concept_ids
    }

    if (length(cond_ids) > 0L) {
      count_sql <- SqlRender::translate(
        SqlRender::render(
          "SELECT COUNT(DISTINCT person_id) AS n
           FROM @cdm_schema.condition_occurrence
           WHERE condition_concept_id IN (@concept_ids);",
          cdm_schema  = active$cdm_schema,
          concept_ids = cond_ids
        ),
        targetDialect = dbms
      )
      candidate_count <- tryCatch(
        {
          df <- DatabaseConnector::querySql(active$conn, count_sql,
                                             snakeCaseToCamelCase = FALSE)
          names(df) <- tolower(names(df))
          as.integer(df$n[[1L]])
        },
        error = function(e) {
          message("Candidate count failed: ", conditionMessage(e))
          NA_integer_
        }
      )
      if (is.na(candidate_count)) {
        message("Count query failed.")
      } else if (candidate_count == 0L) {
        message("0 patients match -- cohort will be empty.")
      } else {
        message(sprintf("%d patients with >=1 qualifying condition (pre-rules).",
                        candidate_count))
        message("If fetch_cohort_from_json() returns 0, run with verbose = TRUE.")
      }
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
      parsed       <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
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
