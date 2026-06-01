# cohort.R
# Cohort instantiation from ATLAS JSON (CirceR format).
#
# CirceR with generateStats = FALSE generates two sections:
#   SETUP  -- creates #Codesets (temp table of expanded concept IDs)
#   COHORT -- DELETE FROM target + INSERT INTO target SELECT ...
#
# Strategy:
#   1. Execute the SETUP section (creates #Codesets; only needs temp-space access)
#   2. Extract the SELECT from the INSERT and run it as a plain query
#   → No writes to CDM schema, no create-table, no permissions beyond SELECT

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
#' Reads an ATLAS cohort definition JSON, generates SQL via `CirceR`, then:
#'
#' 1. Executes the **setup section** (creates the `#Codesets` temp table that
#'    expands concept IDs using `concept_ancestor`). This only requires access
#'    to temp space -- no write permission to the CDM schema is needed.
#' 2. Extracts the **SELECT** from the final `INSERT INTO ... SELECT` and runs
#'    it as a plain read-only query.
#'
#' No permanent table is created in the CDM schema.
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param cohort_id Integer. Cohort definition ID. Default `1L`.
#' @param verbose Logical. Print both SQL sections for debugging. Default
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
  opts       <- CirceR::createGenerateOptions(generateStats = FALSE)
  cohort_sql <- CirceR::buildCohortQuery(expression, options = opts)

  with_cohort_connector(connector, function(active) {
    dbms <- .get_dbms(active)

    # ------------------------------------------------------------------
    # Sentinel markers: unique strings that will not appear in real SQL
    # ------------------------------------------------------------------
    S_SCHEMA <- "CI_MARKER_SCHEMA_7x9"
    S_TABLE  <- "CI_MARKER_TABLE_7x9"

    # Render the full CirceR SQL with sentinel markers for the target table
    sql_full <- SqlRender::render(
      cohort_sql,
      cdm_database_schema        = active$cdm_schema,
      vocabulary_database_schema = active$vocab_schema,
      target_database_schema     = S_SCHEMA,
      target_cohort_table        = S_TABLE,
      target_cohort_id           = as.integer(cohort_id),
      results_database_schema    = active$cdm_schema
    )

    if (verbose) {
      message("\n=== Full rendered CirceR SQL ===\n", sql_full, "\n===\n")
    }

    # ------------------------------------------------------------------
    # Split: everything BEFORE the DELETE FROM sentinel is setup SQL.
    # The setup section creates #Codesets (concept expansion).
    # Only temp-space write access is needed there.
    # ------------------------------------------------------------------
    delete_pattern <- paste0(
      "DELETE\\s+FROM\\s+", S_SCHEMA, "\\.", S_TABLE
    )
    delete_pos <- regexpr(delete_pattern, sql_full, perl = TRUE,
                          ignore.case = TRUE)

    setup_sql <- if (delete_pos > 1L) {
      trimws(substring(sql_full, 1L, delete_pos - 1L))
    } else {
      ""
    }

    # ------------------------------------------------------------------
    # Extract the SELECT from INSERT INTO sentinel (...) SELECT ...
    # ------------------------------------------------------------------
    insert_pattern <- paste0(
      "INSERT\\s+INTO\\s+", S_SCHEMA, "\\.", S_TABLE,
      "\\s*\\([^)]+\\)\\s*"
    )
    m <- regexpr(insert_pattern, sql_full, ignore.case = TRUE, perl = TRUE)

    if (m == -1L) {
      rlang::abort(paste0(
        "Could not find INSERT statement in CirceR SQL for: ", basename(json_path),
        "\nRe-run with verbose = TRUE to inspect the generated SQL."
      ))
    }

    # Everything after the INSERT INTO ... (cols) header
    select_sql <- substring(sql_full, m + attr(m, "match.length"))
    # Drop from the first semicolon that terminates this SELECT statement.
    # Use a lookahead so we don't drop semicolons inside string literals.
    select_sql <- trimws(gsub(";.*$", "", select_sql))

    # ------------------------------------------------------------------
    # Translate both sections for the target DBMS
    # ------------------------------------------------------------------
    if (nzchar(setup_sql)) {
      setup_translated <- SqlRender::translate(setup_sql, targetDialect = dbms)
      if (verbose) {
        message("\n=== Setup SQL (translated for ", dbms, ") ===\n",
                setup_translated, "\n===\n")
      }
      tryCatch(
        DatabaseConnector::executeSql(
          active$conn, setup_translated,
          progressBar = FALSE, reportOverallTime = FALSE
        ),
        error = function(e) {
          rlang::abort(paste0(
            "Codesets setup failed (createing #Codesets temp table).\n",
            "DBMS: ", dbms, "\n",
            "Vocab schema: ", active$vocab_schema, "\n",
            "Hint: re-run with verbose = TRUE.\n",
            "Error: ", conditionMessage(e)
          ))
        }
      )
    }

    select_translated <- SqlRender::translate(select_sql, targetDialect = dbms)
    if (verbose) {
      message("\n=== Cohort SELECT SQL (translated for ", dbms, ") ===\n",
              select_translated, "\n===\n")
    }

    df <- tryCatch(
      {
        if (inherits(active$conn, "JDBCConnection")) {
          as.data.frame(DBI::dbGetQuery(active$conn, select_translated))
        } else {
          DatabaseConnector::querySql(active$conn, select_translated,
                                      snakeCaseToCamelCase = FALSE)
        }
      },
      error = function(e) {
        rlang::abort(paste0(
          "Cohort SELECT failed.\n",
          "DBMS: ", dbms, "\n",
          "Hint: re-run with verbose = TRUE to inspect the SELECT.\n",
          "Error: ", conditionMessage(e)
        ))
      }
    )

    names(df) <- tolower(names(df))
    for (col in c("cohort_start_date", "cohort_end_date")) {
      if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
    }

    # CirceR's SELECT aliases person_id as subject_id; handle both column names
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

#' Diagnose a cohort JSON against the CDM without modifying any tables
#'
#' Runs three read-only checks:
#' 1. JSON parses and SQL can be generated.
#' 2. Concept IDs exist in the CDM vocabulary.
#' 3. Count of patients with at least one qualifying condition code (pre-rules).
#'
#' If `candidate_count > 0` but [fetch_cohort_from_json()] returns 0, the
#' inclusion rules (age, prior observation, etc.) are filtering all patients.
#' Re-run `fetch_cohort_from_json(..., verbose = TRUE)` to inspect the SQL.
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param show_sql Logical. Print the raw CirceR SQL. Default `FALSE`.
#'
#' @return Invisibly, a named list with `$concept_check` and
#'   `$candidate_count`.
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
  if (!file.exists(json_path)) rlang::abort(paste0("JSON file not found: ", json_path))

  message("--- Checking: ", basename(json_path), " ---")

  json_str   <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
  expression <- tryCatch(
    CirceR::cohortExpressionFromJson(json_str),
    error = function(e) rlang::abort(paste("JSON parse failed:", conditionMessage(e)))
  )
  concept_ids <- .extract_concept_ids(json_str)
  message(sprintf("JSON OK. Concept IDs: %s",
                  if (length(concept_ids)) paste(concept_ids, collapse = ", ")
                  else "(none extracted)"))

  if (show_sql) {
    opts <- CirceR::createGenerateOptions(generateStats = FALSE)
    message("--- Raw CirceR SQL ---\n",
            CirceR::buildCohortQuery(expression, options = opts), "\n---")
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
    cond_ids <- concept_ids[concept_ids %in%
      (concept_check$concept_id[concept_check$domain_id == "Condition"])]
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
      if (is.na(candidate_count)) {
        message("Candidate count query failed -- check CDM schema and permissions.")
      } else if (candidate_count == 0L) {
        message("0 patients with matching concept IDs -- cohort will be empty.")
      } else {
        message(sprintf(
          "%d patients with >= 1 qualifying condition (before inclusion rules).",
          candidate_count
        ))
        message("If fetch_cohort_from_json() returns 0, run it with verbose = TRUE.")
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
