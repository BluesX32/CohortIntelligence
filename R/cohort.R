# cohort.R
# Cohort instantiation from ATLAS JSON (CirceR format).
#
# CirceR generates SQL that creates 7 intermediate tables:
#   Codesets -> qualified_events -> Inclusion_0 -> inclusion_events
#   -> included_events -> strategy_ends -> cohort_rows -> final_cohort
#
# Strategy (fully read-only, zero write permissions required):
#   1. Render parameters (SqlRender::render)
#   2. Split at DELETE FROM sentinel -> setup section + final SELECT
#   3. Translate setup to Spark dialect (CREATE TABLE ... USING DELTA ...)
#   4. Convert every CREATE TABLE / INSERT INTO in the setup into a CTE
#   5. Combine: WITH cte1 AS (...), ..., cteN AS (...) <final SELECT>
#   6. Execute as a single plain querySql() call
#
# No CREATE TABLE, no INSERT, no temp space, no write permissions anywhere.

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
# Internal: convert translated Spark setup SQL to a WITH ... CTE preamble
# ---------------------------------------------------------------------------

# Takes the Spark-translated setup section (the part before DELETE FROM sentinel)
# and converts every CREATE TABLE / INSERT INTO statement into a named CTE.
# Returns a character string: "WITH cte1 AS (...), cte2 AS (...), ..."
#
# Handles:
#   DROP TABLE IF EXISTS xxx          -> skip
#   DROP TABLE xxx                    -> skip
#   TRUNCATE TABLE xxx                -> skip
#   CREATE TABLE xxx USING DELTA AS SELECT ... WHERE 1=0   -> schema-only init, skip
#   CREATE TABLE xxx USING DELTA AS SELECT <real body>     -> CTE body
#   INSERT INTO xxx (cols) SELECT <body>                   -> UNION ALL to existing CTE

.spark_setup_to_cte <- function(setup_sql) {
  # Split on semicolons (safe: CirceR SQL has no semicolons inside statements)
  stmts <- trimws(strsplit(setup_sql, ";", fixed = TRUE)[[1]])
  stmts <- stmts[nzchar(stmts)]

  cte_bodies <- list()   # table_name -> character vector of SELECT bodies
  cte_order  <- character(0)

  for (stmt in stmts) {
    # ---- patterns to skip ----
    if (grepl("^(DROP|TRUNCATE)[[:space:]]+TABLE", stmt,
              ignore.case = TRUE, perl = TRUE)) next

    # Schema-only init: CREATE TABLE xxx ... AS SELECT ... WHERE 1=0
    if (grepl("WHERE[[:space:]]+1[[:space:]]*=[[:space:]]*0",
              stmt, ignore.case = TRUE, perl = TRUE)) next

    # ---- CREATE TABLE xxx USING DELTA AS <body> ----
    cta_m <- regexpr(
      paste0("CREATE[[:space:]]+TABLE[[:space:]]+([[:alnum:]_]+)",
             "[[:space:]]+USING[[:space:]]+DELTA[[:space:]]+AS[[:space:]]*"),
      stmt, ignore.case = TRUE, perl = TRUE
    )
    if (cta_m != -1L) {
      # Extract table name from the first line only (avoids multiline sub() leak)
      first_line <- strsplit(stmt, "\n", fixed = TRUE)[[1]][1]
      tname <- sub(".*TABLE[[:space:]]+([[:alnum:]_]+).*", "\\1",
                   first_line, ignore.case = TRUE, perl = TRUE)
      body <- trimws(substring(stmt, cta_m + attr(cta_m, "match.length")))
      if (!tname %in% cte_order) cte_order <- c(cte_order, tname)
      cte_bodies[[tname]] <- c(cte_bodies[[tname]], body)
      next
    }

    # ---- INSERT INTO xxx (cols) SELECT <body> ----
    ins_m <- regexpr(
      "INSERT[[:space:]]+INTO[[:space:]]+([[:alnum:]_]+)[[:space:]]*\\([^)]+\\)[[:space:]]*",
      stmt, ignore.case = TRUE, perl = TRUE
    )
    if (ins_m != -1L) {
      first_line <- strsplit(stmt, "\n", fixed = TRUE)[[1]][1]
      tname <- sub(".*INTO[[:space:]]+([[:alnum:]_]+)[[:space:]]*.*", "\\1",
                   first_line, ignore.case = TRUE, perl = TRUE)
      body <- trimws(substring(stmt, ins_m + attr(ins_m, "match.length")))
      if (!tname %in% cte_order) cte_order <- c(cte_order, tname)
      cte_bodies[[tname]] <- c(cte_bodies[[tname]], body)
      next
    }
  }

  # Keep only CTEs that have at least one SELECT body
  cte_order <- unique(cte_order)
  cte_order <- cte_order[
    sapply(cte_order, function(t) !is.null(cte_bodies[[t]]) &&
             length(cte_bodies[[t]]) > 0L)
  ]

  if (length(cte_order) == 0L) {
    rlang::abort(
      "setup_to_cte: no CTEs could be extracted. Run with verbose=TRUE."
    )
  }

  cte_strs <- sapply(cte_order, function(t) {
    body <- paste(cte_bodies[[t]], collapse = "\nUNION ALL\n")
    paste0(t, " AS (\n", body, "\n)")
  })

  paste("WITH", paste(cte_strs, collapse = ",\n"))
}

# ---------------------------------------------------------------------------
# fetch_cohort_from_json
# ---------------------------------------------------------------------------

#' Instantiate a cohort from an ATLAS JSON file (fully read-only)
#'
#' Reads an ATLAS cohort definition JSON, generates SQL via `CirceR`, converts
#' the entire multi-table pipeline into a single `WITH ... SELECT` CTE query,
#' and executes it as a plain read-only query via `DatabaseConnector`.
#'
#' **No write permissions required anywhere** -- no temp tables, no Delta
#' tables, no intermediate storage.
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param cohort_id Integer. Default `1L`.
#' @param verbose Logical. Print generated SQL sections for debugging. Default
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

  # -- Step 1: build CirceR SQL ----------------------------------------------
  message("[CI] Parsing: ", basename(json_path))
  json_str   <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
  expression <- tryCatch(
    CirceR::cohortExpressionFromJson(json_str),
    error = function(e) rlang::abort(
      paste0("CirceR JSON parse failed: ", conditionMessage(e))
    )
  )
  opts       <- CirceR::createGenerateOptions(generateStats = FALSE)
  cohort_sql <- tryCatch(
    CirceR::buildCohortQuery(expression, options = opts),
    error = function(e) rlang::abort(
      paste0("CirceR SQL generation failed: ", conditionMessage(e))
    )
  )
  if (length(cohort_sql) == 0L || !nzchar(cohort_sql %||% "")) {
    rlang::abort("CirceR returned empty SQL. JSON may be malformed.")
  }
  message("[CI] CirceR SQL generated (", nchar(cohort_sql), " chars).")

  with_cohort_connector(connector, function(active) {
    dbms <- .get_dbms(active)
    message("[CI] DBMS: ", dbms)

    # Sentinel markers (never appear in real SQL)
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
        "cdm_schema: ", active$cdm_schema, " | vocab_schema: ", active$vocab_schema
      ))
    }
    message("[CI] SQL rendered (", nchar(sql_full), " chars).")

    if (verbose) {
      message("\n=== Full rendered SQL ===\n", sql_full, "\n===\n")
    }

    # -- Step 3: split at DELETE FROM sentinel --------------------------------
    del_pat  <- paste0("DELETE[[:space:]]+FROM[[:space:]]+",
                       S_SCHEMA, "\\.", S_TABLE)
    del_pos  <- regexpr(del_pat, sql_full, ignore.case = TRUE, perl = TRUE)
    if (length(del_pos) == 0L) {
      rlang::abort("Could not locate DELETE FROM sentinel. Run with verbose=TRUE.")
    }
    setup_sql <- if (del_pos > 1L) {
      trimws(substring(sql_full, 1L, del_pos - 1L))
    } else {
      ""
    }

    # -- Step 4: extract final SELECT (after INSERT INTO sentinel ...) --------
    ins_pat <- paste0(
      "INSERT[[:space:]]+INTO[[:space:]]+", S_SCHEMA, "\\.", S_TABLE,
      "[[:space:]]*\\([^)]+\\)[[:space:]]*"
    )
    ins_m <- regexpr(ins_pat, sql_full, ignore.case = TRUE, perl = TRUE)
    if (length(ins_m) == 0L || ins_m == -1L) {
      rlang::abort(paste0(
        "Could not locate INSERT INTO sentinel. Run with verbose=TRUE."
      ))
    }
    after_insert <- substring(sql_full, ins_m + attr(ins_m, "match.length"))
    # Remove from first semicolon to end (removes trailing DELETE/INSERT)
    final_select <- trimws(gsub(";[\\s\\S]*$", "", after_insert, perl = TRUE))
    if (!nzchar(final_select)) {
      rlang::abort("Extracted SELECT is empty. Run with verbose=TRUE.")
    }
    message("[CI] Final SELECT extracted (", nchar(final_select), " chars).")

    # -- Step 5: translate both sections to target DBMS ----------------------
    setup_translated  <- SqlRender::translate(setup_sql,    targetDialect = dbms)
    select_translated <- SqlRender::translate(final_select, targetDialect = dbms)

    if (verbose) {
      message("\n=== Translated setup SQL ===\n", setup_translated, "\n===\n")
      message("\n=== Translated SELECT ===\n",   select_translated, "\n===\n")
    }

    # -- Step 6: convert setup to CTE preamble (no writes needed) -----------
    cte_preamble <- tryCatch(
      .spark_setup_to_cte(setup_translated),
      error = function(e) rlang::abort(
        paste0("CTE conversion failed: ", conditionMessage(e), "\n",
               "Run with verbose=TRUE to inspect translated setup SQL.")
      )
    )
    message("[CI] CTE preamble built.")

    # -- Step 7: build and execute the single read-only CTE query -----------
    full_cte_query <- paste0(cte_preamble, "\n", select_translated)

    if (verbose) {
      message("\n=== Final CTE query ===\n", full_cte_query, "\n===\n")
    }

    message("[CI] Executing read-only CTE query...")
    df <- tryCatch(
      DatabaseConnector::querySql(
        active$conn, full_cte_query,
        snakeCaseToCamelCase = FALSE
      ),
      error = function(e) rlang::abort(paste0(
        "CTE query failed.\n",
        "DBMS: ", dbms, "\n",
        "Hint: re-run with verbose=TRUE to inspect the full CTE SQL.\n",
        "Error: ", conditionMessage(e)
      ))
    )

    names(df) <- tolower(names(df))

    # Normalise column names: CirceR SELECT returns start_date / end_date;
    # the rest of the pipeline expects cohort_start_date / cohort_end_date.
    if (!"cohort_start_date" %in% names(df) && "start_date" %in% names(df)) {
      df$cohort_start_date <- df$start_date
    }
    if (!"cohort_end_date" %in% names(df) && "end_date" %in% names(df)) {
      df$cohort_end_date <- df$end_date
    }
    for (col in c("cohort_start_date", "cohort_end_date")) {
      if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
    }
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

#' Diagnose a cohort JSON against the CDM (fully read-only)
#'
#' Checks concept coverage in the vocabulary and counts patients with at least
#' one qualifying condition code (before inclusion rules). No data is modified.
#'
#' @param connector A `cohort_omop_connector` from [create_cohort_connector()].
#' @param json_path Path to an ATLAS cohort definition JSON file.
#' @param show_sql Logical. Print raw CirceR SQL. Default `FALSE`.
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
      concept_check$concept_id[tolower(concept_check$domain_id) == "condition"]
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
        message("0 patients with matching condition codes -- cohort will be empty.")
      } else {
        message(sprintf("%d patients with >=1 qualifying condition (pre-rules).",
                        candidate_count))
        message("If fetch_cohort_from_json() returns 0, run with verbose=TRUE.")
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
