# connect.R
# S3 connector abstraction for CohortIntelligence.
#
# Two connector types:
#   cohort_omop_connector : live OMOP CDM database via DatabaseConnector
#   cohort_df_connector   : in-memory named list of domain tibbles (tests/demos)
#
# Both satisfy the same interface via S3 generics.

# ---------------------------------------------------------------------------
# Dependency guard
# ---------------------------------------------------------------------------

.check_cohort_db_packages <- function() {
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'DatabaseConnector' is required for OMOP CDM connectivity.\n",
      "Install with: install.packages('DatabaseConnector')"
    ))
  }
  if (!requireNamespace("SqlRender", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'SqlRender' is required for cross-DBMS SQL translation.\n",
      "Install with: install.packages('SqlRender')"
    ))
  }
}

# ---------------------------------------------------------------------------
# Constructors
# ---------------------------------------------------------------------------

#' Wrap a live database connection for use with CohortIntelligence
#'
#' The recommended approach. Call `DatabaseConnector::connect()` first to
#' open the connection with the correct permissions, then pass it here.
#' The connection is reused for all queries and never closed by the package --
#' the caller controls the lifecycle.
#'
#' @param connection A live connection from `DatabaseConnector::connect()`.
#' @param cdm_schema `character(1)`. Schema containing OMOP CDM tables.
#' @param vocab_schema `character(1)` or `NULL`. Vocabulary schema.
#'   Defaults to `cdm_schema`.
#' @param cdm_version `character(1)`. Default `"5.4"`.
#'
#' @return An object of class `c("cohort_omop_connector", "cohort_connector")`.
#' @export
create_cohort_connector <- function(connection,
                                     cdm_schema,
                                     vocab_schema = NULL,
                                     cdm_version  = "5.4") {
  if (is.null(connection)) {
    rlang::abort("'connection' must be a live DatabaseConnector connection.")
  }
  if (missing(cdm_schema) || !nzchar(cdm_schema)) {
    rlang::abort("'cdm_schema' must be a non-empty character string.")
  }
  dbms_str <- tryCatch(DatabaseConnector::dbms(connection),
                        error = function(e) {
                          if (inherits(connection, "JDBCConnection")) "spark"
                          else "sql server"
                        })
  structure(
    list(
      type              = "omop",
      connectionDetails = NULL,
      cdm_schema        = cdm_schema,
      cohort_schema     = cdm_schema,
      vocab_schema      = vocab_schema %||% cdm_schema,
      cdm_version       = cdm_version,
      conn              = connection,
      dbms              = dbms_str
    ),
    class = c("cohort_omop_connector", "cohort_connector")
  )
}

#' Create an OMOP CDM connector from connectionDetails (lazy connection)
#'
#' Opens a new connection per query and closes it immediately after.
#' Prefer [create_cohort_connector()] with a pre-opened connection when
#' working with permission-sensitive environments (Databricks, SAFER, etc.).
#'
#' @param connectionDetails A `connectionDetails` object from
#'   `DatabaseConnector::createConnectionDetails()`.
#' @param cdm_schema `character(1)`. Schema containing OMOP CDM tables.
#' @param cohort_schema `character(1)` or `NULL`. Defaults to `cdm_schema`.
#' @param vocab_schema `character(1)` or `NULL`. Defaults to `cdm_schema`.
#' @param cdm_version `character(1)`. Default `"5.4"`.
#'
#' @return An object of class `c("cohort_omop_connector", "cohort_connector")`.
#' @export
create_cohort_omop_connector <- function(connectionDetails,
                                          cdm_schema,
                                          cohort_schema = NULL,
                                          vocab_schema  = NULL,
                                          cdm_version   = "5.4") {
  if (missing(connectionDetails) || is.null(connectionDetails)) {
    rlang::abort("'connectionDetails' must be a DatabaseConnector connectionDetails object.")
  }
  if (missing(cdm_schema) || !nzchar(cdm_schema)) {
    rlang::abort("'cdm_schema' must be a non-empty character string.")
  }
  structure(
    list(
      type              = "omop",
      connectionDetails = connectionDetails,
      cdm_schema        = cdm_schema,
      cohort_schema     = cohort_schema %||% cdm_schema,
      vocab_schema      = vocab_schema  %||% cdm_schema,
      cdm_version       = cdm_version,
      conn              = NULL,
      dbms              = NULL
    ),
    class = c("cohort_omop_connector", "cohort_connector")
  )
}

#' Create an in-memory connector for tests and demos
#'
#' Wraps a named list of pre-built domain tibbles in the standard connector
#' interface. No database or extra packages required.
#'
#' @param cohort_data A named list with slots: `$cohort`, `$person`,
#'   `$condition`, `$drug`, `$procedure`, `$measurement`, `$observation`,
#'   `$visit`, `$death`. Missing slots are filled with zero-row typed tibbles.
#'
#' @return An object of class `c("cohort_df_connector", "cohort_connector")`.
#' @export
create_cohort_df_connector <- function(cohort_data) {
  if (!is.list(cohort_data)) {
    rlang::abort("'cohort_data' must be a named list of domain data frames.")
  }

  valid_slots <- c("cohort", "person", "condition", "drug", "procedure",
                   "measurement", "observation", "visit", "death")

  for (slot in intersect(names(cohort_data), valid_slots)) {
    if (!is.data.frame(cohort_data[[slot]])) {
      rlang::abort(paste0("'cohort_data$", slot, "' must be a data frame."))
    }
    cohort_data[[slot]] <- tibble::as_tibble(cohort_data[[slot]])
  }

  for (slot in valid_slots) {
    if (!slot %in% names(cohort_data)) {
      cohort_data[[slot]] <- .empty_cohort_domain(slot)
    }
  }

  structure(
    list(type = "df", cohort_data = cohort_data),
    class = c("cohort_df_connector", "cohort_connector")
  )
}

# ---------------------------------------------------------------------------
# Print methods
# ---------------------------------------------------------------------------

#' @export
print.cohort_omop_connector <- function(x, ...) {
  cat("<cohort_omop_connector>\n")
  cat("  CDM schema    :", x$cdm_schema, "\n")
  cat("  Cohort schema :", x$cohort_schema, "\n")
  cat("  CDM version   :", x$cdm_version, "\n")
  cat("  Connected     :", if (!is.null(x$conn)) "yes" else "no (lazy)", "\n")
  invisible(x)
}

#' @export
print.cohort_df_connector <- function(x, ...) {
  cat("<cohort_df_connector>\n")
  cd <- x$cohort_data
  for (slot in c("cohort", "person", "condition", "drug", "procedure",
                 "measurement", "observation", "visit", "death")) {
    cat(sprintf("  %-12s: %d rows\n", slot, nrow(cd[[slot]])))
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# Connection lifecycle generics
# ---------------------------------------------------------------------------

#' Execute a function within a managed database connection
#'
#' For `cohort_omop_connector`: opens a connection, runs `fn(connector)`, and
#' closes the connection even on error. Stale connections are detected and
#' reconnected automatically.
#'
#' For `cohort_df_connector`: runs `fn(connector)` directly.
#'
#' @param connector A `cohort_connector` object.
#' @param fn A function accepting the connector as its first argument.
#' @param ... Additional arguments forwarded to `fn`.
#' @return The return value of `fn(connector, ...)`.
#' @export
with_cohort_connector <- function(connector, fn, ...) {
  UseMethod("with_cohort_connector")
}

#' @export
with_cohort_connector.cohort_omop_connector <- function(connector, fn, ...) {
  .check_cohort_db_packages()

  if (!is.null(connector$conn)) {
    result <- tryCatch(
      {
        active      <- connector
        active$dbms <- active$dbms %||%
          tryCatch(DatabaseConnector::dbms(active$conn),
                   error = function(e) "sql server")
        fn(active, ...)
      },
      error = function(e) {
        if (.is_closed_connection_error(e))
          structure(list(), class = "ci_reconnect_needed")
        else
          stop(e)
      }
    )
    if (!inherits(result, "ci_reconnect_needed")) return(result)
    tryCatch(DatabaseConnector::disconnect(connector$conn), error = function(e) NULL)
  }

  conn <- DatabaseConnector::connect(connector$connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  active      <- connector
  active$conn <- conn
  active$dbms <- tryCatch(DatabaseConnector::dbms(conn),
                           error = function(e) "sql server")
  fn(active, ...)
}

#' @export
with_cohort_connector.cohort_df_connector <- function(connector, fn, ...) {
  fn(connector, ...)
}

#' Disconnect a persistent cohort_omop_connector
#'
#' @param connector A `cohort_connector` object.
#' @return `connector` invisibly, with `$conn` set to `NULL`.
#' @export
disconnect_cohort_connector <- function(connector) {
  if (inherits(connector, "cohort_omop_connector") && !is.null(connector$conn)) {
    DatabaseConnector::disconnect(connector$conn)
    connector$conn <- NULL
    message("Disconnected.")
  }
  invisible(connector)
}

# ---------------------------------------------------------------------------
# Internal: connection error detection
# ---------------------------------------------------------------------------

.is_closed_connection_error <- function(e) {
  msg <- conditionMessage(e)
  grepl(paste0(
    "connection is closed|connection.*closed|closed.*connection|",
    "no operations allowed|",
    "08S01|communication link failure|",
    "\\[Simba\\]|\\[Hardy\\]|",
    "socket.*closed|broken.*pipe|network.*error|network.*reset"
  ), msg, ignore.case = TRUE, perl = TRUE)
}

# ---------------------------------------------------------------------------
# Internal: empty domain tibbles
# ---------------------------------------------------------------------------

.empty_cohort_domain <- function(domain) {
  switch(domain,
    cohort      = tibble::tibble(
      cohort_definition_id = integer(0),
      subject_id           = integer(0),
      cohort_start_date    = as.Date(character(0)),
      cohort_end_date      = as.Date(character(0))
    ),
    person      = tibble::tibble(
      person_id              = integer(0),
      birth_year             = integer(0),
      gender_concept_id      = integer(0),
      gender_name            = character(0),
      race_concept_id        = integer(0),
      race_name              = character(0),
      ethnicity_concept_id   = integer(0),
      ethnicity_name         = character(0)
    ),
    condition   = tibble::tibble(
      condition_occurrence_id = integer(0),
      person_id               = integer(0),
      condition_start_date    = as.Date(character(0)),
      condition_end_date      = as.Date(character(0)),
      condition_concept_id    = integer(0),
      condition_name          = character(0),
      condition_source_value  = character(0)
    ),
    drug        = tibble::tibble(
      drug_exposure_id          = integer(0),
      person_id                 = integer(0),
      drug_exposure_start_date  = as.Date(character(0)),
      drug_exposure_end_date    = as.Date(character(0)),
      drug_concept_id           = integer(0),
      drug_name                 = character(0),
      drug_source_value         = character(0)
    ),
    procedure   = tibble::tibble(
      procedure_occurrence_id = integer(0),
      person_id               = integer(0),
      procedure_date          = as.Date(character(0)),
      procedure_concept_id    = integer(0),
      procedure_name          = character(0),
      procedure_source_value  = character(0)
    ),
    measurement = tibble::tibble(
      measurement_id          = integer(0),
      person_id               = integer(0),
      measurement_date        = as.Date(character(0)),
      measurement_concept_id  = integer(0),
      measurement_name        = character(0),
      value_as_number         = numeric(0),
      unit_name               = character(0),
      measurement_source_value = character(0)
    ),
    observation = tibble::tibble(
      observation_id          = integer(0),
      person_id               = integer(0),
      observation_date        = as.Date(character(0)),
      observation_concept_id  = integer(0),
      observation_name        = character(0),
      value_as_number         = numeric(0),
      value_as_string         = character(0),
      observation_source_value = character(0)
    ),
    visit       = tibble::tibble(
      visit_occurrence_id = integer(0),
      person_id           = integer(0),
      visit_start_date    = as.Date(character(0)),
      visit_end_date      = as.Date(character(0)),
      visit_concept_id    = integer(0),
      visit_type          = character(0),
      visit_source_value  = character(0)
    ),
    death       = tibble::tibble(
      person_id          = integer(0),
      death_date         = as.Date(character(0)),
      death_type_concept_id = integer(0),
      cause_concept_id   = integer(0),
      cause_name         = character(0)
    ),
    tibble::tibble()
  )
}
