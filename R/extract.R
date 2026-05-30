# extract.R
# OMOP CDM query layer for CohortIntelligence.
# S3 generics dispatch on cohort_connector type.
# omop methods require DatabaseConnector + SqlRender (guarded by connect.R).
# df methods operate directly on in-memory tibbles.

# ---------------------------------------------------------------------------
# Internal SQL helpers
# ---------------------------------------------------------------------------

.cohort_sql_path <- function(filename) {
  path <- system.file("sql", filename, package = "CohortIntelligence")
  if (nzchar(path) && file.exists(path)) return(path)
  src <- file.path(getwd(), "inst", "sql", filename)
  if (file.exists(src)) return(src)
  rlang::abort(paste0(
    "SQL template '", filename, "' not found. ",
    "Working directory must be the package root (folder containing DESCRIPTION)."
  ))
}

query_cohort <- function(connector, sql_path, params) {
  if (is.null(connector$conn)) {
    rlang::abort("query_cohort() requires an active connection. Use with_cohort_connector().")
  }
  dbms    <- connector$dbms %||% "sql server"
  sql_raw <- SqlRender::readSql(sql_path)
  sql     <- do.call(SqlRender::render, c(list(sql = sql_raw), params))
  sql     <- SqlRender::translate(sql, targetDialect = dbms)

  if (inherits(connector$conn, "JDBCConnection")) {
    as.data.frame(DBI::dbGetQuery(connector$conn, sql))
  } else {
    DatabaseConnector::querySql(connector$conn, sql, snakeCaseToCamelCase = FALSE)
  }
}

.normalise_names <- function(df) {
  names(df) <- tolower(names(df))
  df
}

.to_date_cols <- function(df, cols) {
  for (col in intersect(cols, names(df))) {
    df[[col]] <- as.Date(df[[col]])
  }
  df
}

# ---------------------------------------------------------------------------
# extract_cohort_members
# ---------------------------------------------------------------------------

#' Extract cohort member records
#'
#' @param connector A `cohort_connector` object.
#' @param cohort_definition_id `integer(1)`. Cohort definition ID to filter.
#' @param cohort_table `character(1)`. Name of the cohort table.
#'
#' @return tibble(subject_id, cohort_start_date, cohort_end_date)
#' @export
extract_cohort_members <- function(connector,
                                    cohort_definition_id = 1L,
                                    cohort_table         = "cohort") {
  UseMethod("extract_cohort_members")
}

#' @export
extract_cohort_members.cohort_omop_connector <- function(connector,
                                                          cohort_definition_id = 1L,
                                                          cohort_table         = "cohort") {
  with_cohort_connector(connector, function(active) {
    df <- query_cohort(active, .cohort_sql_path("extract_cohort.sql"), list(
      cohort_schema        = active$cohort_schema,
      cohort_table         = cohort_table,
      cohort_definition_id = as.integer(cohort_definition_id)
    ))
    df <- .normalise_names(df)
    df <- .to_date_cols(df, c("cohort_start_date", "cohort_end_date"))
    tibble::as_tibble(df)
  })
}

#' @export
extract_cohort_members.cohort_df_connector <- function(connector,
                                                        cohort_definition_id = 1L,
                                                        cohort_table         = "cohort") {
  df <- connector$cohort_data$cohort
  if (nrow(df) == 0L) return(.empty_cohort_domain("cohort")[, c("subject_id","cohort_start_date","cohort_end_date")])
  df[df$cohort_definition_id == as.integer(cohort_definition_id), , drop = FALSE] |>
    dplyr::select(dplyr::all_of(c("subject_id", "cohort_start_date", "cohort_end_date"))) |>
    tibble::as_tibble()
}

# ---------------------------------------------------------------------------
# extract_person_demographics
# ---------------------------------------------------------------------------

#' Extract person-level demographic information
#'
#' @param connector A `cohort_connector` object.
#' @param subject_ids Integer vector of patient IDs.
#'
#' @return tibble with person demographics.
#' @export
extract_person_demographics <- function(connector, subject_ids) {
  UseMethod("extract_person_demographics")
}

#' @export
extract_person_demographics.cohort_omop_connector <- function(connector, subject_ids) {
  subject_ids <- as.integer(subject_ids)
  batches <- .make_id_batches(subject_ids, 500L)
  purrr::map_dfr(batches, function(ids) {
    with_cohort_connector(connector, function(active) {
      df <- query_cohort(active, .cohort_sql_path("extract_person.sql"), list(
        cdm_schema   = active$cdm_schema,
        vocab_schema = active$vocab_schema,
        subject_ids  = ids
      ))
      df <- .normalise_names(df)
      tibble::as_tibble(df)
    })
  })
}

#' @export
extract_person_demographics.cohort_df_connector <- function(connector, subject_ids) {
  subject_ids <- as.integer(subject_ids)
  df <- connector$cohort_data$person
  tibble::as_tibble(df[df$person_id %in% subject_ids, , drop = FALSE])
}

# ---------------------------------------------------------------------------
# extract_omop_domains
# ---------------------------------------------------------------------------

#' Extract OMOP domain records for a set of cohort members
#'
#' @param connector A `cohort_connector` object.
#' @param subject_ids Integer vector from [extract_cohort_members()].
#' @param domains Character vector of domains to extract. One or more of
#'   `"condition"`, `"drug"`, `"procedure"`, `"measurement"`, `"observation"`,
#'   `"visit"`, `"death"`.
#' @param date_range Optional `c(start_date, end_date)` as Date or character.
#'   `NULL` retrieves all dates.
#' @param batch_size Integer. Number of subject IDs per SQL IN clause. Default 500.
#'
#' @return Named list with one tibble per requested domain.
#' @export
extract_omop_domains <- function(connector,
                                  subject_ids,
                                  domains    = c("condition","drug","procedure",
                                                 "measurement","observation","visit","death"),
                                  date_range = NULL,
                                  batch_size = 500L) {
  UseMethod("extract_omop_domains")
}

#' @export
extract_omop_domains.cohort_omop_connector <- function(connector,
                                                        subject_ids,
                                                        domains    = c("condition","drug","procedure",
                                                                       "measurement","observation","visit","death"),
                                                        date_range = NULL,
                                                        batch_size = 500L) {
  subject_ids <- as.integer(subject_ids)
  sd <- format(as.Date(date_range[[1]] %||% "1900-01-01"), "%Y-%m-%d")
  ed <- format(as.Date(date_range[[2]] %||% Sys.Date()), "%Y-%m-%d")
  batches <- .make_id_batches(subject_ids, batch_size)

  result <- setNames(
    lapply(domains, function(domain) {
      message(sprintf("Extracting %s (%d batches)...", domain, length(batches)))
      purrr::map_dfr(batches, function(ids) {
        .extract_domain_batch(connector, domain, ids, sd, ed)
      })
    }),
    domains
  )
  result
}

#' @export
extract_omop_domains.cohort_df_connector <- function(connector,
                                                      subject_ids,
                                                      domains    = c("condition","drug","procedure",
                                                                     "measurement","observation","visit","death"),
                                                      date_range = NULL,
                                                      batch_size = 500L) {
  subject_ids <- as.integer(subject_ids)
  sd <- as.Date(date_range[[1]] %||% "1900-01-01")
  ed <- as.Date(date_range[[2]] %||% Sys.Date())

  domain_map <- list(
    condition   = list(slot = "condition",   date_col = "condition_start_date"),
    drug        = list(slot = "drug",        date_col = "drug_exposure_start_date"),
    procedure   = list(slot = "procedure",   date_col = "procedure_date"),
    measurement = list(slot = "measurement", date_col = "measurement_date"),
    observation = list(slot = "observation", date_col = "observation_date"),
    visit       = list(slot = "visit",       date_col = "visit_start_date"),
    death       = list(slot = "death",       date_col = "death_date")
  )

  setNames(
    lapply(domains, function(d) {
      info <- domain_map[[d]]
      df   <- connector$cohort_data[[info$slot]]
      if (nrow(df) == 0L) return(.empty_cohort_domain(d))
      df <- df[df$person_id %in% subject_ids, , drop = FALSE]
      if (info$date_col %in% names(df)) {
        df <- df[!is.na(df[[info$date_col]]) &
                   df[[info$date_col]] >= sd &
                   df[[info$date_col]] <= ed, , drop = FALSE]
      }
      tibble::as_tibble(df)
    }),
    domains
  )
}

# ---------------------------------------------------------------------------
# Internal: per-domain batch query
# ---------------------------------------------------------------------------

.extract_domain_batch <- function(connector, domain, subject_ids, start_date, end_date) {
  sql_file <- paste0("extract_", domain, ".sql")
  with_cohort_connector(connector, function(active) {
    df <- query_cohort(active, .cohort_sql_path(sql_file), list(
      cdm_schema   = active$cdm_schema,
      vocab_schema = active$vocab_schema,
      subject_ids  = subject_ids,
      start_date   = start_date,
      end_date     = end_date
    ))
    df <- .normalise_names(df)
    date_cols <- switch(domain,
      condition   = c("condition_start_date", "condition_end_date"),
      drug        = c("drug_exposure_start_date", "drug_exposure_end_date"),
      procedure   = "procedure_date",
      measurement = "measurement_date",
      observation = "observation_date",
      visit       = c("visit_start_date", "visit_end_date"),
      death       = "death_date"
    )
    df <- .to_date_cols(df, date_cols)
    tibble::as_tibble(df)
  })
}

# ---------------------------------------------------------------------------
# Internal: batch helper
# ---------------------------------------------------------------------------

.make_id_batches <- function(ids, batch_size) {
  if (length(ids) == 0L) return(list())
  idx <- seq_along(ids)
  split(ids, ceiling(idx / batch_size))
}
