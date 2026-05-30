# test_cohort_intelligence.R
# Interactive launch script for CohortIntelligence.
# Run interactively in RStudio. Not part of the automated test suite.
#
# Demo mode (no database): call launch_cohort_intelligence() with no args.
#
# Supported dbms values: "postgresql", "sql server", "spark", "redshift",
#   "oracle", "bigquery", "snowflake"
# See DatabaseConnector::createConnectionDetails() for all parameters.

devtools::load_all(".")

# ==============================================================================
# STEP 1 -- Fill in your connection details
# ==============================================================================

connection_details <- DatabaseConnector::createConnectionDetails(
  dbms     = "postgresql",
  server   = "yourserver.edu/omop",
  user     = "your_username",
  password = "your_password",
  port     = 5432
)

cdm_schema    <- "cdm"
cohort_schema <- "results"
vocab_schema  <- "vocab"

# ==============================================================================
# STEP 2 -- Launch
# ==============================================================================

launch_cohort_intelligence(
  connection_details   = connection_details,
  cdm_schema           = cdm_schema,
  cohort_schema        = cohort_schema,
  vocab_schema         = vocab_schema,
  cohort_table         = "cohort",
  cohort_definition_id = 1L
)
