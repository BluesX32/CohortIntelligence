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

cdm_schema   <- "cdm"
vocab_schema <- "vocab"

# ==============================================================================
# STEP 2 -- Connect
# ==============================================================================

connection <- DatabaseConnector::connect(connection_details)

# ==============================================================================
# STEP 3 -- Specify cohort via ATLAS JSON
# ==============================================================================

json_path <- system.file(
  "template", "DM_infection.json",
  package = "CohortIntelligence"
)

# ==============================================================================
# STEP 4 -- Launch
# ==============================================================================

launch_cohort_intelligence(
  connection   = connection,
  cdm_schema   = cdm_schema,
  vocab_schema = vocab_schema,
  json_path    = json_path
)

DatabaseConnector::disconnect(connection)
