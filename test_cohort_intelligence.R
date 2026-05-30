# test_cohort_intelligence.R
# Interactive launch script for CohortIntelligence.
# Run interactively in RStudio. Not part of the automated test suite.
#
# Demo mode (no database): run launch_cohort_intelligence() with no arguments.
#
# Supported dbms values: "postgresql", "sql server", "spark", "redshift",
#   "oracle", "bigquery", "snowflake"
# See DatabaseConnector::createConnectionDetails() for all parameters.

devtools::load_all(".")

# ==============================================================================
# STEP 1 -- Fill in your connection details
# ==============================================================================

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms         = "postgresql",
  server       = "yourserver.edu/omop",
  user         = "your_username",
  password     = "your_password",
  port         = 5432
)

cdmDatabaseSchema    <- "cdm"
cohortDatabaseSchema <- "results"
vocabDatabaseSchema  <- "vocab"
cohortTable          <- "cohort"
cohortDefinitionId   <- 1L

# ==============================================================================
# STEP 2 -- Extract cohort
# ==============================================================================

connector <- create_cohort_omop_connector(
  connectionDetails = connectionDetails,
  cdm_schema        = cdmDatabaseSchema,
  cohort_schema     = cohortDatabaseSchema,
  vocab_schema      = vocabDatabaseSchema
)

cohortMembers <- extract_cohort_members(
  connector,
  cohort_definition_id = cohortDefinitionId,
  cohort_table         = cohortTable
)
message(nrow(cohortMembers), " patients in cohort.")

domainData <- extract_omop_domains(
  connector,
  subject_ids = cohortMembers$subject_id
)

# ==============================================================================
# STEP 3 -- Build features and run ML
# ==============================================================================

timeWindows <- define_time_windows()
domainAct   <- build_domain_activity(cohortMembers, domainData, timeWindows)
featureMat  <- build_feature_matrix(cohortMembers, domainData, timeWindows)
mlResults   <- run_full_ml_pipeline(featureMat$wide)
rankDf      <- rank_patients(mlResults, domainAct, cohortMembers)

# ==============================================================================
# STEP 4 -- Launch
# ==============================================================================

launch_cohort_intelligence()
