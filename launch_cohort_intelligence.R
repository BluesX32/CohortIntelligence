# launch_cohort_intelligence.R
# Fill in your site's values below, then run the whole file.

devtools::load_all(".")

# ==============================================================================
# STEP 1 -- Fill in your connection details
# ==============================================================================

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms         = "sql server",
  server       = "yourserver.edu/OMOP",
  user         = "your_username",
  password     = "your_password",
  port         = 1433,
  pathToDriver = "C:/jdbc"
)

cdmDatabaseSchema    <- "dbo"
cohortDatabaseSchema <- "results"
vocabDatabaseSchema  <- "dbo"
cohortTable          <- "cohort"
cohortDefinitionId   <- 1L

# ==============================================================================
# STEP 2 -- Connect and extract cohort
# ==============================================================================

connector <- create_cohort_omop_connector(
  connectionDetails = connectionDetails,
  cdm_schema        = cdmDatabaseSchema,
  cohort_schema     = cohortDatabaseSchema,
  vocab_schema      = vocabDatabaseSchema
)

cohortMembers <- extract_cohort_members(connector,
                                         cohort_definition_id = cohortDefinitionId,
                                         cohort_table         = cohortTable)
message(nrow(cohortMembers), " patients in cohort.")

domainData <- extract_omop_domains(connector,
                                    subject_ids = cohortMembers$subject_id)

# ==============================================================================
# STEP 3 -- Build features and run ML
# ==============================================================================

timeWindows  <- define_time_windows()
domainAct    <- build_domain_activity(cohortMembers, domainData, timeWindows)
featureMat   <- build_feature_matrix(cohortMembers, domainData, timeWindows)
mlResults    <- run_full_ml_pipeline(featureMat$wide)
rankDf       <- rank_patients(mlResults, domainAct, cohortMembers)

# ==============================================================================
# STEP 4 -- Launch
# ==============================================================================

launch_cohort_intelligence()
