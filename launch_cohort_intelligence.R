# launch_cohort_intelligence.R
# Fill in your site's values below, then run the whole file.

devtools::load_all(".")

# ==============================================================================
# STEP 1 -- Fill in your connection details
# ==============================================================================

connection_details <- DatabaseConnector::createConnectionDetails(
  dbms         = "sql server",
  server       = "yourserver.edu/OMOP",
  user         = "your_username",
  password     = "your_password",
  port         = 1433,
  pathToDriver = "C:/jdbc"
)

cdm_schema    <- "dbo"
cohort_schema <- "results"   # schema holding the cohort table
vocab_schema  <- "dbo"

# ==============================================================================
# STEP 2 -- Specify your cohort
# ==============================================================================
# cohort_definition_id must match the ID in your cohort table.
# cohort_table is the table name inside cohort_schema.

cohort_definition_id <- 1L
cohort_table         <- "cohort"

# ==============================================================================
# STEP 3 -- Connect and extract
# ==============================================================================

connector <- create_cohort_omop_connector(
  connectionDetails = connection_details,
  cdm_schema        = cdm_schema,
  cohort_schema     = cohort_schema,
  vocab_schema      = vocab_schema
)

cohort_members <- extract_cohort_members(connector,
                                          cohort_definition_id = cohort_definition_id,
                                          cohort_table         = cohort_table)
message(nrow(cohort_members), " patients in cohort.")

domain_data <- extract_omop_domains(connector,
                                     subject_ids = cohort_members$subject_id)

# ==============================================================================
# STEP 4 -- Build features and run ML (optional but enables quilt sorting)
# ==============================================================================

time_windows  <- define_time_windows()
domain_act    <- build_domain_activity(cohort_members, domain_data, time_windows)

feature_mat   <- build_feature_matrix(cohort_members, domain_data, time_windows)
ml_results    <- run_full_ml_pipeline(feature_mat$wide)
rank_df       <- rank_patients(ml_results, domain_act, cohort_members)
quilt_base    <- build_quilt_data(domain_act, rank_df)

# ==============================================================================
# STEP 5 -- Launch
# ==============================================================================

launch_cohort_intelligence()
