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
cohort_schema <- "results"
vocab_schema  <- "dbo"

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
