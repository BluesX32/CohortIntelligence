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

cdm_schema   <- "dbo"
vocab_schema <- "dbo"

# ==============================================================================
# STEP 2 -- Specify your cohort
#   Option A: ATLAS JSON file (recommended -- no cohort schema required)
#   Option B: Pre-built cohort table
# ==============================================================================

# Option A -- ATLAS JSON
json_path <- system.file("template", "DM_infection.json",
                          package = "CohortIntelligence")

# Option B -- pre-built cohort table (comment out if using Option A)
# json_path     <- NULL
# cohort_schema <- "results"
# cohort_table  <- "cohort"
# cohort_definition_id <- 1L

# ==============================================================================
# STEP 3 -- Launch
# ==============================================================================

launch_cohort_intelligence(
  connection_details = connection_details,
  cdm_schema         = cdm_schema,
  vocab_schema       = vocab_schema,
  json_path          = json_path
)
