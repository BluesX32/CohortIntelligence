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
# STEP 2 -- Connect
# ==============================================================================

connection <- DatabaseConnector::connect(connection_details)

# ==============================================================================
# STEP 3 -- Point to your ATLAS cohort JSON
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
