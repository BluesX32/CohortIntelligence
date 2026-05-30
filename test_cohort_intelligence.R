# test_cohort_intelligence.R
# Interactive launch script for CohortIntelligence.
# Run interactively in RStudio. Not part of the automated test suite.
#
# -- Which script should I use? -----------------------------------------------
#
#   launch_cohort_intelligence.R  (OHDSI-standard, recommended)
#     Fill in createConnectionDetails() directly.
#     Same fill-in-the-blanks pattern as CohortDiagnostics and other OHDSI tools.
#
#   test_cohort_intelligence.R  (this file -- env-file approach)
#     Reads credentials from a .env or R.env file -- no passwords in code.
#     Also includes a demo mode that runs without any database.
#
# -- Environment file format --------------------------------------------------
#
#   .env  (SQL Server / generic OMOP)
#     SQL_SERVER=myserver.institution.edu
#     SQL_DATABASE=OMOP_CDM
#     SQL_CDM_SCHEMA=dbo
#     SQL_RESULTS_SCHEMA=results
#     SQL_VOCABULARY_SCHEMA=dbo
#     USE_WINDOWS_AUTH=true
#     JDBC_DRIVER_PATH=/path/to/jdbc
#
#   R.env  (Databricks / SAFER / Discovery HPC)
#     DATABRICKS_SERVER_HOSTNAME=adb-xxx.azuredatabricks.net
#     DATABRICKS_HTTP_PATH=/sql/1.0/warehouses/xxx
#     DATABRICKS_TOKEN=dapiXXXXXXXXXXXXXXXX
#     DATABRICKS_DATA_CATALOG=deid
#     DATABRICKS_JDBC_JAR=/path/to/DatabricksJDBC42.jar
#
# =============================================================================

devtools::load_all(".")

# ----------------------------------------------------------------------------
# Demo mode (no database required)
# Launches with 50 synthetic patients. Skip to Step 4 below.
# ----------------------------------------------------------------------------

launch_cohort_intelligence()   # <-- uncomment this line for demo mode

# ----------------------------------------------------------------------------
# Step 1: Connect  --  choose one block, comment out the other
# ----------------------------------------------------------------------------

# SQL Server via .env file
readRenviron(".env")
connector <- create_cohort_omop_connector(
  connectionDetails = DatabaseConnector::createConnectionDetails(
    dbms         = "sql server",
    server       = Sys.getenv("SQL_SERVER"),
    user         = Sys.getenv("SQL_USER"),
    password     = Sys.getenv("SQL_PASSWORD"),
    port         = as.integer(Sys.getenv("SQL_PORT", "1433")),
    pathToDriver = Sys.getenv("JDBC_DRIVER_PATH")
  ),
  cdm_schema    = Sys.getenv("SQL_CDM_SCHEMA"),
  cohort_schema = Sys.getenv("SQL_RESULTS_SCHEMA"),
  vocab_schema  = Sys.getenv("SQL_VOCABULARY_SCHEMA")
)

# Databricks / SAFER via R.env file
readRenviron("R.env")
connector <- create_cohort_omop_connector(
  connectionDetails = DatabaseConnector::createConnectionDetails(
    dbms                = "spark",
    server              = Sys.getenv("DATABRICKS_SERVER_HOSTNAME"),
    httpPath            = Sys.getenv("DATABRICKS_HTTP_PATH"),
    token               = Sys.getenv("DATABRICKS_TOKEN"),
    pathToDriver        = Sys.getenv("DATABRICKS_JDBC_JAR")
  ),
  cdm_schema    = paste0(Sys.getenv("DATABRICKS_DATA_CATALOG"), ".omop"),
  cohort_schema = paste0(Sys.getenv("DATABRICKS_USER_CATALOG"), ".results"),
  vocab_schema  = paste0(Sys.getenv("DATABRICKS_DATA_CATALOG"), ".omop")
)

# ----------------------------------------------------------------------------
# Step 2: Extract cohort
# ----------------------------------------------------------------------------

cohort_members <- extract_cohort_members(connector,
                                          cohort_definition_id = 1L,
                                          cohort_table         = "cohort")
message(nrow(cohort_members), " patients in cohort.")

domain_data <- extract_omop_domains(connector,
                                     subject_ids = cohort_members$subject_id)

# ----------------------------------------------------------------------------
# Step 3: Build features + run ML (skip to Step 4 to use defaults)
# ----------------------------------------------------------------------------

time_windows <- define_time_windows()
domain_act   <- build_domain_activity(cohort_members, domain_data, time_windows)

feature_mat  <- build_feature_matrix(cohort_members, domain_data, time_windows)
ml_results   <- run_full_ml_pipeline(feature_mat$wide)
rank_df      <- rank_patients(ml_results, domain_act, cohort_members)

# ----------------------------------------------------------------------------
# Step 4: Launch
# ----------------------------------------------------------------------------

launch_cohort_intelligence()
