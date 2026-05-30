# CohortIntelligence

An OHDSI-native cohort exploration and hypothesis-generation workbench for
**OMOP CDM** studies. Helps researchers understand what clinically meaningful
patterns, unusual subgroups, and hypothesis-generating signals exist inside an
existing cohort -- filling the gap between aggregate diagnostics and formal
chart review.

---

## Table of contents

1. [What it does](#what-it-does)
2. [Installation](#installation)
3. [Getting started](#getting-started)
4. [Cohort input](#cohort-input)
5. [Dashboard overview](#dashboard-overview)
6. [Connection setup](#connection-setup)
7. [Key functions](#key-functions)
8. [Package structure](#package-structure)

---

## What it does

CohortIntelligence is not a replacement for ATLAS, CohortDiagnostics, or
formal chart review. It complements the standard OHDSI workflow by adding a
**data-driven cohort intelligence layer** between aggregate diagnostics and
downstream study design.

| Use case | Description |
|---|---|
| Cohort sanity check | Are the selected patients clinically plausible? Which look typical, which look unusual? |
| Research question discovery | Identify high-risk subgroups, unexpected medication patterns, or phenotype artifacts |
| Patient selection for review | Rank patients by anomaly score, cluster membership, or data sparsity |
| Temporal pattern exploration | Explore episode structure, exposure-outcome timing, and medication changes |
| Clinician meeting preparation | Generate structured trajectory summaries and cohort-level signal reports |

---

## Installation

```r
# Install from source
devtools::install("path/to/CohortIntelligence")

# Or load without installing (development)
devtools::load_all("path/to/CohortIntelligence")
```

**Required:** `dplyr`, `purrr`, `rlang`, `tibble`, `tidyr`  
**Shiny UI:** `shiny`, `shinydashboard`, `shinyWidgets`, `plotly`, `DT`  
**Cohort JSON:** `CirceR`, `SqlRender` (OHDSI GitHub packages)  
**ML pipeline:** `umap`, `cluster`, `isotree` (optional -- demo works without)  
**Live OMOP:** `DatabaseConnector`

Install OHDSI packages:

```r
remotes::install_github("OHDSI/CirceR")
remotes::install_github("OHDSI/SqlRender")
remotes::install_github("OHDSI/DatabaseConnector")
```

---

## Getting started

### Option A -- Demo mode (no database needed)

```r
library(CohortIntelligence)
launch_cohort_intelligence()
```

Opens immediately with 50 synthetic patients. No credentials required. All
five dashboard panels are fully functional.

---

### Option B -- Live OMOP database with ATLAS JSON (recommended)

Open **`launch_cohort_intelligence.R`**, fill in the blanks, and run it.

```r
devtools::load_all(".")

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

# Point to your ATLAS cohort JSON -- no cohort schema needed
json_path <- "inst/template/DM_infection.json"

launch_cohort_intelligence(
  connection_details = connection_details,
  cdm_schema         = cdm_schema,
  vocab_schema       = vocab_schema,
  json_path          = json_path
)
```

> **JDBC drivers** are not bundled. Download once with:
> ```r
> DatabaseConnector::downloadJdbcDrivers("sql server", pathToDriver = "C:/jdbc")
> ```

---

## Cohort input

There are three ways to define the study cohort.

### 1. ATLAS JSON file (recommended)

Pass a path to any ATLAS cohort definition JSON via `json_path`. The package
uses `CirceR` to compile the JSON into SQL, executes it against the CDM into
a session temp table, and reads the resulting members. No cohort schema, no
pre-existing cohort table, and no ATLAS server connection required.

```r
launch_cohort_intelligence(
  connection_details = connection_details,
  cdm_schema         = "dbo",
  vocab_schema       = "dbo",
  json_path          = "path/to/my_cohort.json"
)
```

Bundled templates are in `inst/template/`. List them with:

```r
list_cohort_templates()
```

| Template | Cohort |
|---|---|
| `DM_infection.json` | Adult dermatomyositis (excludes juvenile/neonatal subtypes) |

### 2. Pre-built cohort table

If the cohort has already been instantiated into a results schema (e.g. via
ATLAS, `CohortGenerator`, or `CohortDiagnostics`):

```r
launch_cohort_intelligence(
  connection_details   = connection_details,
  cdm_schema           = "dbo",
  vocab_schema         = "dbo",
  cohort_schema        = "results",
  cohort_table         = "cohort",
  cohort_definition_id = 1L
)
```

### 3. Uploaded RDS file (no database)

Select *"Upload RDS file"* in the sidebar and upload a named list saved with
`saveRDS()`. The list must have slots matching the domain names used by
`create_cohort_df_connector()`: `cohort`, `person`, `condition`, `drug`,
`procedure`, `measurement`, `observation`, `visit`, `death`.

---

## Dashboard overview

```
+-- Sidebar ----------------+  +-- Main content ----------------------------------------+
|                           |  |                                                          |
|  Data Source              |  |  Cohort Overview -- Reactive Quilt Plot                 |
|  [Demo / Upload / OMOP]   |  |  Patient x domain x time-window heatmap.                |
|  [Load Cohort]            |  |  Click any cell to select a patient.                    |
|                           |  |                                                          |
|  Cohort Overview          |  |  Anomaly Explorer                                       |
|  Anomaly Explorer         |  |  UMAP projection coloured by cluster.                   |
|  Patient Selector         |  |  Isolation forest anomaly score distribution.            |
|  Trajectory Viewer        |  |                                                          |
|  Hypothesis Panel         |  |  Patient Selector                                       |
|                           |  |  Priority-ranked review queue.                          |
+---------------------------+  |                                                          |
                               |  Trajectory Viewer                                      |
                               |  Per-patient swim-lane across all OMOP domains.         |
                               |                                                          |
                               |  Hypothesis Panel                                       |
                               |  Cluster-comparison tests on demand. Download CSV.      |
                               +----------------------------------------------------------+
```

### Cohort Overview -- Quilt Plot

The quilt is the centrepiece of the dashboard. It renders a
**patient x domain x time-window heatmap** with one column group per clinical
domain (condition, drug, procedure, measurement, observation, visit).

| Control | Effect |
|---|---|
| Cluster filter | Show only selected clusters |
| Domain filter | Show only selected domains |
| Priority tier | Filter to high / medium / low priority patients |
| Sort patients by | Cluster / rank score / subject ID |
| Cell value | log1p(count) / binary / raw count |
| Time window slider | Restrict visible time windows |
| Click a cell | Selects that patient across all panels |
| Download PNG / SVG | Export static ggplot2 version |

Colour palette is domain-specific (red = conditions, blue = drugs, green =
procedures, purple = measurements, orange = observations, teal = visits).
Selected patients are highlighted with a black border across all panels.

### Anomaly Explorer

UMAP scatter coloured by cluster, isolation forest anomaly score histogram,
and table of the 50 most anomalous patients. Clicking a point or table row
selects that patient.

### Patient Selector

Priority-ranked review queue showing rank score, anomaly score, cluster, and
sparsity score. Clicking a row selects the patient and populates the
Trajectory Viewer.

### Trajectory Viewer

Per-patient swim-lane timeline across all active OMOP domains. Filter by
domain, top-N concepts, and day range relative to the cohort index date.

### Hypothesis Panel

On-demand Wilcoxon rank-sum (continuous) and Fisher's exact (binary) tests
comparing feature distributions between cluster pairs, with BH correction.
Produces a ranked table of candidate research questions. Exportable to CSV.

---

## Connection setup

The connection pattern follows the OHDSI HADES convention used by
CohortDiagnostics, PatientLevelPrediction, and other HADES studies.

```r
DatabaseConnector::createConnectionDetails(
  dbms         = "postgresql",
  server       = "yourserver.edu/omop",
  user         = "your_username",
  password     = "your_password",
  port         = 5432
)
```

Supported `dbms` values: `"sql server"`, `"postgresql"`, `"spark"`,
`"redshift"`, `"oracle"`, `"bigquery"`, `"snowflake"`.

### OMOP tables used

| Table | Purpose |
|---|---|
| `condition_occurrence` | Diagnosis history; cohort instantiation from JSON |
| `drug_exposure` | Medication history |
| `procedure_occurrence` | Procedures |
| `measurement` | Lab values and biomarkers |
| `observation` | Clinical observations |
| `visit_occurrence` | Healthcare encounters |
| `death` | Mortality |
| `concept` | Concept name lookups (vocab schema) |
| `concept_ancestor` | Descendant concept expansion for JSON cohorts |

All queries are rendered via `SqlRender` for cross-DBMS compatibility.

---

## Key functions

```r
# -- Cohort input ------------------------------------------------------------
fetch_cohort_from_json(connector, json_path)   # ATLAS JSON -> cohort members
list_cohort_templates()                         # list bundled JSON templates
extract_cohort_members(connector)               # pre-built cohort table path

# -- Launch ------------------------------------------------------------------
launch_cohort_intelligence()                   # demo mode (no database)
launch_cohort_intelligence(                    # ATLAS JSON path (recommended)
  connection_details = connection_details,
  cdm_schema         = "dbo",
  vocab_schema       = "dbo",
  json_path          = "inst/template/DM_infection.json"
)
launch_cohort_intelligence(                    # pre-built cohort table
  connection_details   = connection_details,
  cdm_schema           = "dbo",
  vocab_schema         = "dbo",
  cohort_schema        = "results",
  cohort_table         = "cohort",
  cohort_definition_id = 1L
)

# -- Feature engineering -----------------------------------------------------
define_time_windows(breaks_months = c(-24, -18, -12, -6, 0, 6, 12))
build_domain_activity(cohort_members, domain_data, time_windows)
build_feature_matrix(cohort_members, domain_data, time_windows)
build_quilt_data(domain_activity, rank_df, sort_by = "cluster")

# -- Unsupervised ML ---------------------------------------------------------
run_umap(feature_matrix)
run_clustering(umap_coords, method = "kmeans", k = NULL)
run_isolation_forest(feature_matrix)
run_full_ml_pipeline(feature_matrix)

# -- Patient ranking ---------------------------------------------------------
compute_sparsity(domain_activity, time_windows)
rank_patients(ml_results, domain_activity, cohort_members)

# -- Trajectory visualization ------------------------------------------------
build_patient_timeline(subject_id, domain_data, cohort_members)
plot_patient_timeline(timeline_df, interactive = TRUE)

# -- Hypothesis generation ---------------------------------------------------
generate_hypotheses(feature_matrix, ml_results)
format_hypotheses_report(hypotheses_df, format = "text")

# -- Export ------------------------------------------------------------------
export_cohort_results(results, path, formats = c("rds", "csv"))
export_quilt_plot(quilt_data, path, format = "png")
```

---

## Package structure

```
R/
  app.R                  launch_cohort_intelligence(); .cohort_intel_env
  cohort.R               fetch_cohort_from_json(); list_cohort_templates()
  connect.R              cohort_connector S3 class; stale-connection retry
  extract.R              OMOP query layer; batched IN-clause extraction
  features.R             define_time_windows(), build_domain_activity(),
                         build_feature_matrix(), build_quilt_data()
  anomaly.R              run_umap(), run_clustering(), run_isolation_forest(),
                         run_full_ml_pipeline()
  rank.R                 compute_sparsity(), rank_patients()
  trajectory.R           build_patient_timeline(), plot_patient_timeline()
  hypotheses.R           generate_hypotheses(), format_hypotheses_report()
  export.R               export_cohort_results(), export_quilt_plot()

inst/template/           ATLAS cohort definition JSON templates
  DM_infection.json      Adult dermatomyositis (excludes juvenile subtypes)

inst/sql/                SqlRender-parameterized SQL templates
  extract_condition.sql  condition_occurrence (+ others per domain)
  extract_person.sql
  ...

inst/shiny/
  global.R               library(CohortIntelligence) -- sourced first
  ui.R                   shinydashboard layout (5 tabs)
  server.R               Shared reactive state; cohort load pipeline
  modules/
    cohort_overview.R    Reactive quilt plot (plotly heatmap subplot)
    anomaly_explorer.R   UMAP scatter + anomaly score table
    patient_selector.R   Priority-ranked review queue (DT)
    trajectory_viewer.R  Per-patient swim-lane (plotly)
    hypothesis_panel.R   Cluster-comparison hypothesis generation

tests/testthat/
  helper-synthetic.R     make_test_cohort() -- no database required
  test-connect.R
  test-extract.R
  test-features.R
  test-quilt.R           14 structural, encoding, and sort-order tests
  test-anomaly.R
  test-rank.R
  test-hypotheses.R

launch_cohort_intelligence.R    <- Start here (OHDSI-standard, JSON path)
test_cohort_intelligence.R      Multiple dbms examples; JSON and table paths
```

---

## Author

Minqi Xiong -- Johns Hopkins University -- mxiong5@jhu.edu
