# CohortIntelligence

An OHDSI-native cohort exploration and hypothesis-generation workbench for
**OMOP CDM** studies. CohortIntelligence sits between aggregate cohort
diagnostics and formal downstream analysis. It provides a data-driven layer
that helps researchers identify which cohort patterns, subgroups, and patients
deserve targeted clinical or methodological review before committing to a
formal study design.

---

## Table of contents

1. [Where it fits in the OHDSI workflow](#ohdsi-workflow)
2. [What it is not](#what-it-is-not)
3. [Installation](#installation)
4. [Quick start](#quick-start)
5. [Dashboard overview](#dashboard-overview)
6. [Cohort input](#cohort-input)
7. [How to interpret results](#how-to-interpret-results)
8. [Key functions](#key-functions)
9. [Package structure](#package-structure)
10. [Author](#author)

---

## Where it fits in the OHDSI workflow {#ohdsi-workflow}

```
ATLAS / CohortGenerator
      ↓
  Cohort table or JSON definition
      ↓
  CohortDiagnostics / DataQualityDashboard
  (formal phenotype validation, data quality checks)
      ↓
  ┌─────────────────────────────────────────────────┐
  │  CohortIntelligence                             │  ← YOU ARE HERE
  │  Exploration · Subgroup discovery               │
  │  Patient prioritisation · Hypothesis generation │
  └─────────────────────────────────────────────────┘
      ↓
  Targeted clinical review planning · Clinician discussion
  Formal phenotype validation · Cohort inspection
      ↓
  Formal study design
  (PatientLevelPrediction / CohortMethod / SCCS /
   Incidence rates / Treatment patterns)
```

CohortIntelligence accepts any OMOP cohort — whether defined by an ATLAS JSON
file, a pre-instantiated cohort table, or a synthetic in-memory dataset — and
provides an interactive dashboard to identify which cohort patterns, subgroups,
and patients deserve targeted review before committing to a formal analysis.
It does not replace CohortDiagnostics, formal chart review, or clinical
adjudication. OMOP structured timelines are evidence summaries, not the
complete clinical record.

---

## What it is not {#what-it-is-not}

> **CohortIntelligence does not:**
> - Replace CohortDiagnostics or ATLAS phenotype validation
> - Perform formal statistical inference or causal analysis
> - Make clinical diagnoses or support patient-care decisions
> - Replace formal chart review or clinical adjudication
> - Produce publication-ready results without external validation

All outputs are **hypothesis-generating** and require clinical review. Every
panel uses cautious language: *"potential signal," "possible interpretation,"
"requires clinical validation."*

---

## Installation {#installation}

```r
# Development install from source
devtools::install("path/to/CohortIntelligence")
# or load without installing
devtools::load_all("path/to/CohortIntelligence")
```

**Required ML packages** (install once):
```r
install.packages(c("uwot", "cluster", "isotree"))
```

**OHDSI packages** (not on CRAN):
```r
remotes::install_github("OHDSI/CirceR")
remotes::install_github("OHDSI/SqlRender")
remotes::install_github("OHDSI/DatabaseConnector")
```

| Category | Packages |
|---|---|
| Core (Imports) | `dplyr`, `purrr`, `rlang`, `tibble`, `tidyr` |
| ML pipeline | `uwot` (UMAP), `cluster` (k-means), `isotree` (anomaly) |
| Dashboard UI | `shiny`, `shinydashboard`, `shinyWidgets`, `plotly`, `DT` |
| OMOP connectivity | `DatabaseConnector`, `SqlRender`, `CirceR` |
| Export | `ggplot2`, `htmltools`, `base64enc`, `svglite` |

---

## Quick start {#quick-start}

### Demo mode (no database required)
```r
library(CohortIntelligence)
launch_cohort_intelligence()
# Opens with 50 synthetic patients. All tabs are fully functional.
```

### Live OMOP database with ATLAS JSON
```r
devtools::load_all(".")

connection_details <- DatabaseConnector::createConnectionDetails(
  dbms     = "spark",
  server   = "your-databricks-host/default",
  user     = "token",
  password = "your-pat-token",
  port     = 443,
  pathToDriver = "/path/to/jdbc/"
)
connection <- DatabaseConnector::connect(connection_details)

# Diagnose first -- no writes to CDM
connector <- create_cohort_connector(connection,
                                      cdm_schema   = "your_cdm_schema",
                                      vocab_schema = "your_vocab_schema")
check_cohort_json(connector,
  system.file("template", "T2DM.json", package = "CohortIntelligence"))

# Launch -- cohort loads automatically on browser open
launch_cohort_intelligence(
  connection   = connection,
  cdm_schema   = "your_cdm_schema",
  vocab_schema = "your_vocab_schema",
  json_path    = system.file("template", "T2DM.json",
                              package = "CohortIntelligence")
)
DatabaseConnector::disconnect(connection)
```

> **No write permissions required.** CohortIntelligence runs cohort
> instantiation as a read-only CTE query. No temp tables, no inserts.

---

## Dashboard overview {#dashboard-overview}

The dashboard has **five guided workflow tabs**:

```
Sidebar                       Main panel
─────────────────────────     ────────────────────────────────────────────────
Cohort Overview        →      Quilt plot (patient × domain × time-window)
                              + Demographics summary + Data density heatmap

Cluster & Anomaly      →      UMAP projection + Cluster profile cards
                              + Temporal rule flags

Review Queue           →      Eight guided review set cards
                              + Priority-ranked patient table
                              + Signal Explanation panel

Trajectory Review      →      Per-patient swim-lane timeline
                              + Signal Explanation (why selected)

Hypothesis & Report    →      Candidate hypotheses table
                              + Clinician Review Packet export modal
─────────────────────────     ────────────────────────────────────────────────
```

### Tab 1 — Cohort Overview

The **quilt plot** is the centrepiece. Each row is one patient, each column
group is an OMOP domain, each column within a group is a time window relative
to the cohort index date. Cell colour intensity = event count (log1p scale).

Click any cell to select that patient. The selection propagates to the
Trajectory Review tab and Signal Explanation panel.

### Tab 2 — Cluster & Anomaly Explorer

- **UMAP projection** — patients as points, coloured by cluster. Proximity =
  clinical similarity. Isolated points are candidates for review.
- **Cluster Profile Summary** — one card per cluster with a cautious
  descriptive label, top conditions/drugs, and demographics.
- **Temporal Rule Flags** — rule-based triggers (e.g., no post-index
  follow-up, death shortly after index, isolated index code). Not clinical
  conclusions — review triggers.

### Tab 3 — Review Queue

Eight guided patient sets to structure **cohort inspection, clinician
discussion, and targeted chart-review planning**, ordered from low-risk
(calibration) to high-risk (outlier/data-completeness concern):

| Set | Purpose |
|---|---|
| Typical patients | Use as calibration reference for a plausible cohort member |
| Most anomalous | Review for unusual structured-data patterns or possible data artefacts |
| Sparse follow-up | Assess whether missingness may limit interpretation |
| Rare cluster | Inspect small clusters that may reflect uncommon patterns |
| High post-index activity | Review whether post-index intensity reflects treatment or documentation |
| High pre-index activity | Review whether complex prior history affects cohort entry |
| Boundary patients | Inspect patients with ambiguous cluster assignment |
| Temporal concern | Review temporal flags before interpreting trajectory |

> **Suggested review order:** Start with Typical patients to calibrate
> expectations, then Most Anomalous, then Sparse Follow-up. Temporal
> Concern patients should have flags reviewed before trajectory inspection.

The **Signal Explanation** panel below the queue shows why the selected patient
was prioritised, using severity-tagged cards (red = high, amber = medium, blue = low).
Severity reflects relative prominence within this cohort, not absolute clinical risk.

### Tab 4 — Trajectory Review

Per-patient structured evidence timeline across all OMOP domains, with the
Signal Explanation panel alongside. Use this to inspect whether the structured
OMOP record supports the signal that selected this patient. A "Review context"
card shows the patient's review set, top signal, and temporal flags.

> **Important:** The trajectory shows structured EHR records only — not free
> text, imaging, waveforms, or out-of-network events. It is a structured
> evidence summary, not a substitute for full chart review.

### Tab 5 — Hypothesis & Report

Exploratory cluster-pair feature comparison (Wilcoxon / Fisher's exact tests,
BH-corrected). These are **ranking signals**, not confirmatory statistics —
clusters were derived from the same feature matrix being compared.

A visible warning above the table explains this. Click
**Export Clinician Review Packet** for a self-contained HTML report covering
cohort overview, cluster profiles, review sets, temporal flags, and top
hypotheses — ready for a multidisciplinary discussion meeting.

### Demo story

In demo mode: start with **Cohort Overview** to understand the cohort
landscape (quilt pattern, demographics). Move to **Cluster & Anomaly** to
inspect subgroup structure, cluster profiles, and temporal data-completeness
flags. Use **Review Queue** to select typical patients (calibration), then
most anomalous and sparse patients (prioritised inspection). Open
**Trajectory Review** to inspect the structured OMOP timeline for a selected
patient. Finally, use **Hypothesis & Report** to generate candidate research
questions and export a discussion packet.

---

## Cohort input {#cohort-input}

### Option A — ATLAS JSON (recommended, read-only)
```r
launch_cohort_intelligence(
  connection   = connection,
  cdm_schema   = "schema",
  vocab_schema = "schema",
  json_path    = "path/to/my_cohort.json"
)
```
CirceR compiles the JSON into a single read-only CTE query. No cohort schema,
no temp tables, no write permissions.

**Bundled templates** (`list_cohort_templates()`):

| Template | Cohort | Key inclusion criteria |
|---|---|---|
| `T2DM.json` | Type 2 DM on metformin | T2DM diagnosis + age ≥18 + 365-day prior obs + metformin within 365 days post-index |
| `DM_infection.json` | Adult dermatomyositis (2-hit) | DM diagnosis × 2, 30–365 days apart + age ≥18, excludes juvenile subtypes |

> **Diagnose before launching:**
> ```r
> check_cohort_json(connector, json_path)
> # Returns concept coverage + candidate patient count
> ```

### Option B — Pre-built cohort table
```r
launch_cohort_intelligence(
  connection           = connection,
  cdm_schema           = "schema",
  vocab_schema         = "schema",
  cohort_schema        = "results",
  cohort_table         = "cohort",
  cohort_definition_id = 1L
)
```

### Option C — Upload RDS (no database)
Select *Upload RDS file* in the sidebar. The RDS must be a named list with
slots: `cohort`, `person`, `condition`, `drug`, `procedure`, `measurement`,
`observation`, `visit`, `death`.

---

## How to interpret results {#how-to-interpret-results}

### Guiding principle

Every output from CohortIntelligence is a **structured observation** from
OMOP CDM administrative and clinical records. It is never a clinical diagnosis,
a causal claim, or a validated finding.

### Anomaly scores

Scores range from 0 (typical) to 1 (unusual). Higher scores indicate that a
patient's structured OMOP feature pattern is **less typical relative to the
current cohort**. Thresholds such as 0.7 are heuristic review cutoffs, not
calibrated probabilities. A score of 0.7 means the pattern is relatively
unusual within this specific cohort — it does not imply a fixed clinical
risk level.

Possible causes of a high score: genuine clinical complexity, rare disease
subtype, data coding differences, or data quality issues. All require
clinical review to distinguish.

### Cluster labels

Labels like *"Medication-dense group"* or *"Sparse follow-up group"* are
auto-generated from dominant feature prevalence. They are **descriptive
hypotheses**, not confirmed clinical subtypes. Never report cluster
assignments as diagnoses.

### Temporal flags

Flags are review **triggers**, not errors. A flag like *"No post-index
follow-up"* may reflect: the patient left the system, care was received
elsewhere, data feed lag, or genuinely short follow-up. Check source records.

### Hypotheses

Hypothesis panel results compare features between ML-defined clusters on the
**same data** used to define those clusters (circular). All findings require
replication in an independent dataset or a formally pre-specified analysis.

### Language to use

| Instead of | Use |
|---|---|
| "This cluster is subtype X" | "This cluster may reflect a distinct clinical pattern — requires review" |
| "Patient X has an error" | "Patient X has a potential temporal inconsistency — flag for review" |
| "The data shows Y causes Z" | "There is an association between Y and Z in this cohort — hypothesis-generating only" |

---

## Key functions {#key-functions}

```r
# ── Cohort input ──────────────────────────────────────────────────────────
fetch_cohort_from_json(connector, json_path, verbose = FALSE)
check_cohort_json(connector, json_path)
list_cohort_templates()

# ── Feature engineering ───────────────────────────────────────────────────
define_time_windows(breaks_months = c(-24,-18,-12,-6,0,6,12))
build_domain_activity(cohort_members, domain_data, time_windows)
build_feature_matrix(cohort_members, domain_data, time_windows)
build_quilt_data(domain_activity, rank_df, sort_by = "cluster")

# ── ML pipeline ───────────────────────────────────────────────────────────
run_umap(feature_matrix)                # uwot (pure C++); PCA fallback for large n
run_clustering(umap_coords, method = "kmeans", k = NULL)
run_isolation_forest(feature_matrix)    # isotree; returns 0 scores if not installed
run_full_ml_pipeline(feature_matrix)

# ── Patient ranking ───────────────────────────────────────────────────────
compute_sparsity(domain_activity, time_windows)
rank_patients(ml_results, domain_activity, cohort_members)
                                        # accepts NULL ml_results (sparsity-only)

# ── Signal explanation ────────────────────────────────────────────────────
explain_patient_priority(subject_id, rank_df, feature_matrix,
                          domain_activity, cohort_members,
                          ml_results = NULL, top_n = 8)
# Returns severity-tagged explanation rows: why this patient was prioritised

# ── Cluster profiles ──────────────────────────────────────────────────────
build_cluster_profiles(rank_df, domain_data, cohort_members,
                        person_data = NULL, top_n = 10L)
label_clusters(profiles)                # e.g. "Medication-dense group"
summarize_cluster_profile(profile_row, concepts_df, cluster_label = NULL)
compare_cluster_profiles(concepts_df, cluster_a, cluster_b, top_n = 5)

# ── Review sets ───────────────────────────────────────────────────────────
build_review_sets(rank_df, domain_activity, feature_matrix,
                   ml_results, cohort_members,
                   temporal_flags = NULL, n_per_set = 10L)

# ── Temporal rule flags ───────────────────────────────────────────────────
temporal_flag_config(exposure_domains = c("drug"),
                      outcome_domains  = c("condition","visit","death"),
                      death_window_days = 90L, ...)
detect_temporal_flags(cohort_members, domain_data,
                       time_windows = define_time_windows(),
                       config = NULL)

# ── Trajectory visualisation ──────────────────────────────────────────────
build_patient_timeline(subject_id, domain_data, cohort_members)
plot_patient_timeline(timeline_df, interactive = TRUE)

# ── Hypothesis generation ─────────────────────────────────────────────────
generate_hypotheses(feature_matrix, ml_results,
                     min_effect_size = 0.1, max_hypotheses = 20L)

# ── Export ────────────────────────────────────────────────────────────────
export_cohort_results(results, path, formats = c("rds","csv"))
export_quilt_plot(quilt_data, path, format = "png")
export_cohort_report(results, path, cohort_name = "Cohort")

build_clinician_review_packet(cohort_summary, cluster_profiles,
                               review_sets, temporal_flags,
                               hypotheses, selected_patients,
                               patient_timelines)
export_clinician_review_packet(results, path,
                                cohort_name = "Cohort",
                                n_patients  = 10L,
                                include     = list(typical=TRUE, ...))

# ── Launch ────────────────────────────────────────────────────────────────
launch_cohort_intelligence()                        # demo mode
launch_cohort_intelligence(                         # live OMOP + JSON
  connection   = connection,
  cdm_schema   = "schema",
  vocab_schema = "schema",
  json_path    = "path/to/cohort.json"
)
```

---

## Package structure {#package-structure}

```
R/
  app.R                  launch_cohort_intelligence(); startup ML package check
  CohortIntelligence-package.R  globalVariables; %||%; stats imports
  cohort.R               fetch_cohort_from_json() (read-only CTE);
                         check_cohort_json(); list_cohort_templates()
  connect.R              S3 connector classes; stale-connection retry
  explain.R              explain_patient_priority() -- signal explanation
  export.R               export_cohort_results(); export_quilt_plot();
                         export_cohort_report();
                         build/export_clinician_review_packet()
  extract.R              OMOP query layer; batched IN-clause extraction
  features.R             define_time_windows(); build_domain_activity();
                         build_feature_matrix(); build_quilt_data();
                         build_cohort_summary(); build_data_density();
                         build_cluster_profiles(); label_clusters();
                         summarize_cluster_profile(); compare_cluster_profiles()
  anomaly.R              run_umap() (uwot + PCA fallback);
                         run_clustering(); run_isolation_forest();
                         run_full_ml_pipeline()
  rank.R                 compute_sparsity(); rank_patients()
                         (graceful NULL ml_results fallback)
  review_sets.R          temporal_flag_config(); build_review_sets()
  temporal_flags.R       detect_temporal_flags() -- 7 OMOP-generic rules
  trajectory.R           build_patient_timeline(); plot_patient_timeline()
  hypotheses.R           generate_hypotheses(); format_hypotheses_report()

inst/template/           ATLAS cohort definition JSON templates
  T2DM.json              Type 2 DM on metformin (strict 365-day obs window)
  DM_infection.json      Adult dermatomyositis, 2-hit phenotype

inst/sql/                SqlRender-parameterized SQL (7 OMOP domains + cohort)

inst/shiny/
  global.R               library(CohortIntelligence) guard; empty_plotly()
  ui.R                   5-tab workflow layout
  server.R               Shared reactive state; auto-load pipeline;
                         temporal flags + review sets computed on load
  modules/
    cohort_overview.R    Reactive quilt heatmap (plotly subplot)
    anomaly_explorer.R   UMAP scatter + anomaly histogram + top-50 table
    cluster_profile.R    Cluster summary cards + prevalence bar chart
    temporal_flags.R     Temporal rule flag summary + filtered table
    review_sets.R        Eight guided set cards + priority queue table
    signal_explanation.R Per-patient severity-tagged priority cards
    trajectory_viewer.R  Per-patient OMOP swim-lane timeline
    demographics.R       Value boxes + age/sex/race + data density heatmap
    hypothesis_panel.R   Cluster-pair feature comparison + re-run button
    patient_selector.R   (legacy; kept for backward compatibility)

tests/testthat/
  helper-synthetic.R     make_test_cohort() -- no database required
  test-connect.R         connector construction and dispatch
  test-extract.R         domain extraction column contracts
  test-features.R        time windows, domain activity, quilt structure
  test-quilt.R           14 quilt structural and encoding tests
  test-anomaly.R         UMAP, clustering, isolation forest
  test-rank.R            composite ranking, tier assignment
  test-hypotheses.R      hypothesis generation, empty clusters
  test-explain.R         signal explanation rows, severity labels
  test-cluster-profiles.R  cluster label generation, narrative
  test-review-sets.R     review set construction, set names
  test-temporal-flags.R  flag detection, severity, empty cohort
  test-export.R          clinician report file creation

vignettes/
  quickstart.Rmd         Demo mode quick start
  atlas-json-workflow.Rmd  ATLAS JSON cohort loading end-to-end
  interpreting-results.Rmd Anomaly scores, temporal flags, hypotheses
  clinician-report.Rmd   Exporting a clinician review packet

launch_cohort_intelligence.R  <- Start here for live OMOP
test_cohort_intelligence.R    Connection templates + diagnostic workflow
```

---

## Author {#author}

Minqi Xiong — Johns Hopkins University — mxiong5@jhu.edu

---

*CohortIntelligence is intended for research use only. It is not a clinical
decision-support tool and must not be used to make patient-care decisions.*
