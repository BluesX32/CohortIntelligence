# server.R — CohortIntelligence Shiny dashboard server

source_modules <- function(dir) {
  for (f in list.files(dir, pattern = "\\.R$", full.names = TRUE)) source(f)
}
source_modules(file.path(dirname(sys.frame(1)$ofile), "modules"))

function(input, output, session) {

  # ── Shared reactive state ─────────────────────────────────────────────────
  rv_selected_patient  <- shiny::reactiveVal(NULL)
  rv_quilt_base        <- shiny::reactiveVal(NULL)
  rv_domain_data       <- shiny::reactiveVal(NULL)
  rv_cohort_members    <- shiny::reactiveVal(NULL)
  rv_feature_matrix    <- shiny::reactiveVal(NULL)
  rv_ml_results        <- shiny::reactiveVal(NULL)
  rv_rank_df           <- shiny::reactiveVal(NULL)
  rv_time_win_labels   <- shiny::reactiveVal(character(0))

  # ── Load status UI ────────────────────────────────────────────────────────
  output$load_status <- shiny::renderUI({
    if (is.null(rv_quilt_base())) {
      shiny::p("No cohort loaded.", style = "color: #b8c7ce; font-size: 12px; margin-top: 6px;")
    } else {
      n <- length(unique(rv_cohort_members()$subject_id))
      shiny::div(
        class = "alert alert-success",
        style = "margin-top: 8px; padding: 4px 8px; font-size: 12px;",
        sprintf("✓ %d patients loaded", n)
      )
    }
  })

  # ── Load cohort pipeline ─────────────────────────────────────────────────
  shiny::observeEvent(input$btn_load_cohort, {
    shiny::withProgress(message = "Loading cohort...", value = 0, {

      shiny::setProgress(0.05, detail = "Building connector...")
      connector <- .build_connector(input)
      if (is.null(connector)) return()

      shiny::setProgress(0.1, detail = "Extracting cohort members...")
      cohort_members <- tryCatch(
        extract_cohort_members(connector),
        error = function(e) {
          shiny::showNotification(paste("Error:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      if (is.null(cohort_members) || nrow(cohort_members) == 0L) {
        shiny::showNotification("No cohort members found.", type = "warning")
        return()
      }

      shiny::setProgress(0.20, detail = "Extracting OMOP domains...")
      domain_data <- tryCatch(
        extract_omop_domains(connector, subject_ids = cohort_members$subject_id),
        error = function(e) {
          shiny::showNotification(paste("Domain extract error:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      if (is.null(domain_data)) return()

      shiny::setProgress(0.40, detail = "Engineering features...")
      time_windows <- define_time_windows()
      domain_act   <- build_domain_activity(cohort_members, domain_data, time_windows)

      shiny::setProgress(0.55, detail = "Building feature matrix...")
      feat_mat <- tryCatch(
        build_feature_matrix(cohort_members, domain_data, time_windows),
        error = function(e) NULL
      )

      shiny::setProgress(0.65, detail = "Running ML pipeline...")
      ml_res <- NULL
      rank_df <- NULL
      if (!is.null(feat_mat) && ncol(feat_mat$wide) > 1L) {
        ml_res <- tryCatch(
          run_full_ml_pipeline(feat_mat$wide),
          error = function(e) {
            shiny::showNotification(paste("ML warning:", conditionMessage(e)), type = "warning")
            NULL
          }
        )
        if (!is.null(ml_res)) {
          rank_df <- tryCatch(
            rank_patients(ml_res, domain_act, cohort_members),
            error = function(e) NULL
          )
        }
      }

      shiny::setProgress(0.85, detail = "Building quilt...")
      quilt_base <- build_quilt_data(domain_act, rank_df)

      # Store everything
      rv_cohort_members(cohort_members)
      rv_domain_data(domain_data)
      rv_feature_matrix(feat_mat)
      rv_ml_results(ml_res)
      rv_rank_df(rank_df)
      rv_quilt_base(quilt_base)
      rv_time_win_labels(sort(unique(quilt_base$window_label)))

      shiny::setProgress(1.0, detail = "Done.")
      shiny::showNotification(
        sprintf("Cohort loaded: %d patients.", nrow(cohort_members)),
        type = "message"
      )
    })
  })

  # ── Tab switching when "View Trajectory" is clicked in overview ─────────
  shiny::observeEvent(rv_selected_patient(), {
    pid <- rv_selected_patient()
    if (!is.null(pid) && !is.null(input$main_tabs)) {
      # Don't auto-switch tabs; user controls navigation.
      # But if they're on trajectory tab already, the module reacts automatically.
    }
  })

  # ── Module wiring ─────────────────────────────────────────────────────────
  cohort_overviewServer(
    "overview",
    quilt_base         = rv_quilt_base,
    selected_patient   = rv_selected_patient,
    time_window_labels = shiny::isolate(rv_time_win_labels()) %||% character(0)
  )

  # Re-wire time_window_labels reactively after load
  shiny::observe({
    shiny::req(rv_quilt_base())
    labels <- sort(unique(rv_quilt_base()$window_label))
    rv_time_win_labels(labels)
  })

  anomaly_explorerServer(
    "anomaly",
    ml_results       = reactive(rv_ml_results()),
    selected_patient = rv_selected_patient
  )

  patient_selectorServer(
    "selector",
    rank_df          = reactive(rv_rank_df()),
    selected_patient = rv_selected_patient
  )

  trajectory_viewerServer(
    "trajectory",
    selected_patient = rv_selected_patient,
    domain_data      = reactive(rv_domain_data()),
    cohort_members   = reactive(rv_cohort_members())
  )

  hypothesis_panelServer(
    "hypotheses",
    feature_matrix = reactive(rv_feature_matrix()),
    ml_results     = reactive(rv_ml_results())
  )
}

# ---------------------------------------------------------------------------
# Internal: build connector from sidebar inputs
# ---------------------------------------------------------------------------

.build_connector <- function(input) {
  mode <- input$data_mode %||% "demo"

  if (mode == "demo") {
    return(.make_demo_connector())
  }

  if (mode == "upload") {
    fp <- input$upload_rds$datapath
    if (is.null(fp)) {
      shiny::showNotification("Please upload an .rds file.", type = "warning")
      return(NULL)
    }
    cohort_data <- tryCatch(readRDS(fp), error = function(e) NULL)
    if (is.null(cohort_data)) {
      shiny::showNotification("Could not read .rds file.", type = "error")
      return(NULL)
    }
    return(create_cohort_df_connector(cohort_data))
  }

  # OMOP mode — requires DatabaseConnector
  if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
    shiny::showNotification("DatabaseConnector not installed.", type = "error")
    return(NULL)
  }
  cdm_schema <- input$cdm_schema
  if (!nzchar(cdm_schema %||% "")) {
    shiny::showNotification("CDM schema is required.", type = "warning")
    return(NULL)
  }
  cd <- tryCatch(
    DatabaseConnector::createConnectionDetails(
      dbms     = Sys.getenv("DBMS", "postgresql"),
      server   = Sys.getenv("DB_SERVER"),
      user     = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASSWORD"),
      port     = as.integer(Sys.getenv("DB_PORT", "5432"))
    ),
    error = function(e) NULL
  )
  if (is.null(cd)) {
    shiny::showNotification("Could not create connection details. Check environment variables.", type = "error")
    return(NULL)
  }
  create_cohort_omop_connector(
    connectionDetails = cd,
    cdm_schema        = cdm_schema,
    cohort_schema     = input$cohort_schema %||% cdm_schema,
    vocab_schema      = input$vocab_schema  %||% cdm_schema
  )
}

# ---------------------------------------------------------------------------
# Demo connector: 50 synthetic patients
# ---------------------------------------------------------------------------

.make_demo_connector <- function() {
  set.seed(99L)
  n <- 50L

  cohort_members <- tibble::tibble(
    cohort_definition_id = 1L,
    subject_id           = seq_len(n),
    cohort_start_date    = as.Date("2018-01-01") + sample(0:365, n, replace = TRUE),
    cohort_end_date      = as.Date("2020-12-31")
  )

  .make_events <- function(domain, concept_ids, concept_names, date_offset_range) {
    purrr::map_dfr(seq_len(n), function(pid) {
      idx_date <- cohort_members$cohort_start_date[pid]
      n_events <- sample(0:8, 1)
      if (n_events == 0L) return(tibble::tibble())
      ci <- sample(concept_ids, n_events, replace = TRUE)
      tibble::tibble(
        person_id     = pid,
        concept_id    = ci,
        concept_name  = concept_names[match(ci, concept_ids)],
        event_date    = idx_date + sample(date_offset_range, n_events, replace = TRUE)
      )
    })
  }

  cond_ids   <- c(201820L, 316866L, 4027663L, 4116491L, 73553L)
  cond_names <- c("Type 2 diabetes","Hypertension","Rheumatoid arthritis","Myositis","Osteoarthritis")
  drug_ids   <- c(1503297L, 1124300L, 19016586L, 1777087L, 40163554L)
  drug_names <- c("Methotrexate","Prednisone","Hydroxychloroquine","Mycophenolate","Rituximab")
  proc_ids   <- c(4019964L, 4298431L)
  proc_names <- c("Muscle biopsy","Electromyography")

  cond_df <- .make_events("condition", cond_ids, cond_names, -365:365) |>
    dplyr::rename(condition_concept_id  = concept_id,
                  condition_name        = concept_name,
                  condition_start_date  = event_date) |>
    dplyr::mutate(condition_occurrence_id = dplyr::row_number(),
                  condition_end_date    = condition_start_date + 30L,
                  condition_source_value = as.character(condition_concept_id))

  drug_df <- .make_events("drug", drug_ids, drug_names, -180:365) |>
    dplyr::rename(drug_concept_id           = concept_id,
                  drug_name                 = concept_name,
                  drug_exposure_start_date  = event_date) |>
    dplyr::mutate(drug_exposure_id          = dplyr::row_number(),
                  drug_exposure_end_date    = drug_exposure_start_date + 90L,
                  drug_source_value         = as.character(drug_concept_id))

  proc_df <- .make_events("procedure", proc_ids, proc_names, -365:30) |>
    dplyr::rename(procedure_concept_id  = concept_id,
                  procedure_name        = concept_name,
                  procedure_date        = event_date) |>
    dplyr::mutate(procedure_occurrence_id = dplyr::row_number(),
                  procedure_source_value  = as.character(procedure_concept_id))

  meas_ids   <- c(3013721L, 3016723L)
  meas_names <- c("CK (creatine kinase)","Aldolase")
  meas_df <- .make_events("measurement", meas_ids, meas_names, -365:365) |>
    dplyr::rename(measurement_concept_id  = concept_id,
                  measurement_name        = concept_name,
                  measurement_date        = event_date) |>
    dplyr::mutate(measurement_id           = dplyr::row_number(),
                  value_as_number          = runif(dplyr::n(), 50, 2000),
                  unit_name                = "U/L",
                  measurement_source_value = as.character(measurement_concept_id))

  visit_df <- purrr::map_dfr(seq_len(n), function(pid) {
    idx_date <- cohort_members$cohort_start_date[pid]
    n_v      <- sample(2:6, 1)
    tibble::tibble(
      person_id           = pid,
      visit_occurrence_id = pid * 100L + seq_len(n_v),
      visit_start_date    = idx_date + sample(-365:365, n_v, replace = TRUE),
      visit_end_date      = idx_date + sample(-365:365, n_v, replace = TRUE),
      visit_concept_id    = 9202L,
      visit_type          = "Outpatient Visit",
      visit_source_value  = "OP"
    )
  })

  create_cohort_df_connector(list(
    cohort      = cohort_members,
    person      = .empty_cohort_domain("person"),
    condition   = cond_df,
    drug        = drug_df,
    procedure   = proc_df,
    measurement = meas_df,
    observation = .empty_cohort_domain("observation"),
    visit       = visit_df,
    death       = .empty_cohort_domain("death")
  ))
}
