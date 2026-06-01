# server.R -- CohortIntelligence Shiny dashboard server

for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f)

# ---------------------------------------------------------------------------
# Helpers -- defined before the server function so they are in scope
# ---------------------------------------------------------------------------

.build_connector <- function(input) {
  env <- CohortIntelligence:::.cohort_intel_env

  # Demo mode: no live connection supplied
  if (is.null(env$connection)) {
    if (isTRUE(input$data_mode == "upload")) {
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
    return(.make_demo_connector())
  }

  # Live connection: wrap the already-open connection
  create_cohort_connector(
    connection   = env$connection,
    cdm_schema   = env$cdm_schema,
    vocab_schema = if (!is.null(env$vocab_schema)) env$vocab_schema
                   else env$cdm_schema
  )
}

.run_pipeline <- function(connector, env, rv) {
  shiny::withProgress(message = "Loading cohort...", value = 0, {

    shiny::setProgress(0.10, detail = "Extracting cohort members...")
    use_json       <- !is.null(env$json_path) && nzchar(env$json_path)
    cohort_table   <- if (!is.null(env$cohort_table)) env$cohort_table
                      else "cohort"
    cohort_def_id  <- if (!is.null(env$cohort_definition_id))
                        env$cohort_definition_id else 1L

    cohort_members <- tryCatch(
      if (use_json) {
        fetch_cohort_from_json(connector, env$json_path)
      } else {
        extract_cohort_members(connector,
                               cohort_definition_id = cohort_def_id,
                               cohort_table         = cohort_table)
      },
      error = function(e) {
        msg <- conditionMessage(e)
        message("[CohortIntelligence] Cohort extract error: ", msg)
        shiny::showNotification(paste("Cohort error:", msg),
                                type = "error", duration = NULL)
        NULL
      }
    )
    if (is.null(cohort_members) || nrow(cohort_members) == 0L) {
      message("[CohortIntelligence] No cohort members found.")
      shiny::showNotification(
        "No cohort members found. Run check_cohort_json() in the R console to diagnose.",
        type = "warning", duration = NULL)
      return()
    }

    shiny::setProgress(0.18, detail = "Extracting demographics...")
    person_data <- tryCatch(
      extract_person_demographics(connector,
                                   subject_ids = cohort_members$subject_id),
      error = function(e) NULL
    )

    shiny::setProgress(0.25, detail = "Extracting OMOP domains...")
    domain_data <- tryCatch(
      extract_omop_domains(connector,
                           subject_ids = cohort_members$subject_id),
      error = function(e) {
        msg <- conditionMessage(e)
        message("[CohortIntelligence] Domain extract error: ", msg)
        shiny::showNotification(paste("Domain error:", msg),
                                type = "error", duration = NULL)
        NULL
      }
    )
    if (is.null(domain_data)) return()

    shiny::setProgress(0.45, detail = "Engineering features...")
    time_windows <- define_time_windows()
    domain_act   <- build_domain_activity(cohort_members, domain_data,
                                          time_windows)

    shiny::setProgress(0.60, detail = "Building feature matrix...")
    feat_mat <- tryCatch(
      build_feature_matrix(cohort_members, domain_data, time_windows),
      error = function(e) NULL
    )

    shiny::setProgress(0.70, detail = "Running ML pipeline...")
    ml_res <- NULL
    if (!is.null(feat_mat) && ncol(feat_mat$wide) > 1L) {
      ml_res <- tryCatch(
        run_full_ml_pipeline(feat_mat$wide),
        error = function(e) {
          msg <- conditionMessage(e)
          message("[CohortIntelligence] ML pipeline error: ", msg)
          message("  Tip: install 'uwot' for reliable UMAP: install.packages('uwot')")
          shiny::showNotification(
            paste0("ML pipeline failed: ", msg,
                   " — Anomaly Explorer unavailable. ",
                   "Install uwot: install.packages('uwot')"),
            type = "warning", duration = 10L)
          NULL
        }
      )
    }

    # rank_patients() accepts NULL ml_results (falls back to sparsity-only).
    # Always compute so Patient Selector and priority sorting always work.
    rank_df <- tryCatch(
      rank_patients(ml_res, domain_act, cohort_members),
      error = function(e) {
        message("[CohortIntelligence] Ranking error: ", conditionMessage(e))
        NULL
      }
    )

    shiny::setProgress(0.90, detail = "Building quilt...")
    quilt_base <- build_quilt_data(domain_act, rank_df)

    rv$cohort_members(cohort_members)
    rv$person_data(person_data)
    rv$domain_data(domain_data)
    rv$feature_matrix(feat_mat)
    rv$ml_results(ml_res)
    rv$rank_df(rank_df)
    rv$quilt_base(quilt_base)

    # Auto-select the highest-priority patient so Trajectory Viewer
    # is pre-populated without requiring a manual quilt click
    first_patient <- if (!is.null(rank_df) && nrow(rank_df) > 0L) {
      rank_df$subject_id[which.min(rank_df$rank_position)]
    } else {
      cohort_members$subject_id[[1L]]
    }
    rv$selected_patient(first_patient)

    shiny::setProgress(1.0, detail = "Done.")
    shiny::showNotification(
      sprintf("Cohort loaded: %d patients.", nrow(cohort_members)),
      type = "message"
    )
  })
}

.make_demo_connector <- function() {
  set.seed(99L)
  n <- 50L

  cohort_members <- tibble::tibble(
    cohort_definition_id = 1L,
    subject_id           = seq_len(n),
    cohort_start_date    = as.Date("2018-01-01") +
                             sample(0:365, n, replace = TRUE),
    cohort_end_date      = as.Date("2020-12-31")
  )

  .make_events <- function(concept_ids, concept_names, date_offset_range) {
    purrr::map_dfr(seq_len(n), function(pid) {
      idx_date <- cohort_members$cohort_start_date[pid]
      n_events <- sample(0:8, 1)
      if (n_events == 0L) return(tibble::tibble())
      ci <- sample(concept_ids, n_events, replace = TRUE)
      tibble::tibble(
        person_id    = pid,
        concept_id   = ci,
        concept_name = concept_names[match(ci, concept_ids)],
        event_date   = idx_date +
                         sample(date_offset_range, n_events, replace = TRUE)
      )
    })
  }

  cond_ids   <- c(201820L, 316866L, 4027663L, 4116491L, 73553L)
  cond_names <- c("Diabetes", "Hypertension", "Rheumatoid arthritis",
                  "Myositis", "Osteoarthritis")
  drug_ids   <- c(1503297L, 1124300L, 19016586L, 1777087L, 40163554L)
  drug_names <- c("Methotrexate", "Prednisone", "Hydroxychloroquine",
                  "Mycophenolate", "Rituximab")
  proc_ids   <- c(4019964L, 4298431L)
  proc_names <- c("Muscle biopsy", "Electromyography")
  meas_ids   <- c(3013721L, 3016723L)
  meas_names <- c("CK (creatine kinase)", "Aldolase")

  cond_df <- .make_events(cond_ids, cond_names, -365:365) |>
    dplyr::rename("condition_concept_id" = "concept_id",
                  "condition_name"       = "concept_name",
                  "condition_start_date" = "event_date") |>
    dplyr::mutate(
      condition_occurrence_id = dplyr::row_number(),
      condition_end_date      = .data$condition_start_date + 30L,
      condition_source_value  = as.character(.data$condition_concept_id)
    )

  drug_df <- .make_events(drug_ids, drug_names, -180:365) |>
    dplyr::rename("drug_concept_id"          = "concept_id",
                  "drug_name"                = "concept_name",
                  "drug_exposure_start_date" = "event_date") |>
    dplyr::mutate(
      drug_exposure_id       = dplyr::row_number(),
      drug_exposure_end_date = .data$drug_exposure_start_date + 90L,
      drug_source_value      = as.character(.data$drug_concept_id)
    )

  proc_df <- .make_events(proc_ids, proc_names, -365:30) |>
    dplyr::rename("procedure_concept_id" = "concept_id",
                  "procedure_name"       = "concept_name",
                  "procedure_date"       = "event_date") |>
    dplyr::mutate(
      procedure_occurrence_id = dplyr::row_number(),
      procedure_source_value  = as.character(.data$procedure_concept_id)
    )

  meas_df <- .make_events(meas_ids, meas_names, -365:365) |>
    dplyr::rename("measurement_concept_id" = "concept_id",
                  "measurement_name"       = "concept_name",
                  "measurement_date"       = "event_date") |>
    dplyr::mutate(
      measurement_id           = dplyr::row_number(),
      value_as_number          = stats::runif(dplyr::n(), 50, 2000),
      unit_name                = "U/L",
      measurement_source_value = as.character(.data$measurement_concept_id)
    )

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

  genders <- c("MALE" = 8507L, "FEMALE" = 8532L)
  races   <- c("White" = 8527L, "Black or African American" = 8516L,
                "Asian" = 8515L, "Unknown" = 0L)
  person_df <- tibble::tibble(
    person_id            = seq_len(n),
    birth_year           = sample(1940L:2000L, n, replace = TRUE),
    gender_concept_id    = sample(genders, n, replace = TRUE),
    gender_name          = names(genders)[match(
                             sample(genders, n, replace = TRUE), genders)],
    race_concept_id      = sample(races, n, replace = TRUE,
                                   prob = c(0.6, 0.2, 0.1, 0.1)),
    race_name            = names(races)[match(
                             sample(races, n, replace = TRUE,
                                    prob = c(0.6, 0.2, 0.1, 0.1)), races)],
    ethnicity_concept_id = 0L,
    ethnicity_name       = "Unknown"
  )

  create_cohort_df_connector(list(
    cohort      = cohort_members,
    person      = person_df,
    condition   = cond_df,
    drug        = drug_df,
    procedure   = proc_df,
    measurement = meas_df,
    observation = .empty_cohort_domain("observation"),
    visit       = visit_df,
    death       = tibble::tibble(
      person_id             = sample(seq_len(n), max(1L, as.integer(n * 0.08)),
                                     replace = FALSE),
      death_date            = as.Date("2018-01-01") +
                                sample(0:730, max(1L, as.integer(n * 0.08)),
                                       replace = TRUE),
      death_type_concept_id = 32817L,
      cause_concept_id      = 4306655L,
      cause_name            = "Death"
    )
  ))
}

# ---------------------------------------------------------------------------
# Server function -- must be the last expression in this file
# ---------------------------------------------------------------------------

function(input, output, session) {

  rv <- list(
    selected_patient = shiny::reactiveVal(NULL),
    quilt_base       = shiny::reactiveVal(NULL),
    domain_data      = shiny::reactiveVal(NULL),
    cohort_members   = shiny::reactiveVal(NULL),
    person_data      = shiny::reactiveVal(NULL),
    feature_matrix   = shiny::reactiveVal(NULL),
    ml_results       = shiny::reactiveVal(NULL),
    rank_df          = shiny::reactiveVal(NULL)
  )

  # ── Loading status indicator (replaces the Load Cohort button) ───────────
  output$load_status <- shiny::renderUI({
    if (is.null(rv$quilt_base())) {
      shiny::div(
        style = "margin-top: 8px; text-align: center;",
        shiny::tags$small(
          style = "color: #b8c7ce; font-size: 11px;",
          shiny::icon("spinner"), " Loading cohort..."
        )
      )
    } else {
      n <- length(unique(rv$cohort_members()$subject_id))
      shiny::div(
        class = "alert alert-success",
        style = "margin-top: 8px; padding: 6px 10px; font-size: 12px;",
        shiny::icon("circle-check"),
        sprintf(" %d patients loaded", n)
      )
    }
  })

  # ── Auto-load on session start ────────────────────────────────────────────
  # session$onFlushed(once=TRUE) fires after the first complete reactive flush
  # when the browser is connected. Writing to a reactiveVal here schedules a
  # second flush in which observeEvent runs -- a proper reactive context where
  # withProgress() / showNotification() work correctly.
  #
  # Fires for ALL modes:
  #   Live connection  -- auto-loads immediately (env$connection set)
  #   Demo mode        -- auto-loads demo data  (env$connection NULL, no file)
  #   Upload mode      -- skipped here; triggered by observeEvent(upload_rds)
  rv_do_autoload <- shiny::reactiveVal(FALSE)

  session$onFlushed(function() {
    env        <- CohortIntelligence:::.cohort_intel_env
    is_upload  <- isTRUE(shiny::isolate(input$data_mode) == "upload")
    if (!is_upload) rv_do_autoload(TRUE)
  }, once = TRUE)

  shiny::observeEvent(rv_do_autoload(), {
    shiny::req(rv_do_autoload(), is.null(rv$quilt_base()))
    env       <- CohortIntelligence:::.cohort_intel_env
    connector <- .build_connector(input)
    if (!is.null(connector)) {
      tryCatch(
        .run_pipeline(connector, env, rv),
        error = function(e) {
          msg <- conditionMessage(e)
          message("[CohortIntelligence] Load error: ", msg)
          shiny::showNotification(paste("Load error:", msg), type = "error",
                                  duration = NULL)
        }
      )
    }
  }, ignoreInit = TRUE)

  # ── Upload mode: auto-load as soon as the file is selected ───────────────
  shiny::observeEvent(input$upload_rds, {
    shiny::req(input$upload_rds)
    env       <- CohortIntelligence:::.cohort_intel_env
    connector <- .build_connector(input)
    if (!is.null(connector)) {
      tryCatch(
        .run_pipeline(connector, env, rv),
        error = function(e) {
          msg <- conditionMessage(e)
          message("[CohortIntelligence] Upload load error: ", msg)
          shiny::showNotification(paste("Load error:", msg), type = "error",
                                  duration = NULL)
        }
      )
    }
  })

  cohort_overviewServer(
    "overview",
    quilt_base       = rv$quilt_base,
    selected_patient = rv$selected_patient
  )

  anomaly_explorerServer(
    "anomaly",
    ml_results       = shiny::reactive(rv$ml_results()),
    selected_patient = rv$selected_patient
  )

  patient_selectorServer(
    "selector",
    rank_df          = shiny::reactive(rv$rank_df()),
    selected_patient = rv$selected_patient
  )

  trajectory_viewerServer(
    "trajectory",
    selected_patient = rv$selected_patient,
    domain_data      = shiny::reactive(rv$domain_data()),
    cohort_members   = shiny::reactive(rv$cohort_members())
  )

  demographicsServer(
    "demographics",
    cohort_members = shiny::reactive(rv$cohort_members()),
    domain_data    = shiny::reactive(rv$domain_data()),
    person_data    = shiny::reactive(rv$person_data())
  )

  hypothesis_panelServer(
    "hypotheses",
    feature_matrix = shiny::reactive(rv$feature_matrix()),
    ml_results     = shiny::reactive(rv$ml_results())
  )

  cluster_profileServer(
    "clusters",
    rank_df        = shiny::reactive(rv$rank_df()),
    domain_data    = shiny::reactive(rv$domain_data()),
    cohort_members = shiny::reactive(rv$cohort_members()),
    person_data    = shiny::reactive(rv$person_data())
  )

  # ── Export Report button (visible only after cohort loads) ─────────────
  output$btn_report_ui <- shiny::renderUI({
    shiny::req(rv$quilt_base())
    shiny::downloadButton("btn_report", "Export Report",
                          icon  = shiny::icon("file-export"),
                          class = "btn-default btn-sm btn-block",
                          style = "color: #b8c7ce; margin-bottom: 6px;")
  })

  output$btn_report <- shiny::downloadHandler(
    filename = function() paste0("cohort_report_", Sys.Date(), ".html"),
    content  = function(file) {
      shiny::withProgress(message = "Building report...", value = 0.5, {
        tryCatch(
          export_cohort_report(
            results = list(
              cohort_members = rv$cohort_members(),
              rank_df        = rv$rank_df(),
              domain_data    = rv$domain_data(),
              person_data    = rv$person_data(),
              ml_results     = rv$ml_results(),
              hypotheses     = NULL,
              quilt_base     = rv$quilt_base()
            ),
            path        = file,
            cohort_name = "CohortIntelligence Report"
          ),
          error = function(e) {
            message("[CohortIntelligence] Report export error: ",
                    conditionMessage(e))
          }
        )
        shiny::setProgress(1.0)
      })
    }
  )
}
