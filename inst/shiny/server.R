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
    ml_res  <- NULL
    rank_df <- NULL
    if (!is.null(feat_mat) && ncol(feat_mat$wide) > 1L) {
      ml_res <- tryCatch(
        run_full_ml_pipeline(feat_mat$wide),
        error = function(e) {
          shiny::showNotification(
            paste("ML warning:", conditionMessage(e)), type = "warning")
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

    shiny::setProgress(0.90, detail = "Building quilt...")
    quilt_base <- build_quilt_data(domain_act, rank_df)

    rv$cohort_members(cohort_members)
    rv$domain_data(domain_data)
    rv$feature_matrix(feat_mat)
    rv$ml_results(ml_res)
    rv$rank_df(rank_df)
    rv$quilt_base(quilt_base)

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

# ---------------------------------------------------------------------------
# Server function -- must be the last expression in this file
# ---------------------------------------------------------------------------

function(input, output, session) {

  rv <- list(
    selected_patient = shiny::reactiveVal(NULL),
    quilt_base       = shiny::reactiveVal(NULL),
    domain_data      = shiny::reactiveVal(NULL),
    cohort_members   = shiny::reactiveVal(NULL),
    feature_matrix   = shiny::reactiveVal(NULL),
    ml_results       = shiny::reactiveVal(NULL),
    rank_df          = shiny::reactiveVal(NULL)
  )

  output$load_status <- shiny::renderUI({
    if (is.null(rv$quilt_base())) {
      shiny::p("No cohort loaded.",
               style = "color: #b8c7ce; font-size: 12px; margin-top: 6px;")
    } else {
      n <- length(unique(rv$cohort_members()$subject_id))
      shiny::div(
        class = "alert alert-success",
        style = "margin-top: 8px; padding: 4px 8px; font-size: 12px;",
        sprintf("%d patients loaded", n)
      )
    }
  })

  # Auto-load when a live connection is passed via launch_cohort_intelligence().
  #
  # Pattern: session$onFlushed writes to a reactiveVal; observeEvent reacts.
  # This is required because onFlushed is a plain R callback (NOT a reactive
  # context), so withProgress() / showNotification() cannot be called there.
  # Writing to a reactiveVal from onFlushed schedules a new reactive flush
  # in which the observeEvent runs -- that IS a reactive context.
  rv_do_autoload <- shiny::reactiveVal(FALSE)

  session$onFlushed(function() {
    env <- CohortIntelligence:::.cohort_intel_env
    if (!is.null(env$connection)) rv_do_autoload(TRUE)
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
          message("[CohortIntelligence] Auto-load error: ", msg)
          shiny::showNotification(paste("Load error:", msg), type = "error",
                                  duration = NULL)
        }
      )
    }
  }, ignoreInit = TRUE)

  # Manual load button (demo mode and upload mode)
  shiny::observeEvent(input$btn_load_cohort, {
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

  hypothesis_panelServer(
    "hypotheses",
    feature_matrix = shiny::reactive(rv$feature_matrix()),
    ml_results     = shiny::reactive(rv$ml_results())
  )
}
