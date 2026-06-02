# ui.R -- CohortIntelligence Shiny dashboard (Wave 4 -- 5-tab workflow layout)

pkgs <- c("shiny", "shinydashboard", "shinyWidgets", "plotly", "DT")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) stop(sprintf("Package '%s' is required.", p))
}
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f)

.env        <- CohortIntelligence:::.cohort_intel_env
.has_params <- !is.null(.env$connection)
.source_label <- if (.has_params) {
  if (!is.null(.env$json_path) && nzchar(.env$json_path)) {
    paste0("JSON: ", basename(.env$json_path))
  } else {
    paste0("Table: ", .env$cohort_table %||% "cohort",
           " (ID ", .env$cohort_definition_id %||% 1L, ")")
  }
} else NULL

shinydashboard::dashboardPage(
  skin = "blue",
  title = "CohortIntelligence",

  # ── Header ────────────────────────────────────────────────────────────────
  shinydashboard::dashboardHeader(
    title = shiny::span("CohortIntelligence",
                         style = "font-size:1em; font-weight:700;")
  ),

  # ── Sidebar ───────────────────────────────────────────────────────────────
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "main_tabs",
      shinydashboard::menuItem(
        "Cohort Overview",
        tabName = "overview",
        icon    = shiny::icon("chart-bar")
      ),
      shinydashboard::menuItem(
        "Cluster & Anomaly",
        tabName = "anomaly",
        icon    = shiny::icon("search")
      ),
      shinydashboard::menuItem(
        "Review Queue",
        tabName = "review",
        icon    = shiny::icon("list-check")
      ),
      shinydashboard::menuItem(
        "Trajectory Review",
        tabName = "trajectory",
        icon    = shiny::icon("chart-line")
      ),
      shinydashboard::menuItem(
        "Hypothesis & Report",
        tabName = "hypothesis_report",
        icon    = shiny::icon("flask")
      )
    ),
    shiny::hr(),
    shiny::div(
      style = "padding: 10px 15px;",

      # Data source indicator
      if (.has_params) {
        shiny::tagList(
          shiny::h5("Cohort Source", style = "color: #b8c7ce;"),
          shiny::p(.source_label,
                   style = paste0("color: #ecf0f1; font-size: 11px;",
                                  " margin-bottom: 8px;"))
        )
      } else {
        shiny::tagList(
          shiny::h5("Data Source", style = "color: #b8c7ce;"),
          shiny::selectInput(
            "data_mode", NULL,
            choices  = c("Demo (synthetic)" = "demo",
                         "Upload RDS file"  = "upload"),
            selected = "demo"
          ),
          shiny::conditionalPanel(
            "input.data_mode == 'upload'",
            shiny::fileInput("upload_rds", NULL,
                             placeholder = "Select cohort .rds",
                             accept = ".rds")
          )
        )
      },

      # Status indicator
      shiny::uiOutput("load_status"),
      shiny::hr(),

      # Export buttons (shown after load)
      shiny::uiOutput("sidebar_exports"),

      # User manual
      shiny::tags$a(
        href = "manual.html", target = "_blank",
        class = "btn btn-default btn-sm btn-block",
        style = "color: #b8c7ce; margin-top: 4px;",
        shiny::icon("circle-question"), " User Manual"
      )
    )
  ),

  # ── Body ──────────────────────────────────────────────────────────────────
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),

    shinydashboard::tabItems(

      # ── Tab 1: Cohort Overview ─────────────────────────────────────────
      shinydashboard::tabItem(
        tabName = "overview",
        shiny::fluidRow(
          shiny::column(12,
            shiny::h3("Cohort Overview",
                       shiny::tags$small(
                         style = "color:#64748b; font-size:0.6em; margin-left:10px;",
                         "Use these cards to orient before inspecting the quilt plot."
                       ))
          )
        ),
        # Summary cards — rendered by server.R
        shiny::uiOutput("overview_summary_cards"),
        cohort_overviewUI("overview"),
        shiny::hr(),
        demographicsUI("demographics")
      ),

      # ── Tab 2: Cluster & Anomaly ───────────────────────────────────────
      shinydashboard::tabItem(
        tabName = "anomaly",
        shiny::fluidRow(
          shiny::column(12,
            shiny::h3("Cluster & Anomaly Explorer",
                       shiny::tags$small(
                         style = "color:#64748b; font-size:0.6em; margin-left:10px;",
                         "UMAP projection, cluster profiles, and temporal flags"
                       ))
          )
        ),
        # UMAP + anomaly
        anomaly_explorerUI("anomaly"),
        shiny::hr(),
        # Cluster profiles
        shiny::h4("Cluster Profiles -- Clinical Characterisation",
                   style = "margin-top:12px;"),
        shiny::p(
          style = "color:#64748b; font-size:0.88em; margin-bottom:12px;",
          "Cluster labels are descriptive hypotheses only. Requires clinical review."
        ),
        cluster_profileUI("clusters"),
        shiny::hr(),
        # Temporal flags
        shiny::h4("Temporal Rule Flags",
                   style = "margin-top:12px;"),
        shiny::p(
          style = "color:#64748b; font-size:0.88em; margin-bottom:12px;",
          "Rule-based review triggers. Not clinical conclusions."
        ),
        temporal_flagsUI("temporal_flags")
      ),

      # ── Tab 3: Review Queue ────────────────────────────────────────────
      shinydashboard::tabItem(
        tabName = "review",
        shiny::fluidRow(
          shiny::column(12,
            shiny::h3("Review Queue",
                       shiny::tags$small(
                         style = "color:#64748b; font-size:0.6em; margin-left:10px;",
                         "Eight guided patient sets -- select to begin review"
                       ))
          )
        ),
        review_setsUI("review_sets"),
        shiny::hr(),
        shiny::h4("Signal Explanation -- Why This Patient",
                   style = "margin-top:16px;"),
        signal_explanationUI("signal_explanation")
      ),

      # ── Tab 4: Trajectory Review ───────────────────────────────────────
      shinydashboard::tabItem(
        tabName = "trajectory",
        shiny::fluidRow(
          shiny::column(12,
            shiny::h3("Trajectory Review",
                       shiny::tags$small(
                         style = "color:#64748b; font-size:0.6em; margin-left:10px;",
                         "Per-patient clinical timeline with priority explanation"
                       ))
          )
        ),
        shiny::fluidRow(
          shiny::column(8, trajectory_viewerUI("trajectory")),
          shiny::column(4,
            shiny::div(
              style = "margin-top:0;",
              shiny::h4("Why This Patient",
                         style = "color:#0f3460; margin-bottom:6px;"),
              signal_explanationUI("signal_explanation_traj")
            )
          )
        )
      ),

      # ── Tab 5: Hypothesis & Report ─────────────────────────────────────
      shinydashboard::tabItem(
        tabName = "hypothesis_report",
        shiny::fluidRow(
          shiny::column(12,
            shiny::h3("Hypothesis Generation & Report Export",
                       shiny::tags$small(
                         style = "color:#64748b; font-size:0.6em; margin-left:10px;",
                         "Candidate research hypotheses -- all require clinical validation"
                       ))
          )
        ),
        hypothesis_panelUI("hypotheses"),
        shiny::hr(),
        shiny::h4("Export Clinician Review Packet",
                   style = "margin-top:16px;"),
        shiny::p(
          style = "color:#64748b; font-size:0.88em;",
          "Generates a self-contained HTML report for clinician meetings. ",
          "Includes cohort overview, cluster profiles, review sets, ",
          "temporal flags, and top hypotheses."
        ),
        shiny::uiOutput("clinician_packet_ui")
      )
    )
  )
)
