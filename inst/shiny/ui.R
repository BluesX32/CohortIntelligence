# ui.R -- CohortIntelligence Shiny dashboard

pkgs <- c("shiny", "shinydashboard", "shinyWidgets", "plotly", "DT")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required.", p))
  }
}

for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f)

# Determine at UI-build time whether a live connection was passed.
.env        <- CohortIntelligence:::.cohort_intel_env
.has_params <- !is.null(.env$connection)
.source_label <- if (.has_params) {
  if (!is.null(.env$json_path) && nzchar(.env$json_path)) {
    paste0("JSON: ", basename(.env$json_path))
  } else {
    paste0("Table: ", .env$cohort_table %||% "cohort",
           " (ID ", .env$cohort_definition_id %||% 1L, ")")
  }
} else {
  NULL
}

shinydashboard::dashboardPage(
  skin = "blue",

  shinydashboard::dashboardHeader(title = "CohortIntelligence"),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "main_tabs",
      shinydashboard::menuItem("Cohort Overview",   tabName = "overview",
                               icon = shiny::icon("th")),
      shinydashboard::menuItem("Anomaly Explorer",  tabName = "anomaly",
                               icon = shiny::icon("search")),
      shinydashboard::menuItem("Patient Selector",  tabName = "selector",
                               icon = shiny::icon("list-ol")),
      shinydashboard::menuItem("Trajectory Viewer", tabName = "trajectory",
                               icon = shiny::icon("chart-line")),
      shinydashboard::menuItem("Hypothesis Panel",  tabName = "hypotheses",
                               icon = shiny::icon("lightbulb")),
      shinydashboard::menuItem("Demographics",      tabName = "demographics",
                               icon = shiny::icon("users")),
      shinydashboard::menuItem("Cluster Profiles",  tabName = "clusters",
                               icon = shiny::icon("layer-group"))
    ),
    shiny::hr(),
    shiny::div(
      style = "padding: 10px 15px;",

      if (.has_params) {
        # Live OMOP or JSON mode -- auto-loads on startup, no user action needed
        shiny::tagList(
          shiny::h5("Cohort Source", style = "color: #b8c7ce;"),
          shiny::p(.source_label,
                   style = paste0("color: #ecf0f1; font-size: 11px;",
                                  " margin-bottom: 8px;"))
        )
      } else {
        # No launch params -- auto-loads demo; or upload an RDS for custom data
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
            shiny::fileInput(
              "upload_rds",
              label = NULL,
              placeholder = "Select cohort .rds to load",
              accept = ".rds"
            )
          )
        )
      },

      # Status indicator (replaces the Load Cohort button)
      shiny::uiOutput("load_status"),
      shiny::hr(),

      # Export report (only shown when cohort is loaded)
      shiny::uiOutput("btn_report_ui"),

      # User manual link
      shiny::tags$a(
        href   = "manual.html",
        target = "_blank",
        class  = "btn btn-default btn-sm btn-block",
        style  = "color: #b8c7ce;",
        shiny::icon("circle-question"), " User Manual"
      )
    )
  ),

  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "overview",
        shiny::h3("Cohort Overview -- Quilt Plot"),
        cohort_overviewUI("overview")
      ),
      shinydashboard::tabItem(
        tabName = "anomaly",
        shiny::h3("Anomaly Explorer"),
        anomaly_explorerUI("anomaly")
      ),
      shinydashboard::tabItem(
        tabName = "selector",
        shiny::h3("Patient Review Queue"),
        patient_selectorUI("selector")
      ),
      shinydashboard::tabItem(
        tabName = "trajectory",
        shiny::h3("Patient Trajectory Viewer"),
        trajectory_viewerUI("trajectory")
      ),
      shinydashboard::tabItem(
        tabName = "hypotheses",
        shiny::h3("Hypothesis Generation"),
        hypothesis_panelUI("hypotheses")
      ),
      shinydashboard::tabItem(
        tabName = "demographics",
        shiny::h3("Cohort Demographics & Data Density"),
        demographicsUI("demographics")
      ),
      shinydashboard::tabItem(
        tabName = "clusters",
        shiny::h3("Cluster Profiles -- Clinical Characterisation"),
        cluster_profileUI("clusters")
      )
    )
  )
)
