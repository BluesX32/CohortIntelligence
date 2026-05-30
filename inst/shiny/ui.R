# ui.R — CohortIntelligence Shiny dashboard

pkgs <- c("shiny","shinydashboard","shinyWidgets","plotly","DT")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop(sprintf("Package '%s' required.", p))

for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f)

shinydashboard::dashboardPage(
  skin = "blue",

  # ── Header ────────────────────────────────────────────────────────────────
  shinydashboard::dashboardHeader(
    title = "CohortIntelligence"
  ),

  # ── Sidebar ───────────────────────────────────────────────────────────────
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "main_tabs",
      shinydashboard::menuItem("Cohort Overview",    tabName = "overview",   icon = shiny::icon("th")),
      shinydashboard::menuItem("Anomaly Explorer",   tabName = "anomaly",    icon = shiny::icon("search")),
      shinydashboard::menuItem("Patient Selector",   tabName = "selector",   icon = shiny::icon("list-ol")),
      shinydashboard::menuItem("Trajectory Viewer",  tabName = "trajectory", icon = shiny::icon("chart-line")),
      shinydashboard::menuItem("Hypothesis Panel",   tabName = "hypotheses", icon = shiny::icon("lightbulb"))
    ),
    shiny::hr(),
    shiny::div(
      style = "padding: 10px 15px;",
      shiny::h5("Data Source", style = "color: #b8c7ce;"),
      shiny::selectInput("data_mode", NULL,
                         choices  = c("Demo (synthetic)" = "demo",
                                      "Upload RDS file"   = "upload",
                                      "OMOP CDM database" = "omop"),
                         selected = "demo"),
      shiny::conditionalPanel(
        "input.data_mode == 'upload'",
        shiny::fileInput("upload_rds", "Upload cohort .rds", accept = ".rds")
      ),
      shiny::conditionalPanel(
        "input.data_mode == 'omop'",
        shiny::textInput("cdm_schema",    "CDM Schema",    placeholder = "cdm"),
        shiny::textInput("cohort_schema", "Cohort Schema", placeholder = "results"),
        shiny::textInput("vocab_schema",  "Vocab Schema",  placeholder = "vocab")
      ),
      shiny::actionButton("btn_load_cohort", "Load Cohort",
                          icon  = shiny::icon("play"),
                          class = "btn-primary btn-block"),
      shiny::uiOutput("load_status")
    )
  ),

  # ── Body ──────────────────────────────────────────────────────────────────
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinydashboard::tabItems(
      # Cohort Overview (quilt)
      shinydashboard::tabItem(
        tabName = "overview",
        shiny::h3("Cohort Overview — Quilt Plot"),
        cohort_overviewUI("overview")
      ),
      # Anomaly Explorer
      shinydashboard::tabItem(
        tabName = "anomaly",
        shiny::h3("Anomaly Explorer"),
        anomaly_explorerUI("anomaly")
      ),
      # Patient Selector
      shinydashboard::tabItem(
        tabName = "selector",
        shiny::h3("Patient Review Queue"),
        patient_selectorUI("selector")
      ),
      # Trajectory Viewer
      shinydashboard::tabItem(
        tabName = "trajectory",
        shiny::h3("Patient Trajectory Viewer"),
        trajectory_viewerUI("trajectory")
      ),
      # Hypothesis Panel
      shinydashboard::tabItem(
        tabName = "hypotheses",
        shiny::h3("Hypothesis Generation"),
        hypothesis_panelUI("hypotheses")
      )
    )
  )
)
