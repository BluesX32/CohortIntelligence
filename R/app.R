# app.R
# Entry point for launching the CohortIntelligence Shiny dashboard.

#' Launch the CohortIntelligence Shiny dashboard
#'
#' @param ... Additional arguments forwarded to [shiny::runApp()].
#' @export
launch_cohort_intelligence <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required to launch the dashboard.")
  }
  app_dir <- system.file("shiny", package = "CohortIntelligence")
  if (!nzchar(app_dir)) {
    # Development fallback
    app_dir <- file.path(find.package("CohortIntelligence"), "inst", "shiny")
    if (!dir.exists(app_dir)) {
      app_dir <- file.path(getwd(), "inst", "shiny")
    }
  }
  shiny::runApp(app_dir, ...)
}
