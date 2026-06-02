# global.R -- sourced automatically by Shiny before ui.R and server.R.
# Defines shared utilities available to all modules without extra sourcing.

# In development (devtools::load_all), the package is already in the
# search path -- calling library() here would reload the stale installed
# version and overwrite every function from load_all().
# Only load from the installed package when NOT in development mode.
if (!isNamespaceLoaded("CohortIntelligence")) {
  library(CohortIntelligence)
}

# Returns an empty plotly figure with a centred message.
# Avoids "No trace type specified" warnings from bare plot_ly().
empty_plotly <- function(msg = "No data available.") {
  plotly::plot_ly(
    x = 0.5, y = 0.5,
    type = "scatter", mode = "text",
    text = msg,
    textfont = list(size = 14, color = "#999999")
  ) |>
    plotly::layout(
      xaxis = list(visible = FALSE, range = c(0, 1)),
      yaxis = list(visible = FALSE, range = c(0, 1)),
      paper_bgcolor = "#FAFAFA",
      plot_bgcolor  = "#FAFAFA"
    ) |>
    plotly::config(displayModeBar = FALSE)
}
