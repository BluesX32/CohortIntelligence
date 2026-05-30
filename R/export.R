# export.R
# Data and plot export utilities for CohortIntelligence.

#' Export cohort intelligence results to disk
#'
#' Writes analysis outputs (feature matrices, ML results, rankings,
#' hypotheses) to CSV, RDS, and/or JSON formats.
#'
#' @param results Named list with any subset of: `$feature_matrix`
#'   (from [build_feature_matrix()]), `$ml_results` (from
#'   [run_full_ml_pipeline()]), `$rank` (from [rank_patients()]),
#'   `$hypotheses` (from [generate_hypotheses()]).
#' @param path Character. Directory to write files to.
#' @param formats Character vector. Any combination of `"rds"`, `"csv"`,
#'   `"json"`.
#' @param overwrite Logical. Overwrite existing files. Default `FALSE`.
#'
#' @return Invisibly returns a character vector of paths written.
#' @export
export_cohort_results <- function(results,
                                   path,
                                   formats   = c("rds","csv"),
                                   overwrite = FALSE) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  written <- character(0)

  write_if <- function(obj, name) {
    if ("csv" %in% formats && is.data.frame(obj)) {
      fp <- file.path(path, paste0(name, ".csv"))
      if (!file.exists(fp) || overwrite) {
        utils::write.csv(obj, fp, row.names = FALSE)
        written <<- c(written, fp)
      }
    }
    if ("rds" %in% formats) {
      fp <- file.path(path, paste0(name, ".rds"))
      if (!file.exists(fp) || overwrite) {
        saveRDS(obj, fp)
        written <<- c(written, fp)
      }
    }
    if ("json" %in% formats && requireNamespace("jsonlite", quietly = TRUE)) {
      fp <- file.path(path, paste0(name, ".json"))
      if (!file.exists(fp) || overwrite) {
        jsonlite::write_json(obj, fp, pretty = TRUE, auto_unbox = TRUE)
        written <<- c(written, fp)
      }
    }
  }

  if (!is.null(results$feature_matrix)) {
    if (!is.null(results$feature_matrix$long))
      write_if(results$feature_matrix$long, "cohort_features_long")
    if (!is.null(results$feature_matrix$wide))
      write_if(results$feature_matrix$wide, "cohort_features_wide")
  }

  if (!is.null(results$ml_results)) {
    if (!is.null(results$ml_results$merged))
      write_if(results$ml_results$merged, "ml_results")
  }

  if (!is.null(results$rank)) {
    write_if(results$rank, "patient_rankings")
  }

  if (!is.null(results$hypotheses)) {
    write_if(results$hypotheses, "hypotheses")
  }

  message(sprintf("Wrote %d file(s) to %s", length(written), path))
  invisible(written)
}

#' Export the quilt plot to a static image file
#'
#' Uses ggplot2 + svglite (SVG) or grDevices (PNG/PDF) -- no kaleido required.
#'
#' @param quilt_data tibble from [build_quilt_data()].
#' @param path Character. Output file path (including extension).
#' @param format `"png"`, `"svg"`, or `"pdf"`.
#' @param width Numeric. Width in inches. Default 12.
#' @param height Numeric. Height in inches. Default 8.
#' @param dpi Numeric. Resolution (PNG only). Default 150.
#'
#' @return Invisibly returns `path`.
#' @export
export_quilt_plot <- function(quilt_data,
                               path,
                               format = c("png","svg","pdf"),
                               width  = 12,
                               height = 8,
                               dpi    = 150) {
  format <- match.arg(format)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package 'ggplot2' is required for export_quilt_plot().")
  }

  p <- .render_quilt_ggplot(quilt_data)

  if (format == "svg") {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      rlang::abort("Package 'svglite' is required for SVG export.")
    }
    svglite::svglite(path, width = width, height = height)
    print(p)
    grDevices::dev.off()
  } else {
    ggplot2::ggsave(path, p,
                    width  = width,
                    height = height,
                    dpi    = dpi,
                    device = format)
  }

  message("Quilt plot saved to: ", path)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Internal: static ggplot2 quilt renderer (also used by export)
# ---------------------------------------------------------------------------

#' Build a static ggplot2 quilt (internal)
#' @noRd
.render_quilt_ggplot <- function(quilt_df) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package 'ggplot2' required.")
  }

  ggplot2::ggplot(quilt_df, ggplot2::aes(
    x    = window_label,
    y    = reorder(display_label, -patient_row),
    fill = fill_value
  )) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.1) +
    ggplot2::facet_grid(cols = ggplot2::vars(domain),
                        scales = "free_x", space = "free_x") +
    ggplot2::scale_fill_gradient(
      low  = "white",
      high = "steelblue",
      name = "Activity"
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      axis.text.y      = ggplot2::element_text(size = 5),
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 0, size = 7),
      strip.background = ggplot2::element_rect(fill = "#E0E0E0", colour = NA),
      strip.text       = ggplot2::element_text(face = "bold", size = 9),
      panel.grid       = ggplot2::element_blank(),
      legend.position  = "right"
    ) +
    ggplot2::labs(
      x     = "Time window",
      y     = "Patient",
      title = "Cohort Quilt -- Domain Activity by Patient and Time Window"
    )
}
