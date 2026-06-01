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

# ---------------------------------------------------------------------------
# HTML cohort report
# ---------------------------------------------------------------------------

#' Export a self-contained HTML summary report for the cohort
#'
#' Generates a single HTML file with embedded CSS and base64-encoded images.
#' No rmarkdown or knitr dependency - pure `htmltools`.
#'
#' @param results Named list with any subset of: `$cohort_members`,
#'   `$rank_df`, `$domain_data`, `$person_data`, `$ml_results`,
#'   `$hypotheses`, `$quilt_base`. Missing slots are silently skipped.
#' @param path Output file path, e.g. `"cohort_report.html"`.
#' @param cohort_name Character(1). Title shown at the top. Default
#'   `"Cohort"`.
#'
#' @return Invisibly returns `path`.
#' @export
export_cohort_report <- function(results,
                                  path,
                                  cohort_name = "Cohort") {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    rlang::abort("Package 'htmltools' is required for export_cohort_report().")
  }

  cm     <- results$cohort_members
  rd     <- results$rank_df
  pd     <- results$person_data
  dd     <- results$domain_data
  ml     <- results$ml_results
  hyp    <- results$hypotheses
  qb     <- results$quilt_base
  n_pat  <- if (!is.null(cm)) nrow(cm) else 0L

  # -- Summary stats ------------------------------------------------------
  summ <- if (!is.null(cm) && !is.null(pd)) {
    tryCatch(build_cohort_summary(cm, pd), error = function(e) NULL)
  } else NULL

  # -- Cluster profiles --------------------------------------------------
  cluster_prof <- if (!is.null(rd) && !is.null(dd) && !is.null(cm)) {
    tryCatch(
      build_cluster_profiles(rd, dd, cm, pd, top_n = 5L,
                              domains = c("condition","drug")),
      error = function(e) NULL
    )
  } else NULL

  # -- Quilt PNG (base64) -------------------------------------------------
  quilt_img_tag <- NULL
  if (!is.null(qb) && nrow(qb) > 0L &&
      requireNamespace("ggplot2", quietly = TRUE)) {
    tmp <- tempfile(fileext = ".png")
    tryCatch({
      p <- .render_quilt_ggplot(qb)
      ggplot2::ggsave(tmp, p, width = 14, height = min(9, 2 + n_pat / 30),
                      dpi = 120, device = "png")
      img_b64 <- base64enc::base64encode(tmp)
      quilt_img_tag <- htmltools::tags$img(
        src   = paste0("data:image/png;base64,", img_b64),
        style = "max-width:100%; border:1px solid #e2e8f0; border-radius:6px;"
      )
      unlink(tmp)
    }, error = function(e) NULL)
  }

  # -- CSS ---------------------------------------------------------------
  css <- htmltools::tags$style(htmltools::HTML("
    body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif;
         max-width:1100px;margin:0 auto;padding:24px 32px 64px;color:#1a1a2e;
         font-size:14px;line-height:1.65;}
    h1{font-size:1.9em;font-weight:800;color:#0f3460;margin-bottom:4px;}
    h2{font-size:1.3em;font-weight:700;color:#0f3460;margin:36px 0 12px;
       padding-bottom:6px;border-bottom:2px solid #e94560;}
    h3{font-size:1em;font-weight:700;color:#334155;margin:20px 0 8px;}
    p{margin-bottom:12px;}
    table{width:100%;border-collapse:collapse;margin:12px 0 18px;font-size:.88em;}
    th{background:#0f3460;color:#e2e8f0;padding:8px 12px;text-align:left;}
    td{border:1px solid #e2e8f0;padding:7px 12px;vertical-align:top;}
    tr:nth-child(even) td{background:#f8fafc;}
    .stat-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(160px,1fr));gap:12px;margin:12px 0 20px;}
    .stat-card{background:#f8fafc;border:1px solid #e2e8f0;border-radius:8px;padding:12px 16px;text-align:center;}
    .stat-card .val{font-size:1.6em;font-weight:800;color:#0f3460;}
    .stat-card .lbl{font-size:.78em;color:#64748b;margin-top:2px;}
    footer{font-size:.78em;color:#94a3b8;margin-top:48px;border-top:1px solid #e2e8f0;padding-top:12px;}
  "))

  # -- Build HTML ----------------------------------------------------------
  sections <- list()

  # Header
  sections[["header"]] <- htmltools::tagList(
    htmltools::tags$h1(cohort_name),
    htmltools::tags$p(
      style = "color:#64748b;",
      "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"),
      htmltools::HTML("&nbsp;&nbsp;|&nbsp;&nbsp;"),
      sprintf("%d patients", n_pat),
      htmltools::HTML("&nbsp;&nbsp;|&nbsp;&nbsp;"),
      "CohortIntelligence v0.1"
    )
  )

  # Summary stats cards
  if (!is.null(summ)) {
    med_fu <- if (!is.na(summ$median_followup))
      paste0(round(summ$median_followup / 30.4375, 1), " mo") else "N/A"
    sections[["demo"]] <- htmltools::tagList(
      htmltools::tags$h2("Demographics"),
      htmltools::tags$div(
        class = "stat-grid",
        .stat_card(n_pat, "Patients"),
        .stat_card(med_fu, "Median follow-up"),
        .stat_card(if (!is.na(summ$median_age))
                     round(summ$median_age, 0) else "N/A", "Median age"),
        .stat_card(if (!is.na(summ$pct_death))
                     paste0(summ$pct_death, "%") else "N/A",
                   "% with death recorded")
      )
    )
  }

  # Quilt
  if (!is.null(quilt_img_tag)) {
    sections[["quilt"]] <- htmltools::tagList(
      htmltools::tags$h2("Cohort Overview - Quilt Plot"),
      quilt_img_tag
    )
  }

  # Cluster summary
  if (!is.null(cluster_prof) && nrow(cluster_prof$summary) > 0L) {
    s <- cluster_prof$summary |>
      dplyr::mutate(
        cluster_label = ifelse(cluster_id <= 0L, "Unassigned",
                                paste0("Cluster ", cluster_id)),
        median_age    = round(median_age, 1),
        pct_female    = paste0(round(pct_female, 0), "%"),
        median_fu     = paste0(round(median_followup_days / 30.4375, 1), " mo"),
        pct_death     = paste0(round(pct_death, 0), "%")
      ) |>
      dplyr::select(cluster_label, n_patients, pct_cohort,
                    median_age, pct_female, median_fu, pct_death)
    sections[["clusters"]] <- htmltools::tagList(
      htmltools::tags$h2("Cluster Summary"),
      .df_to_html_table(s, c("Cluster", "N", "% Cohort", "Median Age",
                              "% Female", "Median F/U", "% Death"))
    )
  }

  # Top hypotheses
  if (!is.null(hyp) && nrow(hyp) > 0L) {
    h_disp <- hyp |>
      dplyr::slice_head(n = 10L) |>
      dplyr::mutate(
        p_value_adjusted = round(p_value_adjusted, 4),
        effect_size      = round(effect_size, 3)
      ) |>
      dplyr::select(cluster_a, cluster_b, domain, concept_name,
                    window_label, effect_size, p_value_adjusted, direction)
    sections[["hyp"]] <- htmltools::tagList(
      htmltools::tags$h2("Top Hypothesis Candidates"),
      htmltools::tags$p(
        style = "color:#64748b; font-size:.88em;",
        "Sorted by adjusted p-value. All findings are exploratory."
      ),
      .df_to_html_table(h_disp, c("Cluster A", "Cluster B", "Domain",
                                    "Concept", "Window", "Effect",
                                    "Adj. p", "Direction"))
    )
  }

  # Footer
  sections[["footer"]] <- htmltools::tags$footer(
    "CohortIntelligence v0.1.0  |  Johns Hopkins University  |  ",
    "OMOP CDM v5.3/5.4  |  For research use only."
  )

  doc <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$title(cohort_name, " - CohortIntelligence Report"),
      css
    ),
    htmltools::tags$body(sections)
  )

  htmltools::save_html(doc, file = path)
  message("Report saved to: ", path)
  invisible(path)
}

.stat_card <- function(val, label) {
  htmltools::tags$div(
    class = "stat-card",
    htmltools::tags$div(class = "val", as.character(val)),
    htmltools::tags$div(class = "lbl", label)
  )
}

.df_to_html_table <- function(df, col_names = names(df)) {
  header_cells <- lapply(col_names, function(n) htmltools::tags$th(n))
  rows <- lapply(seq_len(nrow(df)), function(i) {
    cells <- lapply(seq_len(ncol(df)), function(j) {
      htmltools::tags$td(as.character(df[[j]][[i]] %||% ""))
    })
    htmltools::tags$tr(cells)
  })
  htmltools::tags$table(
    htmltools::tags$thead(htmltools::tags$tr(header_cells)),
    htmltools::tags$tbody(rows)
  )
}
