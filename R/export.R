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

# ---------------------------------------------------------------------------
# Clinician review packet
# ---------------------------------------------------------------------------

#' Build the in-memory clinician review packet data structure
#'
#' Pre-computes all sections so [export_clinician_review_packet()] can render
#' them without repeated computation.
#'
#' @param cohort_summary List from [build_cohort_summary()].
#' @param cluster_profiles List from [build_cluster_profiles()].
#' @param review_sets tibble from [build_review_sets()].
#' @param temporal_flags tibble from [detect_temporal_flags()].
#' @param hypotheses tibble from [generate_hypotheses()].
#' @param selected_patients Integer vector of patient IDs to include.
#' @param patient_timelines Named list of timelines from [build_patient_timeline()].
#'
#' @return Named list (all sections pre-computed).
#' @export
build_clinician_review_packet <- function(cohort_summary    = NULL,
                                           cluster_profiles  = NULL,
                                           review_sets       = NULL,
                                           temporal_flags    = NULL,
                                           hypotheses        = NULL,
                                           selected_patients = integer(0),
                                           patient_timelines = list()) {
  list(
    generated_at      = Sys.time(),
    cohort_summary    = cohort_summary,
    cluster_profiles  = cluster_profiles,
    review_sets       = review_sets,
    temporal_flags    = temporal_flags,
    hypotheses        = hypotheses,
    selected_patients = as.integer(selected_patients),
    patient_timelines = patient_timelines
  )
}

#' Export a clinician-ready review packet as a self-contained HTML file
#'
#' Generates a comprehensive, export-ready HTML report covering all major
#' dashboard findings. Includes a clear disclaimer that all findings are
#' hypothesis-generating and require clinical validation.
#'
#' @param results Named list from [build_clinician_review_packet()], OR a
#'   generic results list with slots `$cohort_members`, `$rank_df`,
#'   `$domain_data`, `$person_data`, `$ml_results`, `$hypotheses`,
#'   `$quilt_base`, `$temporal_flags`, `$review_sets`, `$cluster_profiles`.
#' @param path Output file path ending in `.html`.
#' @param cohort_name Character(1). Shown as report title.
#' @param n_patients Integer. Max patient summaries to include.
#' @param include Named list of logical flags controlling which sections
#'   appear: `typical`, `outliers`, `sparse`, `temporal_flags`,
#'   `hypotheses`.
#'
#' @return Invisibly returns `path`.
#' @export
export_clinician_review_packet <- function(results,
                                            path,
                                            cohort_name = "Cohort",
                                            n_patients  = 10L,
                                            include     = list(typical = TRUE,
                                                               outliers = TRUE,
                                                               sparse   = TRUE,
                                                               temporal_flags = TRUE,
                                                               hypotheses = TRUE)) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    rlang::abort("Package 'htmltools' is required.")
  }

  # ── Extract sections ───────────────────────────────────────────────────────
  cm   <- results$cohort_members
  qb   <- results$quilt_base
  pd   <- results$person_data
  hyp  <- results$hypotheses
  tf   <- results$temporal_flags
  rs   <- results$review_sets
  cp   <- results$cluster_profiles
  n_pat <- if (!is.null(cm)) nrow(cm) else 0L

  summ <- if (!is.null(cm)) {
    tryCatch(build_cohort_summary(cm, pd), error = function(e) NULL)
  } else NULL

  # ── CSS ────────────────────────────────────────────────────────────────────
  css <- htmltools::tags$style(htmltools::HTML("
    body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif;
      max-width:1000px;margin:0 auto;padding:24px 32px 80px;color:#1a1a2e;font-size:14px;line-height:1.65;}
    h1{font-size:1.8em;font-weight:800;color:#0f3460;}
    h2{font-size:1.25em;font-weight:700;color:#0f3460;margin:36px 0 10px;
      padding-bottom:6px;border-bottom:2px solid #e94560;}
    h3{font-size:1em;font-weight:700;color:#334155;margin:18px 0 8px;}
    p{margin-bottom:10px;}ul,ol{margin:8px 0 14px 22px;}li{margin-bottom:4px;}
    table{width:100%;border-collapse:collapse;margin:10px 0 18px;font-size:.86em;}
    th{background:#0f3460;color:#e2e8f0;padding:8px 12px;text-align:left;}
    td{border:1px solid #e2e8f0;padding:7px 12px;vertical-align:top;}
    tr:nth-child(even) td{background:#f8fafc;}
    .disclaimer{background:#fef2f2;border:1px solid #fecaca;border-radius:6px;
      padding:12px 16px;margin:16px 0;font-size:.88em;}
    .stat-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin:10px 0 18px;}
    .stat-card{background:#f8fafc;border:1px solid #e2e8f0;border-radius:6px;
      padding:10px;text-align:center;}
    .stat-card .val{font-size:1.5em;font-weight:800;color:#0f3460;}
    .stat-card .lbl{font-size:.76em;color:#64748b;}
    .section-note{font-size:.82em;color:#64748b;font-style:italic;margin-bottom:8px;}
    footer{font-size:.76em;color:#94a3b8;margin-top:48px;
      border-top:1px solid #e2e8f0;padding-top:12px;}
  "))

  sections <- list()

  # Header
  sections$header <- htmltools::tagList(
    htmltools::tags$h1(cohort_name, " -- Clinician Review Packet"),
    htmltools::tags$p(
      style = "color:#64748b;",
      "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), " | ",
      sprintf("%d patients", n_pat), " | CohortIntelligence v0.1"
    ),
    htmltools::tags$div(
      class = "disclaimer",
      htmltools::tags$b("IMPORTANT DISCLAIMER: "),
      "This report is hypothesis-generating and based on structured OMOP CDM ",
      "evidence only. It does NOT replace formal chart review, clinical ",
      "adjudication, or causal inference analysis. All findings are preliminary ",
      "and require clinical validation before use in research or patient care."
    )
  )

  # Demographics
  if (!is.null(summ)) {
    med_fu <- if (!is.na(summ$median_followup))
      paste0(round(summ$median_followup / 30.4375, 1), " mo") else "N/A"
    sections$demo <- htmltools::tagList(
      htmltools::tags$h2("Cohort Overview"),
      htmltools::tags$div(
        class = "stat-grid",
        .stat_card(n_pat, "Total patients"),
        .stat_card(med_fu, "Median follow-up"),
        .stat_card(if (!is.na(summ$median_age)) round(summ$median_age, 0) else "N/A",
                   "Median age at index"),
        .stat_card(if (!is.na(summ$pct_death)) paste0(summ$pct_death, "%") else "N/A",
                   "% with death recorded")
      )
    )
  }

  # Quilt image
  if (!is.null(qb) && nrow(qb) > 0L &&
      requireNamespace("ggplot2", quietly = TRUE) &&
      requireNamespace("base64enc", quietly = TRUE)) {
    tmp <- tempfile(fileext = ".png")
    tryCatch({
      p <- .render_quilt_ggplot(qb)
      ggplot2::ggsave(tmp, p,
                       width  = 14,
                       height = min(9, 2 + n_pat / 30),
                       dpi    = 100, device = "png")
      img_b64 <- base64enc::base64encode(tmp)
      sections$quilt <- htmltools::tagList(
        htmltools::tags$h2("Cohort Quilt -- Bird's-eye View"),
        htmltools::tags$p(
          class = "section-note",
          "Each row = one patient. Columns = OMOP domain x time window. ",
          "Colour intensity = event count (log1p scale)."
        ),
        htmltools::tags$img(
          src   = paste0("data:image/png;base64,", img_b64),
          style = "max-width:100%; border:1px solid #e2e8f0; border-radius:6px;"
        )
      )
      unlink(tmp)
    }, error = function(e) NULL)
  }

  # Cluster profiles
  if (!is.null(cp) && !is.null(cp$summary) && nrow(cp$summary) > 0L) {
    labels <- tryCatch(label_clusters(cp), error = function(e) NULL)
    s <- cp$summary |>
      dplyr::mutate(
        label    = if (!is.null(labels)) labels[as.character(cluster_id)] else paste0("Cluster ", cluster_id),
        med_age  = round(median_age, 1),
        pct_f    = paste0(round(pct_female, 0), "%"),
        med_fu   = paste0(round(median_followup_days / 30.4375, 1), " mo"),
        pct_d    = paste0(round(pct_death, 0), "%")
      ) |>
      dplyr::select(label, n_patients, pct_cohort, med_age, pct_f, med_fu, pct_d)

    sections$clusters <- htmltools::tagList(
      htmltools::tags$h2("Cluster Profile Summary"),
      htmltools::tags$p(
        class = "section-note",
        "Cluster labels are descriptive, not diagnostic. ",
        "All interpretations require clinical review."
      ),
      .df_to_html_table(s, c("Cluster", "N", "% Cohort",
                               "Median Age", "% Female",
                               "Median F/U", "% Death"))
    )
  }

  # Review sets summary
  if (isTRUE(include$outliers) && !is.null(rs) && nrow(rs) > 0L) {
    set_counts <- rs |>
      dplyr::count(review_set, name = "n") |>
      dplyr::rename(`Review Set` = review_set, `N Patients` = n)
    sections$review_sets <- htmltools::tagList(
      htmltools::tags$h2("Review Sets"),
      htmltools::tags$p(class = "section-note",
        "Recommended review order: Typical patients -> Most Anomalous -> ",
        "Sparse Follow-up -> Temporal Concern."),
      .df_to_html_table(set_counts)
    )
  }

  # Temporal flags summary
  if (isTRUE(include$temporal_flags) && !is.null(tf) && nrow(tf) > 0L) {
    flag_summary <- tf |>
      dplyr::count(flag_label, severity, name = "n_patients") |>
      dplyr::arrange(dplyr::desc(severity == "high"), dplyr::desc(n_patients))
    sections$temp_flags <- htmltools::tagList(
      htmltools::tags$h2("Temporal Rule Flags"),
      htmltools::tags$p(class = "section-note",
        "Flags are review triggers only. Use cautious language in discussion."),
      .df_to_html_table(flag_summary, c("Flag", "Severity", "N Patients"))
    )
  }

  # Hypotheses
  if (isTRUE(include$hypotheses) && !is.null(hyp) && nrow(hyp) > 0L) {
    h_disp <- hyp |>
      dplyr::slice_head(n = 10L) |>
      dplyr::mutate(
        p_value_adjusted = round(p_value_adjusted, 4),
        effect_size      = round(effect_size, 3)
      ) |>
      dplyr::select(cluster_a, cluster_b, domain, concept_name,
                    window_label, effect_size, p_value_adjusted, direction)
    sections$hyp <- htmltools::tagList(
      htmltools::tags$h2("Candidate Research Hypotheses (Top 10)"),
      htmltools::tags$p(class = "section-note",
        "Statistical tests compare feature distributions between ML clusters. ",
        "Circular: clusters were defined on the same data. Requires external validation."),
      .df_to_html_table(h_disp, c("Cluster A", "Cluster B", "Domain",
                                    "Concept", "Window", "Effect",
                                    "Adj. p-value", "Direction"))
    )
  }

  # Recommended next steps
  sections$next_steps <- htmltools::tagList(
    htmltools::tags$h2("Recommended Next Steps"),
    htmltools::tags$ol(
      htmltools::tags$li("Review patient trajectories for the top high-priority cases."),
      htmltools::tags$li("Discuss cluster interpretations with a clinical expert."),
      htmltools::tags$li("Validate temporal flags by checking source clinical records."),
      htmltools::tags$li("Select hypotheses for formal pre-specified analysis."),
      htmltools::tags$li("Consider CohortDiagnostics for formal phenotype validation.")
    ),
    htmltools::tags$h3("Limitations"),
    htmltools::tags$ul(
      htmltools::tags$li("Structured OMOP data only -- no free text, imaging, or out-of-network events."),
      htmltools::tags$li("Unsupervised ML results depend on feature engineering choices."),
      htmltools::tags$li("Death capture is often incomplete in EHR data."),
      htmltools::tags$li("All cluster-based hypotheses are circular and require external validation.")
    )
  )

  sections$footer <- htmltools::tags$footer(
    "CohortIntelligence v0.1.0  |  Johns Hopkins University  |  ",
    "OMOP CDM v5.3/5.4  |  Hypothesis-generating only. Not for clinical decisions."
  )

  doc <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$title(cohort_name, " -- Clinician Review Packet"),
      css
    ),
    htmltools::tags$body(sections)
  )

  htmltools::save_html(doc, file = path)
  message("Clinician review packet saved to: ", path)
  invisible(path)
}
