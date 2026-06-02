# hypotheses.R
# Rule-based hypothesis generation for CohortIntelligence.
# Compares feature distributions between cluster pairs using
# Wilcoxon rank-sum tests (continuous) and Fisher's exact tests (binary),
# with BH multiple testing correction.

#' Generate candidate research hypotheses from cluster-level differences
#'
#' For each pair of clusters, tests whether each feature in the long feature
#' matrix differs significantly. Returns the top hypotheses after correction.
#'
#' @param feature_matrix List from [build_feature_matrix()]. Uses `$long`.
#' @param ml_results List from [run_full_ml_pipeline()]. Uses `$clusters`.
#' @param min_effect_size Numeric. Minimum absolute effect size (rank-biserial
#'   correlation for continuous, Cramer's V for binary) to retain.
#' @param max_hypotheses Integer. Maximum number of hypotheses to return.
#' @param include_unmapped Logical. If `FALSE` (default), concepts labelled
#'   `"Unmapped concept"` (the canonical term for OMOP concept_id = 0) or
#'   similar are excluded from the ranked output. Unmapped concepts reflect
#'   vocabulary coverage gaps and are data-quality signals, not clinical
#'   hypotheses. Set `TRUE` only to audit mapping coverage.
#'
#' @return tibble(hypothesis_id, cluster_a, cluster_b, domain, concept_name,
#'   window_label, effect_size, p_value_raw, exploratory_p_adjusted,
#'   direction, description_text)
#' @export
generate_hypotheses <- function(feature_matrix,
                                 ml_results,
                                 min_effect_size  = 0.3,
                                 max_hypotheses   = 20L,
                                 include_unmapped = FALSE) {
  long     <- feature_matrix$long
  clusters <- if (is.data.frame(ml_results)) ml_results else ml_results$clusters

  if (nrow(long) == 0L || is.null(clusters)) {
    return(.empty_hypotheses())
  }

  # Join cluster assignments onto long feature tibble
  long <- dplyr::left_join(long, dplyr::select(clusters, subject_id, cluster_id),
                            by = "subject_id")
  long <- long[!is.na(long$cluster_id) & long$cluster_id != -1L, , drop = FALSE]

  cluster_ids <- sort(unique(long$cluster_id))
  if (length(cluster_ids) < 2L) return(.empty_hypotheses())

  # All unique cluster pairs
  pairs <- utils::combn(cluster_ids, 2L, simplify = FALSE)

  features <- dplyr::distinct(long, domain, concept_name, window_label)

  results <- purrr::map_dfr(pairs, function(pair) {
    ca <- pair[[1L]]
    cb <- pair[[2L]]

    purrr::map_dfr(seq_len(nrow(features)), function(i) {
      feat <- features[i, ]
      vals <- long[long$domain       == feat$domain      &
                     long$concept_name == feat$concept_name &
                     long$window_label == feat$window_label, , drop = FALSE]

      x_a <- vals$value[vals$cluster_id == ca]
      x_b <- vals$value[vals$cluster_id == cb]

      if (length(x_a) < 2L || length(x_b) < 2L) return(tibble::tibble())

      is_binary <- all(vals$value %in% c(0, 1))

      tryCatch({
        if (is_binary) {
          ct  <- table(c(rep(ca, length(x_a)), rep(cb, length(x_b))),
                       c(x_a, x_b))
          ft  <- stats::fisher.test(ct)
          p   <- ft$p.value
          eff <- .cramers_v(ct)
          dir <- if (mean(x_a) > mean(x_b)) "higher_in_cluster_a" else "higher_in_cluster_b"
        } else {
          wt  <- stats::wilcox.test(x_a, x_b, exact = FALSE)
          p   <- wt$p.value
          eff <- .rank_biserial(x_a, x_b)
          dir <- if (mean(x_a, na.rm = TRUE) > mean(x_b, na.rm = TRUE))
                   "higher_in_cluster_a" else "higher_in_cluster_b"
        }
        tibble::tibble(
          cluster_a    = ca,
          cluster_b    = cb,
          domain       = feat$domain,
          concept_name = feat$concept_name,
          window_label = feat$window_label,
          effect_size  = abs(eff),
          p_value_raw  = p,
          direction    = dir
        )
      }, error = function(e) tibble::tibble())
    })
  })

  if (nrow(results) == 0L) return(.empty_hypotheses())

  # Rename to emphasise exploratory nature
  results$exploratory_p_adjusted <- stats::p.adjust(results$p_value_raw,
                                                      method = "BH")

  # Filter unmapped concepts by default. "Unmapped concept" is the canonical
  # label (normalised at extract time); "No matching concept" is retained as
  # a legacy fallback for data loaded before that normalisation was applied.
  # These entries are data-quality signals (vocabulary coverage gaps), not
  # clinical hypotheses, and should not rank among the top findings.
  if (!include_unmapped) {
    unmapped_pattern <- "^(Unmapped concept|No matching concept|unknown|NA)$"
    results <- results[
      !grepl(unmapped_pattern, results$concept_name, ignore.case = TRUE),
      , drop = FALSE
    ]
  }

  results <- results[results$effect_size >= min_effect_size, , drop = FALSE]
  results <- results[order(results$exploratory_p_adjusted), , drop = FALSE]
  results <- utils::head(results, max_hypotheses)

  if (nrow(results) == 0L) return(.empty_hypotheses())

  results <- dplyr::mutate(results,
    hypothesis_id    = seq_len(dplyr::n()),
    description_text = sprintf(
      "Cluster %s vs Cluster %s: %s (%s, %s) -- effect size = %.2f, exploratory adj. p = %.3g",
      cluster_a, cluster_b, concept_name, domain, window_label,
      effect_size, exploratory_p_adjusted
    )
  )

  dplyr::select(results,
    hypothesis_id, cluster_a, cluster_b, domain, concept_name, window_label,
    effect_size, p_value_raw, exploratory_p_adjusted, direction, description_text
  )
}

#' Format hypotheses as text or HTML
#'
#' @param hypotheses_df tibble from [generate_hypotheses()].
#' @param format One of `"tibble"`, `"text"`, `"html"`.
#'
#' @return Formatted output.
#' @export
format_hypotheses_report <- function(hypotheses_df,
                                      format = c("tibble","text","html")) {
  format <- match.arg(format)
  if (format == "tibble") return(hypotheses_df)
  if (nrow(hypotheses_df) == 0L) {
    txt <- "No significant hypotheses found."
    return(if (format == "html") paste0("<p>", txt, "</p>") else txt)
  }
  dq_note <- paste0(
    "Note: concepts labelled \"Unmapped concept\" indicate vocabulary coverage",
    " gaps and are data-quality signals, not clinical hypotheses. They are",
    " excluded by default (see include_unmapped parameter)."
  )
  lines <- paste0(seq_len(nrow(hypotheses_df)), ". ",
                  hypotheses_df$description_text)
  if (format == "text") {
    return(paste(c(paste(lines, collapse = "\n"), "", dq_note),
                 collapse = "\n"))
  }
  paste0(
    "<ol>", paste0("<li>", lines, "</li>", collapse = ""), "</ol>",
    "<p class=\"dq-note\"><em>", dq_note, "</em></p>"
  )
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.empty_hypotheses <- function() {
  tibble::tibble(
    hypothesis_id          = integer(0),
    cluster_a              = integer(0),
    cluster_b              = integer(0),
    domain                 = character(0),
    concept_name           = character(0),
    window_label           = character(0),
    effect_size            = numeric(0),
    p_value_raw            = numeric(0),
    exploratory_p_adjusted = numeric(0),
    direction              = character(0),
    description_text       = character(0)
  )
}

.rank_biserial <- function(x, y) {
  # Rank-biserial correlation for Wilcoxon test effect size
  nx <- length(x); ny <- length(y)
  U  <- stats::wilcox.test(x, y, exact = FALSE)$statistic
  1 - (2 * U) / (nx * ny)
}

.cramers_v <- function(ct) {
  chi2 <- suppressWarnings(stats::chisq.test(ct)$statistic)
  n    <- sum(ct)
  k    <- min(nrow(ct), ncol(ct))
  sqrt(chi2 / (n * (k - 1)))
}
