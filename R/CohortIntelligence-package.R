#' @keywords internal
"_PACKAGE"

#' @importFrom stats predict reorder setNames
#' @importFrom rlang .data
NULL

utils::globalVariables(c(
  "person_id", "subject_id", "cohort_definition_id",
  "cohort_start_date", "cohort_end_date",
  "domain", "window_label", "window_idx", "window_start", "window_end",
  "event_count", "presence", "days_from_index", "index_date",
  "concept_id", "concept_name", "value", "months_start", "months_end",
  "feature_col",
  "patient_row", "display_label", "fill_value", "clip_max",
  "cluster_id", "cluster_label", "rank_score", "anomaly_score",
  "umap_1", "umap_2", "sparsity_score", "rank_position", "priority_tier",
  "anomaly_norm", "noise_flag", "zero_cells",
  "domain_rank",
  "hypothesis_id", "cluster_a", "cluster_b", "effect_size",
  "p_value_raw", "p_value_adjusted", "direction", "description_text",
  "n", "value_as_number", "birth_year",
  "gender_concept_id", "race_concept_id", "ethnicity_concept_id"
))

`%||%` <- function(x, y) if (!is.null(x)) x else y
