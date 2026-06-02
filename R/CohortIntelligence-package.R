#' @keywords internal
"_PACKAGE"

#' @importFrom stats predict reorder setNames median quantile sd
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
  "gender_concept_id", "race_concept_id", "ethnicity_concept_id",
  "gender_name", "race_name", "ethnicity_name",
  "age_at_index", "calendar_month", "n_patients",
  "median_followup", "median_age", "pct_death",
  "prevalence", "n_patients_total", "pct_cohort",
  "pct_female", "median_followup_days", "is_female",
  "has_death", "followup_days", "max_prev",
  "cluster_label", "median_fu", "concept_name",
  # explain.R
  "explanation_type", "explanation_label", "explanation_detail",
  "importance_score", "severity", "z_score", "ratio",
  "med_count", "sd_count", "win_total",
  # review_sets.R
  "review_set", "reason_for_selection", "set_priority",
  "total_events", "post_index_events", "pre_index_events",
  "n_post", "n_recurrent",
  # temporal_flags.R
  "flag_type", "flag_label", "flag_description",
  "event_date", "days_from_index", "evidence_summary",
  "recommended_action", "has_post", "gap",
  "days", "n_cond", "window", "death_date",
  # features.R cluster helpers
  "med_prev", "prev_a", "prev_b",
  # export.R clinician report
  "pct_f", "pct_d", "med_age", "label",
  # temporal_flags.R domain columns
  "condition_concept_id", "condition_start_date",
  "drug_exposure_start_date", "total", "pct"
))

`%||%` <- function(x, y) if (!is.null(x)) x else y
