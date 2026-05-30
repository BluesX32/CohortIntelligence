-- extract_condition.sql
-- Extract condition_occurrence records for a set of subjects.
--
-- Parameters:
--   @cdm_schema   : schema containing OMOP CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @subject_ids  : comma-separated integer list
--   @start_date   : lower bound on condition_start_date
--   @end_date     : upper bound on condition_start_date

SELECT
    co.condition_occurrence_id,
    co.person_id,
    CAST(co.condition_start_date AS DATE) AS condition_start_date,
    CAST(co.condition_end_date   AS DATE) AS condition_end_date,
    co.condition_concept_id,
    c.concept_name                        AS condition_name,
    co.condition_source_value

FROM @cdm_schema.condition_occurrence co

LEFT JOIN @vocab_schema.concept c
    ON co.condition_concept_id = c.concept_id

WHERE co.person_id IN (@subject_ids)
  AND co.condition_start_date >= CAST('@start_date' AS DATE)
  AND co.condition_start_date <= CAST('@end_date'   AS DATE)

ORDER BY co.person_id, co.condition_start_date;
