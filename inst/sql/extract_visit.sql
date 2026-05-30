-- extract_visit.sql
-- Extract visit_occurrence records for a set of subjects.
--
-- Parameters:
--   @cdm_schema   : schema containing OMOP CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @subject_ids  : comma-separated integer list
--   @start_date   : lower bound on visit_start_date
--   @end_date     : upper bound on visit_start_date

SELECT
    vo.visit_occurrence_id,
    vo.person_id,
    CAST(vo.visit_start_date AS DATE) AS visit_start_date,
    CAST(vo.visit_end_date   AS DATE) AS visit_end_date,
    vo.visit_concept_id,
    c.concept_name                    AS visit_type,
    vo.visit_source_value

FROM @cdm_schema.visit_occurrence vo

LEFT JOIN @vocab_schema.concept c
    ON vo.visit_concept_id = c.concept_id

WHERE vo.person_id IN (@subject_ids)
  AND vo.visit_start_date >= CAST('@start_date' AS DATE)
  AND vo.visit_start_date <= CAST('@end_date'   AS DATE)

ORDER BY vo.person_id, vo.visit_start_date;
