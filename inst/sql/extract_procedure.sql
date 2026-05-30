-- extract_procedure.sql
-- Extract procedure_occurrence records for a set of subjects.
--
-- Parameters:
--   @cdm_schema   : schema containing OMOP CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @subject_ids  : comma-separated integer list
--   @start_date   : lower bound on procedure_date
--   @end_date     : upper bound on procedure_date

SELECT
    po.procedure_occurrence_id,
    po.person_id,
    CAST(po.procedure_date AS DATE) AS procedure_date,
    po.procedure_concept_id,
    c.concept_name                  AS procedure_name,
    po.procedure_source_value

FROM @cdm_schema.procedure_occurrence po

LEFT JOIN @vocab_schema.concept c
    ON po.procedure_concept_id = c.concept_id

WHERE po.person_id IN (@subject_ids)
  AND po.procedure_date >= CAST('@start_date' AS DATE)
  AND po.procedure_date <= CAST('@end_date'   AS DATE)

ORDER BY po.person_id, po.procedure_date;
