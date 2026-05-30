-- extract_death.sql
-- Extract death records for a set of subjects.
--
-- Parameters:
--   @cdm_schema   : schema containing OMOP CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @subject_ids  : comma-separated integer list
--   @start_date   : lower bound on death_date
--   @end_date     : upper bound on death_date

SELECT
    d.person_id,
    CAST(d.death_date AS DATE) AS death_date,
    d.death_type_concept_id,
    d.cause_concept_id,
    c.concept_name             AS cause_name

FROM @cdm_schema.death d

LEFT JOIN @vocab_schema.concept c
    ON d.cause_concept_id = c.concept_id

WHERE d.person_id IN (@subject_ids)
  AND d.death_date >= CAST('@start_date' AS DATE)
  AND d.death_date <= CAST('@end_date'   AS DATE)

ORDER BY d.person_id;
