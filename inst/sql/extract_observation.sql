-- extract_observation.sql
-- Extract observation records for a set of subjects.
--
-- Parameters:
--   @cdm_schema   : schema containing OMOP CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @subject_ids  : comma-separated integer list
--   @start_date   : lower bound on observation_date
--   @end_date     : upper bound on observation_date

SELECT
    o.observation_id,
    o.person_id,
    CAST(o.observation_date AS DATE) AS observation_date,
    o.observation_concept_id,
    c.concept_name                   AS observation_name,
    o.value_as_number,
    o.value_as_string,
    o.observation_source_value

FROM @cdm_schema.observation o

LEFT JOIN @vocab_schema.concept c
    ON o.observation_concept_id = c.concept_id

WHERE o.person_id IN (@subject_ids)
  AND o.observation_date >= CAST('@start_date' AS DATE)
  AND o.observation_date <= CAST('@end_date'   AS DATE)

ORDER BY o.person_id, o.observation_date;
