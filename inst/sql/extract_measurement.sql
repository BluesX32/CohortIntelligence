-- extract_measurement.sql
-- Extract measurement records for a set of subjects.
--
-- Parameters:
--   @cdm_schema   : schema containing OMOP CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @subject_ids  : comma-separated integer list
--   @start_date   : lower bound on measurement_date
--   @end_date     : upper bound on measurement_date

SELECT
    m.measurement_id,
    m.person_id,
    CAST(m.measurement_date AS DATE) AS measurement_date,
    m.measurement_concept_id,
    c.concept_name                   AS measurement_name,
    m.value_as_number,
    uc.concept_name                  AS unit_name,
    m.measurement_source_value

FROM @cdm_schema.measurement m

LEFT JOIN @vocab_schema.concept c
    ON m.measurement_concept_id = c.concept_id

LEFT JOIN @vocab_schema.concept uc
    ON m.unit_concept_id = uc.concept_id

WHERE m.person_id IN (@subject_ids)
  AND m.measurement_date >= CAST('@start_date' AS DATE)
  AND m.measurement_date <= CAST('@end_date'   AS DATE)

ORDER BY m.person_id, m.measurement_date;
