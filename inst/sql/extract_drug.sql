-- extract_drug.sql
-- Extract drug_exposure records for a set of subjects.
--
-- Parameters:
--   @cdm_schema   : schema containing OMOP CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @subject_ids  : comma-separated integer list
--   @start_date   : lower bound on drug_exposure_start_date
--   @end_date     : upper bound on drug_exposure_start_date

SELECT
    de.drug_exposure_id,
    de.person_id,
    CAST(de.drug_exposure_start_date AS DATE) AS drug_exposure_start_date,
    CAST(de.drug_exposure_end_date   AS DATE) AS drug_exposure_end_date,
    de.drug_concept_id,
    c.concept_name                            AS drug_name,
    de.drug_source_value

FROM @cdm_schema.drug_exposure de

LEFT JOIN @vocab_schema.concept c
    ON de.drug_concept_id = c.concept_id

WHERE de.person_id IN (@subject_ids)
  AND de.drug_exposure_start_date >= CAST('@start_date' AS DATE)
  AND de.drug_exposure_start_date <= CAST('@end_date'   AS DATE)

ORDER BY de.person_id, de.drug_exposure_start_date;
