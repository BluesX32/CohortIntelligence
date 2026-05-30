-- extract_person.sql
-- Extract person demographics for a set of subjects.
--
-- Parameters:
--   @cdm_schema   : schema containing OMOP CDM tables
--   @vocab_schema : schema containing vocabulary tables
--   @subject_ids  : comma-separated integer list

SELECT
    p.person_id,
    p.year_of_birth                AS birth_year,
    p.gender_concept_id,
    gc.concept_name                AS gender_name,
    p.race_concept_id,
    rc.concept_name                AS race_name,
    p.ethnicity_concept_id,
    ec.concept_name                AS ethnicity_name

FROM @cdm_schema.person p

LEFT JOIN @vocab_schema.concept gc ON p.gender_concept_id    = gc.concept_id
LEFT JOIN @vocab_schema.concept rc ON p.race_concept_id      = rc.concept_id
LEFT JOIN @vocab_schema.concept ec ON p.ethnicity_concept_id = ec.concept_id

WHERE p.person_id IN (@subject_ids);
