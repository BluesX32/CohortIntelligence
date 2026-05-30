-- extract_cohort.sql
-- Extract cohort members from a cohort table.
--
-- Parameters:
--   @cohort_schema        : schema containing the cohort table
--   @cohort_table         : name of the cohort table
--   @cohort_definition_id : integer cohort definition ID

SELECT
    subject_id,
    CAST(cohort_start_date AS DATE) AS cohort_start_date,
    CAST(cohort_end_date   AS DATE) AS cohort_end_date

FROM @cohort_schema.@cohort_table

WHERE cohort_definition_id = @cohort_definition_id

ORDER BY subject_id;
