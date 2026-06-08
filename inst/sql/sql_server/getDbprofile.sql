SELECT 
    release_key,
    analysis_id,
    stratum_1,
    count_value
FROM @results_database_schema.@results_table_name
WHERE release_key = '@databaseName'
    AND analysis_id IN (1800, 400, 600, 700, 800, 2100)
    AND CAST(stratum_1 AS INT) IN @study_concept_ids

UNION

SELECT 
    release_key,
    analysis_id,
    stratum_1,
    count_value
FROM @results_database_schema.@results_table_name
WHERE release_key = '@databaseName'
    AND analysis_id IN (@base_analysis_ids)

UNION

SELECT r
    release_key,
    analysis_id,
    MAX(stratum_1) AS stratum_1,
    count_value
FROM @results_database_schema.@results_table_name
WHERE release_key = '@databaseName'
    AND analysis_id IN (101, 111, 112)

UNION

SELECT r
    release_key,
    analysis_id,
    MIN(stratum_1) AS stratum_1,
    count_value
FROM @results_database_schema.@results_table_name
WHERE release_key = '@databaseName'
    AND analysis_id IN (101, 111, 112)
