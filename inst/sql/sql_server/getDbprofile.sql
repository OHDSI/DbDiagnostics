SELECT 
    release_key,
    analysis_id,
    stratum_1,
    count_value,
    visit_ancestor_concept_id 
FROM @results_database_schema.@results_table_name
WHERE release_key = '@databaseName'
    AND analysis_id IN (1800, 400, 600, 700, 800, 2100)
    AND CAST(stratum_1 AS INTEGER) IN (@study_concept_ids)

UNION ALL

SELECT 
    release_key,
    analysis_id,
    stratum_1,
    count_value,
    visit_ancestor_concept_id
FROM @results_database_schema.@results_table_name
WHERE release_key = '@databaseName'
    AND analysis_id IN (@base_analysis_ids)
