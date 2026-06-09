SELECT 
    release_key,
    analysis_id,
    stratum_1,
    count_value,
    visit_ancestor_concept_id 
FROM @results_database_schema.@results_table_name
WHERE release_key = '@databaseName'
    AND analysis_id IN (1800, 400, 600, 700, 800, 2100)
    AND CAST(stratum_1 AS INT) IN (@study_concept_ids)

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

UNION ALL

SELECT 
    release_key,
    analysis_id,
    stratum_1,
    count_value,
    visit_ancestor_concept_id
FROM (
    SELECT  
        release_key,
        analysis_id,
        stratum_1,
        count_value,
        visit_ancestor_concept_id
        ROW_NUMBER() OVER (
            PARTITION BY release_key, analysis_id
            ORDER BY count_value DESC
        ) as row_number
    FROM @results_database_schema.@results_table_name
    WHERE release_key = '@databaseName'
        AND analysis_id IN (101, 111, 112)
) tmp
where row_number = 1

UNION ALL

SELECT 
    release_key,
    analysis_id,
    stratum_1,
    count_value,
    visit_ancestor_concept_id
FROM (
    SELECT  
        release_key,
        analysis_id,
        stratum_1,
        count_value,
        visit_ancestor_concept_id
        ROW_NUMBER() OVER (
            PARTITION BY release_key, analysis_id
            ORDER BY count_value ASC
        ) as row_number
    FROM @results_database_schema.@results_table_name
    WHERE release_key = '@databaseName'
        AND analysis_id IN (101, 111, 112)
) tmp
where row_number = 1