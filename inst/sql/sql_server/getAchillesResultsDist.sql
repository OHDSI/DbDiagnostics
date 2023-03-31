/*This query will return the achilles_results_dist*/

SELECT *
FROM @resultsDatabaseSchema.achilles_results_dist
WHERE analysis_id IN (@analysis_ids)
AND count_value > @min_cell_count
