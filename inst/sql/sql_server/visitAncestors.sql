/*This query will return the high-level ancestors for every visit concept*/

with cte_visit_ancestors as (

SELECT concept_id, concept_name
  FROM @vocabDatabaseSchema.concept
  LEFT JOIN @vocabDatabaseSchema.concept_ancestor
    ON concept_id=descendant_concept_id
    AND ancestor_concept_id!=descendant_concept_id
  WHERE domain_id='Visit'
    AND standard_concept='S'
    AND ancestor_concept_id IS NULL
)

SELECT descendant_concept_id,
       c2.concept_name as descendant_concept_name,
       c1.concept_id as visit_ancestor_concept_id,
       c1.concept_name as visit_ancestor_concept_name
FROM cte_visit_ancestors c1
JOIN @vocabDatabaseSchema.concept_ancestor ca
  ON c1.concept_id = ca.ancestor_concept_id
JOIN @vocabDatabaseSchema.concept c2
  ON ca.descendant_concept_id = c2.concept_id

