---
title: "DbProfile Summary Statistics"
output: html_document
date: "2023-03-31"
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{DbProfile Summary Statistics}
  %\usepackage[UTF-8]{inputenc}
---

## List of Summary Statistics Included in executeDbProfile

**The executeDbProfile function will execute and/or export the following aggregate summary statistics and DQD checks:**

### Statistics 

- Number of persons
- Number of persons by gender
- Number of persons by year of birth
- Number of persons by race
- Number of persons by ethnicity
- Number of persons with at least one day of observation in each month
- Number of persons by observation period start month
- Number of persons by number of observation periods
- Number of persons by length of observation period, in 30d increments
- Number of persons with at least one visit occurrence, by visit_concept_id
- Number of distinct patients that overlap between specific domains - including death
- Number of persons with at least one concept_id, by measurement_concept_id
- Number of measurement occurrence records, by measurement_concept_id
- Number of measurement occurrence records, by measurement_source_concept_id
- Number of measurement records with no value (numeric, string, or concept)
- Number of persons with at least one concept_id, by condition_concept_id
- Number of condition occurrence records, by condition_concept_id
- Number of condition occurrence records, by condition_source_concept_id
- Number of persons with at least one concept_id, by procedure_concept_id
- Number of procedure occurrence records, by procedure_concept_id
- Number of procedure occurrence records, by procedure_source_concept_id
- Number of persons with at least one concept_id, by drug_concept_id
- Number of drug exposure records, by drug_concept_id
- Number of drug exposure records, by drug_source_concept_id
- Number of persons with at least one concept_id, by observation_concept_id
- Number of observation occurrence records, by observation_concept_id
- Number of observation occurrence records, by observation_source_concept_id
- Number of persons with at least one concept_id, by device_concept_id
- Number of device exposure records, by device_concept_id
- Number of device exposure records, by device_source_concept_id
- Distribution of numeric values, by measurement_concept_id and unit_concept_id

Later inclusions
- condition_status? 
- type concepts


### Data Quality Checks

- [measurePersonCompleteness](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#measurepersoncompleteness-1)
- [measureValueCompleteness](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#measurevaluecompleteness-1)
- [cdmField](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#cdmfield-1)
- [cdmTable](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#cdmtable-1)
- [isRequired](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#isrequired)
- [cdmDatatype](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#cdmdatatype-1)
- [isPrimaryKey](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#isprimarykey-1)
- [isForeignKey](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#isforeignkey-1)
- [fkDomain](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#fkdomain-1)
- [fkClass](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#fkclass-1)
- [isStandardValidConcept](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#isstandardvalidconcept-1)
- [standardConceptRecordCompleteness](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#standardconceptrecordcompleteness)
- [sourceConceptRecordCompleteness](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#sourceconceptrecordcompleteness-1)
- [plausibleValueLow](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#plausiblevaluelow-2)
- [plausibleValueHigh](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#plausiblevaluehigh-2)
- [plausibleTemporalAfter](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#plausibletemporalafter-1)
- [plausibleDuringLife](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#plausibleduringlife-1)
