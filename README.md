# DbProfile README

This package relies on the [Achilles](https://github.com/ohdsi/Achilles) and [DataQualityDashboard](https://github.com/ohdsi/DataQualityDashboard) packages to run a subset of characterization and data quality analyses. This subset is referred to as the database profile. This profile will be used to determine if a database has the necessary elements required to run a study. 

It works by connecting to a database through a connectionDetails object created by the [DatabaseConnector](http://ohdsi.github.io/DatabaseConnector/articles/Connecting.html) package. It will then check to see if Achilles results are already present. If so, it will export those results. If not, it will run the required Achilles analyses and then export. Then, it will run a set of DataQualityDashboard checks and export those results as well.

## How to run DbProfile

1. Create an empty folder or new RStudio project, and in `R`, use the following code to install the study package and its dependencies:

  ```r
  install.packages("renv")
  download.file("https://raw.githubusercontent.com/OHDSI/DbProfile/main/extras/renv.lock", "renv.lock")
  renv::init()
  ```  

  Once installed, you can execute the study by modifying and using the following code.
  
2. Create a connectionDetails object:

  Call the `createConnectionDetails` function to create the object, inputting the information for how to connect to your database. Detailed instructions on how to do this can be found [here](http://ohdsi.github.io/DatabaseConnector/articles/Connecting.html). 

  ```r 
  library(DbProfile)
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = Sys.getenv("dbms"),
      server = Sys.getenv("server"),
    user = Sys.getenv("user"),
    password = Sys.getenv("password"),
    pathToDriver = Sys.getenv("path_to_driver"))
  ```
  
3. Call the execute function with options:

  ```r
  # The schema where your CDM-structured data are housed
  cdmDatabaseSchema <- "" 
  
  # The (writeable) schema where your achilles results are or will be housed
  resultsDatabaseSchema <- ""
  
  # The schema where your vocabulary tables are housed, typically the same as the cdmDatabaseSchema
  vocabDatabaseSchema <- cdmDatabaseSchema
  
  # A unique, identifiable name for your database
  cdmSourceName <- ""
  
  # The folder where your results should be written
  outputFolder <- ""
  
  # The version of the OMOP CDM you are currently on, v5.3 and v5.4 are supported.
  cmdVersion <- "5.3"
  
  # Whether the function should overwrite existing Achilles tables and create new ones
  overwriteAchilles <- FALSE
  
  # The location on your machine of the DQD config file for concept check thresholds or "default".
  conceptCheckThresholds <- system.file("thresholds", "LegendT2dm.adf", package = "DbProfile")
    
  execute <- function(connectionDetails = connectionDetails
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      resultsDatabaseSchema = resultsDatabaseSchema,
                      vocabDatabaseSchema = vocabDatabaseSchema,
                      cdmSourceName = cdmSourceName,
                      outputFolder = outputFolder,
                      cdmVersion = cdmVersion,
                      overwriteAchilles = overwriteAchilles,
                      conceptCheckThresholds = conceptCheckThresholds)
  ```


**This function will execute and/or export the following Achilles analyses and DQD checks:**

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
- Number of distinct patients that overlap between specific domains
- Number of measurement occurrence records, by measurement_concept_id
- Number of measurement records with no value (numeric, string, or concept)
- Number of condition occurrence records, by condition_concept_id
- Number of procedure occurrence records, by procedure_concept_id
- Number of drug exposure records, by drug_concept_id
- Number of observation occurrence records, by observation_concept_id
- Number of device exposure records, by device_concept_id
- Distribution of numeric values, by measurement_concept_id and unit_concept_id

- [measurePersonCompleteness](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#measurepersoncompleteness-1)
- [cdmField](https://ohdsi.github.io/DataQualityDashboard/articles/CheckTypeDescriptions.html#cdmfield-1)
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

4. Output:

  The Achilles results `*.csv` file and the data quality dashboard JSON file will be located in the output location you specified in the execute function. 
