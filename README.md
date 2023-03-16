# DbDiagnostics README

The `executeDbProfile` function in this package relies on the [Achilles](https://github.com/ohdsi/Achilles) and [DataQualityDashboard](https://github.com/ohdsi/DataQualityDashboard) packages to run a subset of characterization and data quality analyses, along with metadata about the database. This information is collectively referred to as the database profile. This profile will be used to determine if a database has the necessary elements required to run a study. 

It works by connecting to a database through a connectionDetails object created by the [DatabaseConnector](http://ohdsi.github.io/DatabaseConnector/articles/Connecting.html) package. It will first look for metadata about the database, either in the parameters of the function or in a csv file in the output folder. If neither are present the function will ask the user a series of questions in the form of pop-ups and save this information in a csv file to ship along with the output. It will then check to see if Achilles results are already present. If so, it will export those results. If not, it will run the required Achilles analyses and then export. Then, it will run a set of DataQualityDashboard checks and export those results as well.

Once the results are generated they are then loaded to a separate results schema. The `executeDbDiagnostics` function will take in a list of analysis settings to compare against the dbProfile results to determine if a database is eligible to run a particular analysis.

## How to run DbDiagnostics::executeDbProfile

0. Ensure that your `GitHub Personal Access Token` lies in an accessible `.Renviron` file.

1. Create an empty folder or new RStudio project, and in `R`, use the following code to install the study package and its dependencies:

  ```r
  install.packages("renv")
  download.file("https://raw.githubusercontent.com/OHDSI/DbProfile/main/extras/renv.lock", "renv.lock")
  renv::init()
  ```
  
  Select option 1 (Restore the project from the lockfile).

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
  
3. Call the executeDbProfile function with options:

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
  cdmVersion <- "5.3"
  
  # Whether the function should overwrite existing Achilles tables and create new ones
  overwriteAchilles <- FALSE
  
  # Whether to round to the 10s or 100s place. Valid inputs are 10 or 100, default is 10.
  roundTo <- 10
  
  # Vector of concepts to exclude from the output. Note: No patient-level data is pulled as part of the package or included as part of the output
  excludedConcepts <- c()
  
  # Whether the DQD should be run as part of the profile exercise
  addDQD <- FALSE
  
  
    
  DbDiagnostics::executeDbProfile(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   resultsDatabaseSchema = resultsDatabaseSchema,
                                   vocabDatabaseSchema = vocabDatabaseSchema,
                                   cdmSourceName = cdmSourceName,
                                   outputFolder = outputFolder,
                                   cdmVersion = cdmVersion,
                                   overwriteAchilles = overwriteAchilles,
                                   roundTo = roundTo,
                                   excludedConcepts = excludedConcepts,
                                   addDQD = addDQD
                                   )
  ```
  
  
  # The location on your machine of the DQD config file for concept check thresholds or "default".
  conceptCheckThresholds <- system.file("LegendT2dm", "ConceptLevelMeasurements.csv", package = "DbProfile")

4. Email the file `DbProfileResults_<cdmSourceName>.zip` in the `outputFolder` directory to the data-quality study coordinator.

**DbProfile will execute and/or export the following aggregate summary statistics and DQD checks:**

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
- Number of measurement occurrence records, by measurement_source_concept_id
- Number of measurement records with no value (numeric, string, or concept)
- Number of condition occurrence records, by condition_concept_id
- Number of condition occurrence records, by condition_source_concept_id
- Number of procedure occurrence records, by procedure_concept_id
- Number of procedure occurrence records, by procedure_source_concept_id
- Number of drug exposure records, by drug_concept_id
- Number of drug exposure records, by drug_source_concept_id
- Number of observation occurrence records, by observation_concept_id
- Number of observation occurrence records, by observation_source_concept_id
- Number of device exposure records, by device_concept_id
- Number of device exposure records, by device_source_concept_id
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

  The summary statistics results `*.csv` file, the CDM_SOURCE table, metadata.csv and the data quality dashboard JSON file will be located in the output location you specified in the execute function. 
