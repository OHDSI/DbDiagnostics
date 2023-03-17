# DbDiagnostics README

The `executeDbProfile` function in this package relies on the [Achilles](https://github.com/ohdsi/Achilles) and [DataQualityDashboard](https://github.com/ohdsi/DataQualityDashboard) packages to run a subset of characterization and data quality analyses, along with metadata about the database. This information is collectively referred to as the database profile. This profile will be used to determine if a database has the necessary elements required to run a study. 

It works by connecting to a database through a connectionDetails object created by the [DatabaseConnector](http://ohdsi.github.io/DatabaseConnector/articles/Connecting.html) package. It will first look for metadata about the database, either in the parameters of the function or in a csv file in the output folder. If neither are present the function will ask the user a series of questions in the form of pop-ups and save this information in a csv file to ship along with the output. It will then check to see if Achilles results are already present. If so, it will export those results. If not, it will run the required Achilles analyses and then export. Then, it will run a set of DataQualityDashboard checks and export those results as well.

Once the results are generated they are then loaded to a separate results schema. The `executeDbDiagnostics` function will take in a list of analysis settings to compare against the dbProfile results to determine if a database is eligible to run a particular analysis.

## How to run DbDiagnostics::executeDbProfile

1. Create a connectionDetails object:

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
  
2. Call the executeDbProfile function with options:

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

3. Email the file `DbProfileResults_<cdmSourceName>.zip` in the `outputFolder` directory to the data-quality study coordinator.


4. Output:

  The summary statistics results `*.csv` file, the CDM_SOURCE table, metadata.csv and the data quality dashboard JSON file will be located in the output location you specified in the execute function. 
