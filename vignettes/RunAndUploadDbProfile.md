---
title: "How to Run and Upload dbProfile Results"
output: html_document
date: "2023-04-02"
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{How to Run and Upload dbProfile Results}
  %\usepackage[UTF-8]{inputenc}
---



This document will take you through the steps on how to run the database profile functions and upload them to a location to be reference when running database diagnostics.

## Set the parameters to run executeDbProfile

Call the `createConnectionDetails` function to create the object, inputting the information for how to connect to your database. Detailed instructions on how to do this can be found [here](http://ohdsi.github.io/DatabaseConnector/articles/Connecting.html). 

```r
library(DbDiagnostics)

connectionDetails <- DatabaseConnector::createConnectionDetails(
	dbms = "postgresql",
	server = "localhost/synthea",
	user = "postgres",
	password = "postgres"
)

	# The schema where your CDM-structured data are housed
  cdmDatabaseSchema <- "cdm_54_test" 
  
  # The schema where your achilles results are or will be housed
  resultsDatabaseSchema <- "cdm_54_results"
  
  # The schema where any missing achilles analyses should be written. 
  writeTo <- "cdm_54_results"
  
  # The schema where your vocabulary tables are housed, typically the same as the cdmDatabaseSchema
  vocabDatabaseSchema <- cdmDatabaseSchema
  
  # A unique, identifiable name for your database
  cdmSourceName <- "Synthea_v54"
  
  # The folder where your results should be written
  outputFolder <- folder where output is stored
  
  # The version of the OMOP CDM you are currently on, v5.3 and v5.4 are supported.
  cdmVersion <- "5.4"
  
  # Whether the function should append existing Achilles tables or create new ones
  appendAchilles <- FALSE
  
  # Whether to round to the 10s or 100s place. Valid inputs are 10 or 100, default is 10.
  roundTo <- 10
  
  # Vector of concepts to exclude from the output. Note: No patient-level data is pulled as part of the package or included as part of the output
  excludedConcepts <- c()
  
  # Whether the DQD should be run as part of the profile exercise
  addDQD <- FALSE
  
```

## Run the executeDbProfile function

```r

DbDiagnostics::executeDbProfile(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   resultsDatabaseSchema = resultsDatabaseSchema,
                                   writeTo = writeTo,
                                   vocabDatabaseSchema = vocabDatabaseSchema,
                                   cdmSourceName = cdmSourceName,
                                   outputFolder = outputFolder,
                                   cdmVersion = cdmVersion,
                                   appendAchilles = appendAchilles,
                                   roundTo = roundTo,
                                   excludedConcepts = excludedConcepts,
                                   addDQD = addDQD
                                   )

```

Upon completion, the summary statistics results `*.csv` file, the CDM_SOURCE table, metadata.csv and the data quality dashboard JSON file (if specified) will be located in a zip file in the output location you set as part of the execute function. 


## Upload dbProfile results to a local schema

Be sure to unzip the dbProfile results that are generated in the above. Then, set the location where you unzipped the results as the parameter `resultsLocation`. Any parameter below denoted as <cdmSourceName> should be replaced with the database key that was generated when `executeDbProfile` was run. You can find this in the file names. 

```r

# set the location of the results that were generated from executeDbProfile
resultsLocation <- ""

# set a parameter detailing if the DQD was run
addDQD <- FALSE

# add dbId and prep output files for writing into the results schema
db_profile_results <- read.csv(paste0(resultsLocation,"/db_profile_results.csv"), stringsAsFactors = F, colClasses = c("STRATUM_1"="character"))

# make sure the columns are read in as characters to facilitate dbDiagnostics execution
db_profile_results$STRATUM_1 <- as.character(db_profile_results$STRATUM_1)
db_profile_results$STRATUM_2 <- as.character(db_profile_results$STRATUM_2)
db_profile_results$STRATUM_3 <- as.character(db_profile_results$STRATUM_3)
db_profile_results$STRATUM_4 <- as.character(db_profile_results$STRATUM_4)
db_profile_results$STRATUM_5 <- as.character(db_profile_results$STRATUM_5)

# read in the metadata

db_metadata <- read.csv(paste0(resultsLocation,"/<cdmSourceName>_metadata.csv"), stringsAsFactors = F)

# read in the cdm_source table

db_cdm_source <- read.csv(paste0(resultsLocation,"/<cdmSourceName>_cdm_source.csv"), stringsAsFactors = F)

# determine which tables should be uploaded based on if the DQD was included
if (addDQD) {

	dqdJsonDf <- jsonlite::fromJSON(
		paste0(outputFolder,"/",dbId,"_DbProfile.json"),
		simplifyDataFrame = TRUE)

	dqd_overview     <- as.data.frame(dqdJsonDf$Overview)
	dqd_checkresults <- as.data.frame(dqdJsonDf$CheckResults)
	
	dqd_checkresults$THRESHOLD_VALUE <- as.character(dqd_checkresults$THRESHOLD_VALUE)

	tablesToUpload <- c("db_profile_results","db_metadata","db_cdm_source","dqd_checkresults","dqd_overview")
} else {
	tablesToUpload <- c("db_profile_results","db_metadata","db_cdm_source")
}

# create the connectionDetails for the database where the results should be uploaded. It is likely this will be different than the database where the dbProfile was run

connectionDetails <- DatabaseConnector::createConnectionDetails(
	dbms = "postgresql",
	server = "localhost/synthea",
	user = "postgres",
	password = "postgres"
)

conn <- DatabaseConnector::connect(connectionDetails)

# When the schema is empty, use createTable = TRUE
for (tableName in tablesToUpload) {
	DatabaseConnector::insertTable(
		connection        = conn,
		tableName         = tableName,
		databaseSchema    = "cdm_54_results",
		data              = eval(parse(text=tableName)),
		dropTableIfExists = FALSE,
		createTable       = TRUE,
		tempTable         = FALSE,
		progressBar       = TRUE)
}

DatabaseConnector::disconnect(conn)
```

