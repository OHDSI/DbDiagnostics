# This script is an example showing how to run Data Diagnostics to determine if given
# databases should be used in an analysis. This code assumes that DataDiagnostics::executeDbProfile
# has been run for each database and those results have been pushed to a results table in a database schema.

# NOTE: the executeDbDiagnostics function also assumes that the executeDbProfile results
# for ALL databases are in one table with two database identifiers.
#
# db_id is a composite identifier that combines the CDM_SOURCE.cdm_source_name and CDM_SOURCE.cdm_release_date with a "-" between.
# database_id is a hashed value to identify individual database releases


## Read in csv with concepts for target, comparator, and outcome this example uses
## the csv that ATLAS exports listing included concepts in a concept set.

diagnosticsConcepts <- read.csv("D://input//includedConcepts.csv", stringsAsFactors = FALSE)

targetConcepts <- diagnosticsConcepts$Concept.ID[diagnosticsConcepts$Concept.Set.ID == 0]

## create the data diagnostics settings to be passed to the executeDbDiagnostics function. Note you can also
## specify comparator and outcome names and concepts.

ddSettings <- DbDiagnostics::createDataDiagnosticsSettings(analysisName = "gynecomastia",
																													 minAge = 6,
																													 maxAge = 12,
																													 studyStartDate = 200706,
																													 studyEndDate = 202201,
																													 requiredDurationDays = 365,
																													 requiredDomains = c("condition", "drug"),
																													 targetName = "gynecomastia",
																													 targetConceptIds = targetConcepts)
)

## Create the connection details for the database where the dbProfile results are held

dbProfileConnectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
																																				 user = "user",
																																				 password = "password",
																																				 server = "server_address",
																																				 port =  ,
																																				 extraSettings = "")

## Run data diagnostics. The results will be exported as a csv to the output folder and returned as a dataframe in your R session.

dbDiagnosticResults <- DbDiagnostics::executeDbDiagnostics(connectionDetails = dbProfileConnectionDetails,
																													 resultsDatabaseSchema = "results_schema_name",
																													 resultsTableName = "results_table_name",
																													 outputFolder = "D:\\output",
																													 dataDiagnosticsSettings = ddSettings)
