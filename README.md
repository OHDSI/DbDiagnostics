# DbDiagnostics README

This package is built to do two things:

1. Create a profile of an individual database that has been converted to the OMOP Common Data Model. This profile consists of aggregated summary statistics and data quality results. 
2. Allow a user to use the profile of a database to determine if that database has the elements necessary required to answer clinical questions of interest.
	
The instructions below contain an overview of how to generate the database profile and then run the data diagnostics on one or more databases.  

## Database Profile
The `executeDbProfile` function in this package relies on the [Achilles](https://github.com/ohdsi/Achilles) and [DataQualityDashboard](https://github.com/ohdsi/DataQualityDashboard) packages to run a subset of characterization and data quality analyses, along with metadata about the database. This information is collectively referred to as the database profile. It works by connecting to a database through a connectionDetails object created by the [DatabaseConnector](http://ohdsi.github.io/DatabaseConnector/articles/Connecting.html) package. It will first look for metadata about the database, either in the parameters of the function or in a csv file in the output folder. If neither are present the function will ask the user a series of questions in the form of pop-ups and save this information in a csv file to ship along with the output. It will then check to see if Achilles results are already present. If so, it will export those results. If not, it will run the required Achilles analyses and then export. Then, it will run a set of DataQualityDashboard checks and export those results as well.

Once the results are generated they are then loaded to a separate results schema. The `executeDbDiagnostics` function will take in a list of analysis settings to compare against the dbProfile results to determine if a database is eligible to run a particular analysis.

## 1. Run DbDiagnostics::executeDbProfile

Please see [this article](https://ohdsi.github.io/DbDiagnostics/articles/RunAndUploadDbProfile.html) for detailed instruction on running the database profile.

### 1.1 Input the required metadata for the database profile

You can either include a metadata.csv file in the root output location, input the values as parameters to the `executeDbProfile` function or answer the pop-ups as the function is running. Information on these options can be found [here](https://ohdsi.github.io/DbDiagnostics/articles/Metadata.html)

## 2. Upload results to a local database

Once you have your database profile, you will want to put it somewhere that everyone can access. In an institution with multiple databases, it is best to upload ALL profiles for ALL databases to one table on one central server. That way, when anyone wants to run data diagnostics for their questions it is easy to connect once and get all information. The output is automatically stratified on `cdmSourceName` so you don't need to worry about collision. Below is an example for how you can read in the results generated by `executeDbProfile` and upload to a postgres instance. 

Information on how to do this can be found [here](file:///Users/clairblacketer/Documents/GitHub/DbDiagnostics/docs/articles/RunAndUploadDbProfile.html#upload-dbprofile-results-to-a-local-schema). 
 
## 3. Use DbDiagnostics to determine if your database(s) have the information required to answer particular clinical questions

Once you have your profile data uploaded to a central location, you can now use the `executeDbDiagnostics` function to determine if your database(s) satisfy the criteria for clinical research questions. 

### 3.1 Create the DbDiagnostics settings objects

The first thing you will need to do is create a settings object with the specified inputs using the `createDataDiagnosticsSettings` function. This will put all the necessary items for the research question into a list to be used by the `executeDbDiagnostics` function. Please see [this article](https://ohdsi.github.io/DbDiagnostics/articles/DataDiagnosticsSettings.html) for how to put together the settings and what the options are. You should end with an object that is a list of settings lists, one for each analysis you want to perform. 

### 3.2 Run the `executeDbDiagnostics` function

Call the function, passing in the list of analysis settings created in the above step.

``` r
dbProfileConnectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "",
                                                                         user = ,
                                                                         password = ,
                                                                         server = "your server",
                                                                         port = ,
                                                                         extraSettings = )

dbDiagnosticResults <- DbDiagnostics::executeDbDiagnostics(
												connectionDetails = dbProfileConnectionDetails,
										    resultsDatabaseSchema = "",
										    resultsTableName = "",
										    outputFolder = outputFolder,
										    dataDiagnosticsSettingsList = settingsList)

```

This will write an output csv file and a summary csv file with all results. 


## 4. Investigate the dbDiagnostics results

The summary csv file will show each analysis and each database available. The column 'total_fails' give a number of the total required elements for an analysis that are unavailable in each database. Any database with a 'total_fails' value of zero indicates that the database is a potential candidate to run the full study as specified in the analysis settings. Any value in the 'total_fails' column >= 1 indicates that the database is not a good candidated to run the full study. The additional columns in the summary file detail the exact items in the settings list that did not pass diagnostics. The results csv file provides all information including the percentage of people in the database that meet each individual criteria in the settings list and what thresholds were used to determine failures. 
