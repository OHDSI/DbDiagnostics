# DbDiagnostics README

The `executeDbProfile` function in this package relies on the [Achilles](https://github.com/ohdsi/Achilles) and [DataQualityDashboard](https://github.com/ohdsi/DataQualityDashboard) packages to run a subset of characterization and data quality analyses, along with metadata about the database. This information is collectively referred to as the database profile. This profile will be used to determine if a database has the necessary elements required to run a study. 

It works by connecting to a database through a connectionDetails object created by the [DatabaseConnector](http://ohdsi.github.io/DatabaseConnector/articles/Connecting.html) package. It will first look for metadata about the database, either in the parameters of the function or in a csv file in the output folder. If neither are present the function will ask the user a series of questions in the form of pop-ups and save this information in a csv file to ship along with the output. It will then check to see if Achilles results are already present. If so, it will export those results. If not, it will run the required Achilles analyses and then export. Then, it will run a set of DataQualityDashboard checks and export those results as well.

Once the results are generated they are then loaded to a separate results schema. The `executeDbDiagnostics` function will take in a list of analysis settings to compare against the dbProfile results to determine if a database is eligible to run a particular analysis.

## 1. Run DbDiagnostics::executeDbProfile

Please see [this article](https://ohdsi.github.io/DbDiagnostics/articles/RunAndUploadDbProfile.html) for detailed instruction on running the database profile.

## 1.1 Input the required metadata for the database profile

You can either include a metadata.csv file in the root output location, input the values as parameters to the `executeDbProfile` function or answer the pop-ups as the function is running. Information on these options can be found [here](https://ohdsi.github.io/DbDiagnostics/articles/Metadata.html)

## 2. Upload results to a local database and/or send to the OHDSI network central sftp server

## 3. Use DbDiagnostics to determine if your database(s) have the information required to answer particular clinical questions

## 3.1 Create the DbDiagnostics settings objects

## 3.2 Run the `executeDbDiagnostics` function

## 4. Investigate the dbDiagnostics results

## 4.1 Set up a local instance of the shiny application to visualize the results
