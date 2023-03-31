---
title: "Metadata"
output: html_document
date: "2023-03-31"
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{Metadata}
  %\usepackage[UTF-8]{inputenc}
---

This document describes how metadata is compiled and reported when the `executeDbProfile` function is run. There are three ways to enter the necessary database metadata:

- By answering the questions that pop-up to the user as `executeDbProfile` is running
- By including a metadata csv file in the output folder
- By passing values to the metadata parameters when calling the `executeDbProfile` function
	
### 1. Answer the questions that pop-up to the user as `executeDbProfile` is running

If the user does not pass any values to the metadata parameters and if the metadata csv file is missing from the output folder, the function will ask the user the series of following questions. 

- What is the name of the site or institution that owns or licenses the data?
- Has your site contributed to an OHDSI study in the past?
- Is there someone at your site who can run and/or debug an OHDSI study package?
- What is the full name of the database?
- What is the short name or nickname of the database?
- What is the *name* of the person who should be contacted in the event this database is identified as a good candidate for a study?
- What is the *email address* of the person who should be contacted in the event this database is identified as a good candidate for a study?
- Does your database have a digital object identifier (DOI) that can aid in identification?
- How long (in weeks) does it typically take to receive approval to run a study on this database?
- What types of data are present in your database?
- How often are the data refreshed?
	
Once the questions are answered once for a database, the package will save a csv file with this information so the user does not need to answer them again. As long as the same `cdmSourceName` is used each time the `executeDbProfile` function is run, it will find the metadata file and include it in the output zip file.

### 2. Include a metadata csv file in the output folder

The function will create an folder in the specified output folder labelled with the the `cdmSourceName` parameter that is passed in. If there is a csv file in that location with the name <cdmSourceName>.metadata.csv with the proper structure, the function will not ask the user the pop-up questions and instead include the information in the csv file in the output zip file.

### 3. passing values to the metadata parameters when calling the `executeDbProfile` function

There are parameters in the function that allow you to pass in the metadata programmatically. These are listed below.

**siteName**: The name of the site or institution that owns or licenses the data.

**siteOHDSIParticipation**: Yes/No if the site contributed to an OHDSI study in the past

**siteOHDSIRunPackage**: Yes/No if site has someone who can run and/or debug an OHDSI study package

**dataFullName**: The full name of the database

**dataShortName**: The short name or nickname of the database

**dataContactName**: The *name* of the person who should be contacted in the event this database is identified as a good candidate for a study

**dataContactemail**: The *email address* of the person who should be contacted in the event this database is identified as a good candidate for a study

**dataDoiType**: The type of data object identifier (DOI) the database has. Options are "DOI", "CURIE", "ARK", "Other"

**governanceTime**: How long (in weeks) it typically takes to receive approval to run a study on this database

**dataProvenance**: The type(s) of data that are present in your database. Options are "Electronic Health Records", "Administrative Claims", "Disease-specific Registry", "Wearable or Sensor Data", "Other"

**refreshTime**: How often the data are refreshed



