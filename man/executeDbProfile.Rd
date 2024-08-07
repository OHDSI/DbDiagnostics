% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/executeDbProfile.R
\name{executeDbProfile}
\alias{executeDbProfile}
\title{executeDbProfile}
\usage{
executeDbProfile(
  connectionDetails,
  cdmDatabaseSchema,
  resultsDatabaseSchema,
  writeTo,
  vocabDatabaseSchema,
  cdmSourceName = NA,
  siteName = NA,
  siteOHDSIParticipation = NA,
  siteOHDSIRunPackage = NA,
  siteSponsoredStudy = NA,
  dataFullName = NA,
  dataShortName = NA,
  dataContactName = NA,
  dataContactEmail = NA,
  dataDoiType = NA,
  governanceTime = NA,
  dataProvenance = NA,
  refreshTime = NA,
  outputFolder = getwd(),
  cdmVersion = "5.3",
  appendAchilles = FALSE,
  minCellCount = 5,
  roundTo = 10,
  excludedConcepts = c(),
  addDQD = FALSE,
  tableCheckThresholds = "default",
  fieldCheckThresholds = "default",
  conceptCheckThresholds = "default"
)
}
\arguments{
\item{connectionDetails}{A connectionDetails object for connecting to the CDM database}

\item{cdmDatabaseSchema}{The fully qualified database name of the CDM schema}

\item{resultsDatabaseSchema}{The fully qualified database name of the results schema where achilles results are held}

\item{writeTo}{The fully qualified schema you have write access to. This will be used to store any missing analyses that need to be run.}

\item{vocabDatabaseSchema}{The fully qualified database name of the vocabulary schema}

\item{cdmSourceName}{A string containing the name of the database}

\item{siteName}{The name of the site or institution that owns or licenses the data.}

\item{siteOHDSIParticipation}{Yes/No if the site contributed to an OHDSI study in the past}

\item{siteOHDSIRunPackage}{Yes/No if site has someone who can run and/or debug an OHDSI study package}

\item{siteSponsoredStudy}{Yes/No if site is interested in participating in sponsored studies}

\item{dataFullName}{The full name of the database}

\item{dataShortName}{The short name or nickname of the database}

\item{dataContactName}{The name of the person who should be contacted in the event this database is identified as a good candidate for a study}

\item{dataContactEmail}{The email address of the person who should be contacted in the event this database is identified as a good candidate for a study}

\item{dataDoiType}{The type of data object identifier (DOI) the database has. Options are "DOI", "CURIE", "ARK", "Other",}

\item{governanceTime}{How long (in weeks) it typically takes to receive approval to run a study on this database}

\item{dataProvenance}{The type(s) of data that are present in your database. Options are "Electronic Health Records", "Administrative Claims", "Disease-specific Registry", "Wearable or Sensor Data", "Other".}

\item{refreshTime}{How often the data are refreshed}

\item{outputFolder}{Results will be written to this directory, Default = getwd()}

\item{cdmVersion}{The CDM version to target for the data source. Default = "5.3"}

\item{appendAchilles}{Specify if any missing achilles analyses should be appended to existing achilles results tables. This requires write access to the schema specified in the \code{resultsDatabaseSchema}. Default is FALSE meaning any missing analyses will be written to the schema specified in the \code{writeTo} parameter.}

\item{minCellCount}{Minimum cell count to allow in analyses. Default = 0}

\item{roundTo}{Specify whether to round to the 10s or 100s place. Valid inputs are 10 or 100, default is 10.}

\item{excludedConcepts}{A vector of concepts that should not be included in the final file "achilles_results_augmented.csv".
This is the file that is shared with as part of the OHDSI network initiative.}

\item{addDQD}{Specify if DQD should be run. Default = TRUE}

\item{tableCheckThresholds}{OPTIONAL Location of the custom threshold file for the DQD table checks. In absence of this parameter the default thresholds are used.}

\item{fieldCheckThresholds}{OPTIONAL Location of the custom threshold file for the DQD field checks. In absence of this parameter the default thresholds are used.}

\item{conceptCheckThresholds}{OPTIONAL Location of the custom threshold file for the DQD concept checks. In absence of this parameter the default thresholds are used.}
}
\description{
executeDbProfile
}
