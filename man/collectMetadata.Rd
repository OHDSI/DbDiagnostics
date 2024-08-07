% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collectMetadata.R
\name{collectMetadata}
\alias{collectMetadata}
\title{collectMetadata}
\usage{
collectMetadata(
  outputFolder,
  outputFileName = "metadata",
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
  refreshTime = NA
)
}
\arguments{
\item{outputFolder}{The output folder location where the csv with the metadata should be saved}

\item{outputFileName}{The name of the file to be written. Default is metadata.csv}

\item{siteName}{The name of the site or institution that owns or licenses the data.}

\item{siteOHDSIParticipation}{Yes/No if the site contributed to an OHDSI study in the past}

\item{siteOHDSIRunPackage}{Yes/No if site has someone who can run and/or debug an OHDSI study package}

\item{dataFullName}{The full name of the database}

\item{dataShortName}{The short name or nickname of the database}

\item{dataContactName}{The name of the person who should be contacted in the event this database is identified as a good candidate for a study}

\item{dataContactEmail}{The email address of the person who should be contacted in the event this database is identified as a good candidate for a study}

\item{dataDoiType}{The type of data object identifier (DOI) the database has. Options are "DOI", "CURIE", "ARK", "Other",}

\item{governanceTime}{How long (in weeks) it typically takes to receive approval to run a study on this database}

\item{dataProvenance}{The type(s) of data that are present in your database. Options are "Electronic Health Records", "Administrative Claims", "Disease-specific Registry", "Wearable or Sensor Data", "Other".}

\item{refreshTime}{How often the data are refreshed}
}
\description{
collectMetadata
}
