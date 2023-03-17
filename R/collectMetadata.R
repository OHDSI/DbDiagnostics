# @file collectMetadata.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of the DbDiagnostics package
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' collectMetadata
#'
#' @param outputFolder The output folder location where the csv with the metadata should be saved
#' @param outputFileName The name of the file to be written. Default is metadata.csv
#' @param siteName The name of the site or institution that owns or licenses the data.
#' @param siteOHDSIParticipation Yes/No if the site contributed to an OHDSI study in the past
#' @param siteOHDSIRunPackage Yes/No if site has someone who can run and/or debug an OHDSI study package
#' @param dataFullName The full name of the database
#' @param dataShortName The short name or nickname of the database
#' @param dataContactName The *name* of the person who should be contacted in the event this database is identified as a good candidate for a study
#' @param dataContactemail The *email address* of the person who should be contacted in the event this database is identified as a good candidate for a study
#' @param dataDoiType The type of data object identifier (DOI) the database has. Options are "DOI", "CURIE", "ARK", "Other",
#' @param governanceTime How long (in weeks) it typically takes to receive approval to run a study on this database
#' @param dataProvenance The type(s) of data that are present in your database. Options are "Electronic Health Records", "Administrative Claims", "Disease-specific Registry", "Wearable or Sensor Data", "Other".
#' @param refreshTime How often the data are refreshed
#'
#' @import svDialogs
#'
#' @export

collectMetadata <- function(outputFolder,
														outputFileName = "metadata",
														siteName = NA,
														siteOHDSIParticipation = NA,
														siteOHDSIRunPackage = NA,
														dataFullName = NA,
														dataShortName = NA,
														dataContactName = NA,
														dataContactEmail = NA,
														dataDoiType = NA,
														governanceTime = NA,
														dataProvenance = NA,
														refreshTime = NA){

if(all(is.na(c(siteName,
						 siteOHDSIParticipation,
						 siteOHDSIRunPackage,
						 dataFullName,
						 dataShortName,
						 dataContactName,
						 dataContactEmail,
						 dataDoiType,
						 governanceTime,
						 dataProvenance,
						 refreshTime)))) {

		# Site information ----------------------------------
		svDialogs::dlgMessage(
			message = "The next dialog boxes will ask you a series of questions about the site or institution that owns or licenses database you are currently profiling. Please answer to the best of your knowledge",
			type = c("ok")
		)

		siteName <- svDialogs::dlgInput(
			message = "What is the name of the site or institution that owns or licenses the data?",
			default = ""
		)$res

		siteOHDSIParticipation <- svDialogs::dlgMessage(
			message = "Has your site contributed to an OHDSI study in the past?",
			type = c("yesno")
		)$res

		siteOHDSIRunPackage <- svDialogs::dlgMessage(
			message = "Is there someone at your site who can run and/or debug an OHDSI study package?",
			type = c("yesno")
		)$res

		metadata <- cbind(siteName,siteOHDSIParticipation,siteOHDSIRunPackage)
		# Database Information --------------------------------

		svDialogs::dlgMessage(
			message = "The next dialog boxes will ask you a series of questions about the database you are currently profiling. Please answer to the best of your knowledge",
			type = c("ok")
		)

		dataFullName <- svDialogs::dlgInput(
			message = "What is the full name of the database?",
			default = ""
		)$res

		hasShortName <- svDialogs::dlgMessage(
			message = "Does your database have a short name or nickname that can aid in identification?",
			type = c("yesno")
		)$res

		if(hasShortName == 'yes'){
			dataShortName <- svDialogs::dlgInput(
				message = "What is the short name or nickname of the database?",
				default = ""
			)$res
		}else{
			dataShortName <- ""
		}

		dataContactName <- svDialogs::dlgInput(
			message = "What is the *name* of the person who should be contacted in the event this database is identified as a good candidate for a study?",
			default = ""
		)$res

		dataContactEmail <- svDialogs::dlgInput(
			message = "What is the *email address* of the person who should be contacted in the event this database is identified as a good candidate for a study?",
			default = ""
		)$res

		hasDoi <- svDialogs::dlgMessage(
			message = "Does your database have a digital object identifier (DOI) that can aid in identification?",
			type = c("yesno")
		)$res

		if(hasDoi == 'yes'){
			dataDoiType <- svDialogs::dlg_list(
				choices = c("DOI", "CURIE", "ARK", "Other"),
				multiple = TRUE,
				title = "Choose DOI Type"
			)$res
		}else{
			dataDoiType <- ""
		}

		governanceTime <- svDialogs::dlgInput(
			message = "How long (in weeks) does it typically take to receive approval to run a study on this database?",
			default = ""
		)$res


		svDialogs::dlgMessage(
			message = "In the next dialog box please select one or more types of data that are present in your database",
			type = c("ok")
		)

		dataProvenance <- svDialogs::dlg_list(
			choices = c("Electronic Health Records", "Administrative Claims", "Disease-specific Registry", "Wearable or Sensor Data", "Other"),
			multiple = TRUE
		)$res

		refreshTime <- svDialogs::dlgInput(
			message = "How often are the data refreshed?",
			default = ""
		)$res

		metadata <- cbind(metadata,
											dataFullName,
											dataShortName,
											dataContactName,
											dataContactEmail,
											dataDoiType,
											governanceTime,
											dataProvenance,
											refreshTime
											)
}else{

	if(is.na(dataContactName)|is.na(dataContactEmail)){
		stop("Either the data contact name or data contact email was not filled in. Please fill in these fields and re-run")
	}else{


	metadata <- cbind(siteName,
										siteOHDSIParticipation,
										siteOHDSIRunPackage,
										dataFullName,
										dataShortName,
										dataContactName,
										dataContactEmail,
										dataDoiType,
										governanceTime,
										dataProvenance,
										refreshTime)

	}

}

write.csv(metadata,paste0(outputFolder,"/",outputFileName,".csv"), row.names = FALSE)
}

