# @file executeDbProfile.R
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

#' executeDbProfile
#'
#' @param connectionDetails         A connectionDetails object for connecting to the CDM database
#' @param cdmDatabaseSchema         The fully qualified database name of the CDM schema
#' @param resultsDatabaseSchema     The fully qualified database name of the results schema where achilles results are held
#' @param writeTo										The fully qualified schema you have write access to. This will be used to store any missing analyses that need to be run.
#' @param vocabDatabaseSchema       The fully qualified database name of the vocabulary schema
#' @param cdmSourceName             A string containing the name of the database
#' @param siteName									The name of the site or institution that owns or licenses the data.
#' @param siteOHDSIParticipation		Yes/No if the site contributed to an OHDSI study in the past
#' @param siteOHDSIRunPackage 			Yes/No if site has someone who can run and/or debug an OHDSI study package
#' @param dataFullName							The full name of the database
#' @param dataShortName 						The short name or nickname of the database
#' @param dataContactName 					The name of the person who should be contacted in the event this database is identified as a good candidate for a study
#' @param dataContactEmail					The email address of the person who should be contacted in the event this database is identified as a good candidate for a study
#' @param dataDoiType 							The type of data object identifier (DOI) the database has. Options are "DOI", "CURIE", "ARK", "Other",
#' @param governanceTime						How long (in weeks) it typically takes to receive approval to run a study on this database
#' @param dataProvenance						The type(s) of data that are present in your database. Options are "Electronic Health Records", "Administrative Claims", "Disease-specific Registry", "Wearable or Sensor Data", "Other".
#' @param refreshTime 							How often the data are refreshed
#' @param outputFolder              Results will be written to this directory, Default = getwd()
#' @param cdmVersion                The CDM version to target for the data source. Default = "5.3"
#' @param appendAchilles            Specify if any missing achilles analyses should be appended to existing achilles results tables. This requires write access to the schema specified in the `resultsDatabaseSchema`. Default is FALSE meaning any missing analyses will be written to the schema specified in the `writeTo` parameter.
#' @param minCellCount              Minimum cell count to allow in analyses. Default = 0
#' @param roundTo										Specify whether to round to the 10s or 100s place. Valid inputs are 10 or 100, default is 10.
#' @param excludedConcepts					A vector of concepts that should not be included in the final file "achilles_results_augmented.csv".
#'																	This is the file that is shared with as part of the OHDSI network initiative.
#' @param addDQD										Specify if DQD should be run. Default = TRUE
#' @param tableCheckThresholds      OPTIONAL Location of the custom threshold file for the DQD table checks. In absence of this parameter the default thresholds are used.
#' @param fieldCheckThresholds      OPTIONAL Location of the custom threshold file for the DQD field checks. In absence of this parameter the default thresholds are used.
#' @param conceptCheckThresholds    OPTIONAL Location of the custom threshold file for the DQD concept checks. In absence of this parameter the default thresholds are used.
#'
#' @import DataQualityDashboard Achilles DatabaseConnector SqlRender magrittr dplyr
#' @importFrom utils read.csv write.csv zip
#' @importFrom lubridate ymd
#' @export
executeDbProfile <- function(connectionDetails,
														 cdmDatabaseSchema,
														 resultsDatabaseSchema,
														 writeTo,
														 vocabDatabaseSchema,
														 cdmSourceName = NA,
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
														 conceptCheckThresholds = "default") {

	# Set the name of the output file --------------------------------
	if (!dir.exists(outputFolder)) {
		dir.create(path = outputFolder, recursive = TRUE)
	}

	# Check inputs -------------------------------------
	if(is.na(cdmSourceName)|cdmSourceName == ""){
		stop("cdmSourceName cannot be empty, it must be a string containing the name of the database")
	}

	if(!roundTo %in% c(10,100)){
		stop(paste0("The rounding value must be either 10 or 100. You inputted ", roundTo))
	}

	## Create the sourceKey which will become the name of the folder where the results will be stored
	sourceKey <- gsub(" ","_",cdmSourceName)
	outputCdmFolder <- paste0(outputFolder,"/",sourceKey)

	if (!dir.exists(outputCdmFolder)) {
		dir.create(path = outputCdmFolder, recursive = TRUE)
	}


	## Check CDM_SOURCE table ---------------------------------
		connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
		sql <- SqlRender::render(
			sql = "select * from @cdmDatabaseSchema.cdm_source;",
			cdmDatabaseSchema = cdmDatabaseSchema
		)
		sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)
		cdm_source <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
		if (nrow(cdm_source) < 1) {
			stop(paste0("Please populate the cdm_source table in the ",cdmDatabaseSchema," schema before executing data profile function."))
		}

		DatabaseConnector::disconnect(connection)

	## Create the releaseDateKey and create release folder

		releaseDateKey <- format(lubridate::ymd(cdm_source$cdmReleaseDate[1]), "%Y%m%d")

		outputCdmReleaseFolder <- paste0(outputCdmFolder,"/",releaseDateKey)

		if (!dir.exists(outputCdmReleaseFolder)) {
			dir.create(path = outputCdmReleaseFolder, recursive = TRUE)
		}

	## Get vocabulary version ----------------------------------
		connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
		sql <- SqlRender::render(
			sql = "select vocabulary_version from @vocabDatabaseSchema.vocabulary where vocabulary_id = 'None';",
			vocabDatabaseSchema = vocabDatabaseSchema
		)
		sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)
		vocabVersion <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
		if (is.null(vocabVersion)|is.na(vocabVersion)) {
			stop("Please check your vocabulary tables. The vocabulary version could not be determined")
		}

		on.exit(DatabaseConnector::disconnect(connection))

	## Add vocab version from data to cdm_source
		colnames(vocabVersion) <- "vocabularyVersionFromData"

		cdm_source <- cbind(cdm_source,vocabVersion)

		write.csv(cdm_source,paste0(outputCdmReleaseFolder,"/",sourceKey,"_",releaseDateKey,"_cdm_source.csv"), row.names = FALSE)

	## Collect Metadata
		if(file.exists(paste0(outputCdmFolder,"/",sourceKey,"_metadata.csv"))){

			metadata <- read.csv(paste0(outputCdmFolder,"/",sourceKey,"_metadata.csv"))

			if(is.na(metadata$dataContactName)|is.na(metadata$dataContactEmail)){

				paste0("Please check the metadata file located at ",outputCdmFolder,"/",sourceKey,"_metadata.csv. It is missing either the contact name or email")

			}else{

				print("Metadata file exists and is populated, skipping metadata collection")

			}
		}else{

			collectMetadata(outputFolder = outputCdmFolder,
											outputFileName = paste0(sourceKey,"_metadata"),
											siteName,
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



	## Create zip file name with sourceKey, releaseKey, and timestamp
	startTime <- Sys.time()
	startTimestamp <- format(startTime, "%Y%m%d%H%M%S")

	outputFile <- file.path(outputCdmReleaseFolder, paste0("DbProfileResults_", sourceKey, "_" , releaseDateKey, "_", startTimestamp, ".zip"))
	# ACHILLES ------------------------------------------------

	# The Achilles tables we will look for to see if the analyses have already been run
	achillesTables <- c("ACHILLES_RESULTS", "ACHILLES_RESULTS_DIST")

	# Subset of analyses that will be run (if not already available) and the results returned
	# Helper code to View the descriptions of these analyses:
	# View(subset(Achilles::getAnalysisDetails(),ANALYSIS_ID %in% analysisIds))
	analysisIds <-
		c(1,
			2,
			3,
			4,
			5,
			101,
			105,
			117,
			111,
			112,
			113,
			108,
			200,
			2004,
			1800,
			1801,
			1814,
			1825,
			400,
			401,
			425,
			600,
			601,
			625,
			700,
			701,
			725,
			800,
			801,
			822,
			825,
			900,
			901,
			1822,
			2100,
			2101,
			2125,
			1815)

	## First we will test to see if the Achilles tables already exist.

	resultsTables <- tryCatch(
		expr = {
			conn <- DatabaseConnector::connect(connectionDetails)
			as.data.frame(DatabaseConnector::getTableNames(conn, resultsDatabaseSchema))
		},
		error = function(e) {
			message(
				paste(
					"Results schema does not exist or you do not have access. Please check your parameters"
				)
			)
			message(e)
			resultsTables <- as.data.frame(NA)
			return(resultsTables)
		}
	)

	colnames(resultsTables) <- "resultsTables"

	resultsTables$achillesTables <-
		ifelse(tolower(resultsTables$resultsTables) %in% tolower(achillesTables), 1, 0)

	if (!is.na(resultsTables[1, 1])) {
		if (sum(resultsTables$achillesTables) == length(achillesTables)) {
			#check for both results tables

			writeLines("All results tables present, now checking required analyses and running only missing analyses")

			missingAnalyses <-
				Achilles::listMissingAnalyses(connectionDetails,
																			resultsDatabaseSchema)

			missingAnalyses$requiredAnalyses <-
				ifelse(missingAnalyses$ANALYSIS_ID %in% analysisIds, 1, 0)

			analysesToRun <- subset(missingAnalyses, requiredAnalyses == 1)

			if (nrow(analysesToRun) > 0 ){

				if (appendAchilles){

					writeLines(paste("Running Analyses", analysesToRun$ANALYSIS_ID, "and attempting to append to existing results tables"))

					Achilles::achilles( connectionDetails,
															cdmDatabaseSchema = cdmDatabaseSchema,
															vocabDatabaseSchema = vocabDatabaseSchema,
															createTable = FALSE,
															resultsDatabaseSchema = resultsDatabaseSchema,
															sourceName = cdmSourceName,
															updateGivenAnalysesOnly = TRUE,
															analysisIds = analysesToRun$ANALYSIS_ID,
															cdmVersion = cdmVersion,
															outputFolder = outputFolder
															)

					connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

					# get the achilles results
					sql <- SqlRender::loadRenderTranslateSql("getAchillesResults.sql",
																									 packageName = "DbDiagnostics",
																									 dbms = connectionDetails$dbms,
																									 resultsDatabaseSchema = resultsDatabaseSchema,
																									 analysis_ids = analysisIds,
																									 min_cell_count = minCellCount)

					achillesResults <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)

					# get the achilles results dist
					sql <- SqlRender::loadRenderTranslateSql("getAchillesResultsDist.sql",
																									 packageName = "DbDiagnostics",
																									 dbms = connectionDetails$dbms,
																									 resultsDatabaseSchema = resultsDatabaseSchema,
																									 analysis_ids = analysisIds,
																									 min_cell_count = minCellCount)

					achillesResultsDist <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)


				}else{

					connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

					# get the achilles results
					sql <- SqlRender::loadRenderTranslateSql("getAchillesResults.sql",
																									 packageName = "DbDiagnostics",
																									 dbms = connectionDetails$dbms,
																									 resultsDatabaseSchema = resultsDatabaseSchema,
																									 analysis_ids = analysisIds,
																									 min_cell_count = minCellCount)

					achillesResultsIncomplete <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)

					# get the achilles results dist
					sql <- SqlRender::loadRenderTranslateSql("getAchillesResultsDist.sql",
																									 packageName = "DbDiagnostics",
																									 dbms = connectionDetails$dbms,
																									 resultsDatabaseSchema = resultsDatabaseSchema,
																									 analysis_ids = analysisIds,
																									 min_cell_count = minCellCount)

					achillesResultsDistIncomplete <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)

					writeLines(paste("Running Analyses", analysesToRun$ANALYSIS_ID, "and writing to", writeTo))

					Achilles::achilles(
									connectionDetails,
									cdmDatabaseSchema = cdmDatabaseSchema,
									vocabDatabaseSchema = vocabDatabaseSchema,
									createTable = TRUE,
									resultsDatabaseSchema = writeTo,
									sourceName = cdmSourceName,
									updateGivenAnalysesOnly = TRUE,
									analysisIds = analysesToRun$ANALYSIS_ID,
									cdmVersion = cdmVersion,
									outputFolder = outputFolder
								)

					# get the missing achilles results
					sql <- SqlRender::loadRenderTranslateSql("getAchillesResults.sql",
																									 packageName = "DbDiagnostics",
																									 dbms = connectionDetails$dbms,
																									 resultsDatabaseSchema = writeTo,
																									 analysis_ids = analysisIds,
																									 min_cell_count = minCellCount)

					achillesResultsMissing <-	tryCatch(
																			expr = {
																				DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
																			},
																			error = function(e) {
																				achillesResultsMissing <- data.frame(analysisId  = NA,
																																						 stratum1 = NA,
																																						 stratum2 = NA,
																																						 stratum3 = NA,
																																						 stratum4 = NA,
																																						 stratum5 = NA,
																																						 countValue = NA)
																				return(achillesResultsMissing)
																			}
																		)

					# get the missing achilles results dist
					sql <- SqlRender::loadRenderTranslateSql("getAchillesResultsDist.sql",
																									 packageName = "DbDiagnostics",
																									 dbms = connectionDetails$dbms,
																									 resultsDatabaseSchema = writeTo,
																									 analysis_ids = analysisIds,
																									 min_cell_count = minCellCount)

					achillesResultsDistMissing <- 	tryCatch(
																						expr = {
																							DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
																						},
																						error = function(e) {
																							achillesResultsDistMissing <- data.frame(analysisId  = NA,
																																												 stratum1 = NA,
																																												 stratum2 = NA,
																																												 stratum3 = NA,
																																												 stratum4 = NA,
																																												 stratum5 = NA,
																																												 countValue = NA,
																																											   minValue = NA,
																																											   maxValue = NA,
																																											   avgValue = NA,
																																											   stdevValue = NA,
																																											   medianValue = NA,
																																											   p10Value = NA,
																																											   p25Value = NA,
																																											   p75Value = NA,
																																											   p90Value = NA)
																							return(achillesResultsDistMissing)
																						}
																					)

					achillesResults <- rbind(achillesResultsIncomplete,achillesResultsMissing)

					achillesResultsDist <- rbind(achillesResultsDistIncomplete,achillesResultsDistMissing)

				}
			} else {

			writeLines("All tables and analyses are present, grabbing all results")

				connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

				# get the achilles results
				sql <- SqlRender::loadRenderTranslateSql("getAchillesResults.sql",
																								 packageName = "DbDiagnostics",
																								 dbms = connectionDetails$dbms,
																								 resultsDatabaseSchema = resultsDatabaseSchema,
																								 analysis_ids = analysisIds,
																								 min_cell_count = minCellCount)

				achillesResults <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)

				# get the achilles results dist
				sql <- SqlRender::loadRenderTranslateSql("getAchillesResultsDist.sql",
																								 packageName = "DbDiagnostics",
																								 dbms = connectionDetails$dbms,
																								 resultsDatabaseSchema = resultsDatabaseSchema,
																								 analysis_ids = analysisIds,
																								 min_cell_count = minCellCount)

				achillesResultsDist <- DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)

			}

		} else {
			writeLines(
				paste0("One or more achilles tables are missing, running entire package for the required analyses and regenerating tables. Results will be written to the schema ",writeTo)
			)

				Achilles::achilles(
														connectionDetails,
														cdmDatabaseSchema = cdmDatabaseSchema,
														vocabDatabaseSchema = vocabDatabaseSchema,
														resultsDatabaseSchema = writeTo,
														sourceName = cdmSourceName,
														analysisIds = analysisIds,
														cdmVersion = cdmVersion,
														outputFolder = outputFolder
													)

				connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

				# get the achilles results
				sql <- SqlRender::loadRenderTranslateSql("getAchillesResults.sql",
																								 packageName = "DbDiagnostics",
																								 dbms = connectionDetails$dbms,
																								 resultsDatabaseSchema = writeTo,
																								 analysis_ids = analysisIds,
																								 min_cell_count = minCellCount)

				achillesResults <-	tryCatch(
					expr = {
						DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
					},
					error = function(e) {
						achillesResults <- data.frame(analysisId  = NA,
																								 stratum1 = NA,
																								 stratum2 = NA,
																								 stratum3 = NA,
																								 stratum4 = NA,
																								 stratum5 = NA,
																								 countValue = NA)
						return(achillesResults)
					}
				)

				# get the achilles results dist
				sql <- SqlRender::loadRenderTranslateSql("getAchillesResultsDist.sql",
																								 packageName = "DbDiagnostics",
																								 dbms = connectionDetails$dbms,
																								 resultsDatabaseSchema = writeTo,
																								 analysis_ids = analysisIds,
																								 min_cell_count = minCellCount)

				achillesResultsDist <- 	tryCatch(
					expr = {
						DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
					},
					error = function(e) {
						achillesResultsDist <- data.frame(analysisId  = NA,
																										 stratum1 = NA,
																										 stratum2 = NA,
																										 stratum3 = NA,
																										 stratum4 = NA,
																										 stratum5 = NA,
																										 countValue = NA,
																										 minValue = NA,
																										 maxValue = NA,
																										 avgValue = NA,
																										 stdevValue = NA,
																										 medianValue = NA,
																										 p10Value = NA,
																										 p25Value = NA,
																										 p75Value = NA,
																										 p90Value = NA)
						return(achillesResultsDist)
					}
				)

		}
	} else {
		writeLines(paste0("No Achilles tables detected, running entire package for the required analyses. Results will be written to schema ",writeTo))

		Achilles::achilles(
												connectionDetails,
												cdmDatabaseSchema = cdmDatabaseSchema,
												vocabDatabaseSchema = vocabDatabaseSchema,
												resultsDatabaseSchema = writeTo,
												sourceName = cdmSourceName,
												analysisIds = analysisIds,
												cdmVersion = cdmVersion,
												outputFolder = outputFolder
											)

		connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

		# get the achilles results
		sql <- SqlRender::loadRenderTranslateSql("getAchillesResults.sql",
																						 packageName = "DbDiagnostics",
																						 dbms = connectionDetails$dbms,
																						 resultsDatabaseSchema = writeTo,
																						 analysis_ids = analysisIds,
																						 min_cell_count = minCellCount)

		achillesResults <-	tryCatch(
			expr = {
				DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
			},
			error = function(e) {
				achillesResults <- data.frame(analysisId  = NA,
																			stratum1 = NA,
																			stratum2 = NA,
																			stratum3 = NA,
																			stratum4 = NA,
																			stratum5 = NA,
																			countValue = NA)
				return(achillesResults)
			}
		)

		# get the achilles results dist
		sql <- SqlRender::loadRenderTranslateSql("getAchillesResultsDist.sql",
																						 packageName = "DbDiagnostics",
																						 dbms = connectionDetails$dbms,
																						 resultsDatabaseSchema = writeTo,
																						 analysis_ids = analysisIds,
																						 min_cell_count = minCellCount)

		achillesResultsDist <- 	tryCatch(
			expr = {
				DatabaseConnector::querySql(connection = connection, sql = sql, snakeCaseToCamelCase = TRUE)
			},
			error = function(e) {
				achillesResultsDist <- data.frame(analysisId  = NA,
																					stratum1 = NA,
																					stratum2 = NA,
																					stratum3 = NA,
																					stratum4 = NA,
																					stratum5 = NA,
																					countValue = NA,
																					minValue = NA,
																					maxValue = NA,
																					avgValue = NA,
																					stdevValue = NA,
																					medianValue = NA,
																					p10Value = NA,
																					p25Value = NA,
																					p75Value = NA,
																					p90Value = NA)
				return(achillesResultsDist)
			}
		)

	}

	## Add back any missing analyses since achilles will not write zero results to the database

	achillesAnalysesIncluded <- unique(c(achillesResults$analysisId, achillesResultsDist$analysisId))

	requiredAnalyses <- as.data.frame(analysisIds)

	requiredAnalyses$missingAnalyses <-
		ifelse(requiredAnalyses[,1] %in% achillesAnalysesIncluded, 0, 1)

	analysesToAdd <- subset(requiredAnalyses, missingAnalyses == 1)

	if (nrow(analysesToAdd) > 0){

		for(i in 1:nrow(analysesToAdd)){
			 analysisId <- c(analysesToAdd$analysisIds[i])
			 stratum1 <- c(0)
			 stratum2 <- c(NA)
			 stratum3 <- c(NA)
			 stratum4 <- c(NA)
			 stratum5 <- c(NA)
			 countValue <- c(0)
			 minValue <- c(NA)
			 maxValue <- c(NA)
			 avgValue <- c(NA)
			 stdevValue <- c(NA)
			 medianValue <- c(NA)
			 p10Value <- c(NA)
			 p25Value <- c(NA)
			 p75Value <- c(NA)
			 p90Value <- c(NA)

			 addAnalysesResults <- data.frame(analysisId, stratum1, stratum2, stratum3, stratum4, stratum5, countValue)

			 achillesResults <- rbind(achillesResults,addAnalysesResults)

			 addAnalysesResultsDist <- data.frame(analysisId, stratum1, stratum2, stratum3, stratum4, stratum5, countValue, minValue, maxValue, avgValue,
			 																		 stdevValue, medianValue, p10Value, p25Value, p75Value, p90Value)

			 achillesResultsDist <- rbind(achillesResultsDist,addAnalysesResultsDist)
		}

	 rm(analysisId, stratum1, stratum2, stratum3, stratum4, stratum5, countValue, minValue, maxValue, avgValue,
	 	 stdevValue, medianValue, p10Value, p25Value, p75Value, p90Value)
	}

	# Add vocabulary ancestor information for later processing

	sql <- SqlRender::loadRenderTranslateSql("visitAncestors.sql",
																					 packageName = "DbDiagnostics",
																					 dbms = connectionDetails$dbms,
																					 vocabDatabaseSchema = vocabDatabaseSchema)

	conn <- DatabaseConnector::connect(connectionDetails)

	visitAncestors <- DatabaseConnector::querySql(conn, sql)

	DatabaseConnector::disconnect(conn)

	#### Rounding and exclusions ----------

	if(!is.null(excludedConcepts)){
		achillesResultsExclusions <- achillesResults %>%
			filter(stratum1 %in% as.character(excludedConcepts))

		achillesResultsWithExclusions <- achillesResults %>%
			filter(!stratum1 %in% as.character(excludedConcepts))

		achillesResultsToAddBack <- achillesResultsExclusions %>%
			filter(ANALYSIS_ID == 2004)

		achillesResultsFinalExclusions <- rbind(achillesResultsWithExclusions,achillesResultsToAddBack)
	}else{
		achillesResultsFinalExclusions <- achillesResults
	}

	if(!is.null(excludedConcepts)){
		achillesResultsDistExclusions <- achillesResultsDist %>%
			filter(stratum1 %in% as.character(excludedConcepts))

		achillesResultsDistWithExclusions <- achillesResultsDist %>%
			filter(!stratum1 %in% as.character(excludedConcepts))

		achillesResultsDistFinalExclusions <- achillesResultsDistWithExclusions
	}else{
		achillesResultsDistFinalExclusions <- achillesResultsDist
	}

	roundingInput <- roundTo/10*-1

	achillesResultsRounded <- achillesResultsFinalExclusions %>%
		mutate(countValueRounded = round(countValue,roundingInput))

	achillesResultsDistRounded <- achillesResultsDistFinalExclusions %>%
		mutate(countValueRounded = round(countValue,roundingInput)) %>%
		mutate(avgValueRounded = round(avgValue,2)) %>%
		mutate(stdevValueRounded = round(stdevValue,2))

	achillesResultsRounded$cdmSourceName <- cdmSourceName
	achillesResultsRounded$releaseKey <- paste(sourceKey,releaseDateKey,sep = "_")

	achillesResultsDistRounded$cdmSourceName <- cdmSourceName
	achillesResultsDistRounded$releaseKey <- paste(sourceKey,releaseDateKey,sep = "_")


	achillesResultsFinal <- sqldf::sqldf(
		"SELECT cdmSourceName,
						releaseKey,
					  analysisId,
						stratum1,
						stratum2,
						stratum3,
						stratum4,
						stratum5,
						countValueRounded	as countValue,
						DESCENDANT_CONCEPT_NAME AS visitConceptName,
						VISIT_ANCESTOR_CONCEPT_ID AS visitAncestorConceptId,
						VISIT_ANCESTOR_CONCEPT_NAME as visitAncestorConceptName
			FROM achillesResultsRounded ar
			LEFT JOIN visitAncestors va
				ON ar.stratum1 = va.DESCENDANT_CONCEPT_ID
				AND ar.analysisId = 200"
	)

	achillesResultsDistFinal <- sqldf::sqldf(
		"SELECT cdmSourceName,
		        releaseKey,
		        analysisId,
		        stratum1,
						stratum2,
						stratum3,
						stratum4,
						stratum5,
						countValueRounded	as countValue,
		        minValue,
		        maxValue,
		        avgValueRounded as avgValue,
		        stdevValueRounded as stdevValue,
		        medianValue,
		        p10Value,
		        p25Value,
		        p75Value,
		        p90Value
		 FROM achillesResultsDistRounded"
	)

		# export the new achilles analysis and dist results
	colnames(achillesResultsFinal) <- SqlRender::camelCaseToSnakeCase(colnames(achillesResultsFinal))
	write.csv(x = achillesResultsFinal, file = paste(outputCdmReleaseFolder,"db_profile_results.csv", sep="/"), quote = TRUE, row.names = FALSE)

	colnames(achillesResultsDistFinal) <- SqlRender::camelCaseToSnakeCase(colnames(achillesResultsDistFinal))
	write.csv(x = achillesResultsDistFinal, file = paste(outputCdmReleaseFolder,"db_profile_results_dist.csv", sep="/"), quote = TRUE, row.names = FALSE)

	# start of DQD analysis
	# DQD ----------------------------------

	if(addDQD) {
	checkNames <- c(
		"measurePersonCompleteness",
		"cdmField",
		"measureValueCompleteness",
		"cdmTable",
		"isRequired",
		"cdmDatatype",
		"isPrimaryKey",
		"isForeignKey",
		"fkDomain",
		"fkClass",
		"isStandardValidConcept",
		"standardConceptRecordCompleteness",
		"sourceConceptRecordCompleteness",
		"plausibleValueLow",
		"plausibleValueHigh",
		"plausibleTemporalAfter",
		"plausibleDuringLife"
	)

	tablesToExclude <- c(
		"VISIT_DETAIL",
		"NOTE",
		"NOTE_NLP",
		"SPECIMEN",
		"FACT_RELATIONSHIP",
		"LOCATION",
		"CARE_SITE",
		"PROVIDER",
		"PAYER_PLAN_PERIOD",
		"COST",
		"DOSE_ERA",
		"CONDITION_ERA"
	)

	dqdCdmVersion <- cdmVersion

	if (cdmVersion == "5.3") {
		dqdCdmVersion <- "5.3.1"
	}

	dqResults <- DataQualityDashboard::executeDqChecks(
		connectionDetails,
		cdmDatabaseSchema = cdmDatabaseSchema,
		resultsDatabaseSchema = resultsDatabaseSchema,
		cdmSourceName = cdmSourceName,
		outputFolder = outputCdmReleaseFolder,
		outputFile = paste(sourceKey,releaseDateKey,"dqd_results.json",sep = "_"),
		verboseMode = TRUE,
		writeToTable = FALSE,
		checkNames = checkNames,
		tablesToExclude = tablesToExclude,
		cdmVersion = dqdCdmVersion,
		tableCheckThresholdLoc = tableCheckThresholds,
		fieldCheckThresholdLoc = fieldCheckThresholds,
		conceptCheckThresholdLoc = conceptCheckThresholds
	)

	zip(zipfile = outputFile,
			c(#paste(outputCdmReleaseFolder,"achilles_results.csv",sep = "/"), #####removing for now so there is a choice to filter down the achilles results
				paste(outputCdmReleaseFolder,"db_profile_results.csv", sep = "/"),
				paste(outputCdmReleaseFolder,"db_profile_results_dist.csv", sep = "/"),
			  paste(outputCdmReleaseFolder,paste(sourceKey,releaseDateKey,"DbProfile.json",sep = "_"), sep="/"),
				paste0(outputCdmFolder,"/",sourceKey,"_metadata.csv"),
				paste0(outputCdmReleaseFolder,"/",sourceKey,"_",releaseDateKey,"_cdm_source.csv")),
			extras = '-j')
	} else {

		zip(zipfile = outputFile,
				c(#paste(outputCdmReleaseFolder,"achilles_results.csv",sep = "/"),
					paste(outputCdmReleaseFolder,"db_profile_results.csv", sep = "/"),
					paste(outputCdmReleaseFolder,"db_profile_results_dist.csv", sep = "/"),
					paste0(outputCdmFolder,"/",sourceKey,"_metadata.csv"),
					paste0(outputCdmReleaseFolder,"/",sourceKey,"_",releaseDateKey,"_cdm_source.csv")),
				extras = '-j')

	}

	writeLines(paste0("Final results are now available in: ", outputFile)) # TODO: change to ParallelLogger
}
