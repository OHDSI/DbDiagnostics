# @file execute.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of the DbProfile package
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

#' execute
#'
#' @param connectionDetails         A connectionDetails object for connecting to the CDM database
#' @param cdmDatabaseSchema         The fully qualified database name of the CDM schema
#' @param resultsDatabaseSchema     The fully qualified database name of the results schema
#' @param vocabDatabaseSchema       The fully qualified database name of the vocabulary schema
#' @param cdmSourceName             The name of the CDM data source
#' @param outputFolder              Results will be written to this directory, Default = getwd()
#' @param cdmVersion                The CDM version to target for the data source. Default = "5.3"
#' @param overwriteAchilles         Specify if existing achilles results tables should be overwritten, Default=FALSE
#' @param minCellCount              Minimum cell count to allow in analyses. Default = 0
#' @param tableCheckThresholds      OPTIONAL Location of the custom threshold file for the DQD table checks. In absence of this parameter the default thresholds are used.
#' @param fieldCheckThresholds      OPTIONAL Location of the custom threshold file for the DQD field checks. In absence of this parameter the default thresholds are used.
#' @param conceptCheckThresholds    OPTIONAL Location of the custom threshold file for the DQD concept checks. In absence of this parameter the default thresholds are used.
#'
#' @return
#'
#' @import DataQualityDashboard Achilles DatabaseConnector SqlRender
#'
#' @export
execute <- function(connectionDetails,
										cdmDatabaseSchema,
										resultsDatabaseSchema,
										vocabDatabaseSchema,
										cdmSourceName,
										outputFolder = getwd(),
										cdmVersion = "5.3",
										overwriteAchilles = FALSE,
										minCellCount = 5,
										tableCheckThresholds = "default",
										fieldCheckThresholds = "default",
										conceptCheckThresholds = "default") {
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
			117,
			111,
			113,
			108,
			200,
			2004,
			1801,
			1814,
			401,
			601,
			701,
			801,
			2101,
			1815)

	if (!dir.exists(outputFolder)) {
		dir.create(path = outputFolder, recursive = TRUE)
	}

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
		ifelse(resultsTables$resultsTables %in% achillesTables, 1, 0)

	if (!is.na(resultsTables[1, 1])) {
		if (sum(resultsTables$achillesTables) == length(achillesTables)) {
			#check for both results tables

			writeLines("All achilles tables present, now checking required analyses")

			missingAnalyses <-
				Achilles::listMissingAnalyses(connectionDetails,
																			resultsDatabaseSchema)

			missingAnalyses$requiredAnalyses <-
				ifelse(missingAnalyses$ANALYSIS_ID %in% analysisIds, 1, 0)

			analysesToRun <- subset(missingAnalyses, requiredAnalyses == 1)

			writeLines(paste("Running Analyses", analysesToRun$ANALYSIS_ID))

			Achilles::achilles(
				connectionDetails,
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
		} else if (overwrite) {
			writeLines(
				"One or more achilles tables are missing, running entire package for the required analyses and regenerating tables"
			)

			Achilles::achilles(
				connectionDetails,
				cdmDatabaseSchema = cdmDatabaseSchema,
				vocabDatabaseSchema = vocabDatabaseSchema,
				resultsDatabaseSchema = resultsDatabaseSchema,
				sourceName = cdmSourceName,
				analysisIds = analysisIds,
				cdmVersion = cdmVersion,
				outputFolder = outputFolder
			)

		} else if (!overwrite) {
			tryCatch(
				expr = {
					writeLines(
						"One or more achilles tables are missing, attempting to update analyses without regenerating tables"
					)

					missingAnalyses <-
						Achilles::listMissingAnalyses(connectionDetails,
																					resultsDatabaseSchema)

					missingAnalyses$requiredAnalyses <-
						ifelse(missingAnalyses$ANALYSIS_ID %in% analysisIds, 1, 0)

					analysesToRun <- subset(missingAnalyses, requiredAnalyses == 1)

					writeLines(paste("Running Analyses", analysesToRun$ANALYSIS_ID))

					Achilles::achilles(
						connectionDetails,
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
				},
				error = function(e) {
					message(
						paste(
							"An attempt was made to update missing analyses but the table could not be overwritten. Try setting overwrite = TRUE. Any results exported are most likely incomplete."
						)
					)
					message(e)
				}
			)
		}
	} else {
		writeLines("No Achilles tables detected, running entire package for the required analyses")

		Achilles::achilles(
			connectionDetails,
			cdmDatabaseSchema = cdmDatabaseSchema,
			vocabDatabaseSchema = vocabDatabaseSchema,
			resultsDatabaseSchema = resultsDatabaseSchema,
			sourceName = cdmSourceName,
			analysisIds = analysisIds,
			cdmVersion = cdmVersion,
			outputFolder = outputFolder
		)
	}

	Achilles::exportResultsToCSV(
		connectionDetails,
		resultsDatabaseSchema = resultsDatabaseSchema,
		analysisIds = analysisIds,
		minCellCount = minCellCount,
		exportFolder = outputFolder
	)

	checkNames <- c(
		"measurePersonCompleteness",
		"cdmField",
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
		"DEVICE_EXPOSURE",
		"VISIT_DETAIL",
		"NOTE",
		"NOTE_NLP",
		"OBSERVATION",
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

	if (cdmVersion == "5.3") {
		dqdCdmVersion <- "5.3.1"
	}

	dqResults <- DataQualityDashboard::executeDqChecks(
		connectionDetails,
		cdmDatabaseSchema = cdmDatabaseSchema,
		resultsDatabaseSchema = resultsDatabaseSchema,
		cdmSourceName = cdmSourceName,
		outputFolder = outputFolder,
		verboseMode = TRUE,
		writeToTable = FALSE,
		checkNames = checkNames,
		tablesToExclude = tablesToExclude,
		cdmVersion = dqdCdmVersion,
		tableCheckThresholdLoc = tableCheckThresholds,
		fieldCheckThresholdLoc = fieldCheckThresholds,
		conceptCheckThresholdLoc = conceptCheckThresholds
	)
}
