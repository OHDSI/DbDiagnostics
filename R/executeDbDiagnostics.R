# @file executeDbDiagnostics.R
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

#' executeDbDiagnostics
#'
#' @param connectionDetails         	  A connectionDetails object for connecting to the database containing the DbProfile results
#' @param resultsDatabaseSchema     	  The fully qualified database name of the results schema where the DbProfile results are housed. Default is "dp_temp".
#' @param resultsTableName						  The name of the table in the results schema with the DbProfile results. Default is "dp_achilles_results_augmented."
#' @param outputFolder              	  Results will be written to this directory, default = getwd()
#' @param dataDiagnosticsSettingsList		A list of settings objects, each created from DataDiagnostics::createDataDiagnosticsSettings() function and each representing one analysis.
#'
#' @import DataQualityDashboard Achilles DatabaseConnector SqlRender dplyr magrittr
#'
#' @export

executeDbDiagnostics <- function(connectionDetails,
																 resultsDatabaseSchema,
																 resultsTableName,
																 outputFolder = getwd(),
																 dataDiagnosticsSettingsList) {

	# Set up outputFolder

	if (!dir.exists(outputFolder)) {
		dir.create(path = outputFolder, recursive = TRUE)
	}

	# Connect to the results schema to get list of databases included in results table ---------------
	options(scipen = 999)

	conn <- DatabaseConnector::connect(connectionDetails)
	on.exit(DatabaseConnector::disconnect(conn))

	#TODO check the name of the column in the results schema -----------
	sql <- "SELECT DISTINCT CDM_SOURCE_NAME, RELEASE_KEY
        FROM @results_database_schema.@results_table_name"

	rsql <- SqlRender::render(sql = sql,
														results_database_schema = resultsDatabaseSchema,
														results_table_name = resultsTableName)
	tsql <- SqlRender::translate(rsql, connectionDetails$dbms)

	dbNames <- DatabaseConnector::querySql(conn, tsql)

	# Get the most recent release for each database -------------------------------
	# message("Get most recent database release")
	# for(i in 1:nrow(dbNames)){
	# 	dbInfo <- strsplit(dbNames$DB_ID[i], split = "-")
	#
	# 	dbNames$DB_ABBREV[i] <- dbInfo[[1]][[1]]
	# 	dbNames$DB_DATE[i] <- dbInfo[[1]][[2]]
	#
	# 	rm(dbInfo)
	# }
	# rm(i)
	#
	# latestDbs<- dbNames %>%
	# 	group_by(DB_ABBREV) %>%
	# 	slice_max(DB_DATE, n=1)

	dbNum <- nrow(dbNames)

		# Loop through the databases -----------------------------------------------

		for(i in 1:dbNum){

			dbName <- dbNames[i,2]

			message(paste0("Database: ", dbName, " (", i, "/", nrow(dbNames), ")"))

			# Get the dbProfile information for the database
			sql <- "SELECT * FROM @results_database_schema.@results_table_name WHERE RELEASE_KEY = '@databaseName'"

			rsql <- SqlRender::render(sql, databaseName = dbName,
																results_database_schema = resultsDatabaseSchema,
																results_table_name = resultsTableName)

			tsql <- SqlRender::translate(rsql, connectionDetails$dbms)
			dbProfile <- DatabaseConnector::querySql(conn, tsql)

			# Set up the specs for this study/db combination. This is done after getting the dbProfile information because NULL
			# values in the specs get values from the database in order to evaluate them

			for(k in 1:length(dataDiagnosticsSettingsList)){
				# Get the specifications ------------------

				studySpecs <- dataDiagnosticsSettingsList[[k]]

				# TODO ---------------
				# evaluate the specs input
				# look at data types and stop if target concept id is null


				# Get the thresholds for the study -------------------------------------------

				ddThresholds <- read.csv(system.file("csv", "ddThresholds.csv", package = "DbDiagnostics"), stringsAsFactors = FALSE)

				# ID of this individual study
				analysisId <- studySpecs$analysisId

				# Name of this individual study
				analysisName <- studySpecs$analysisName

				message(paste0("   -- Analysis #", analysisId, " - ", analysisName, " (", k, "/", length(dataDiagnosticsSettingsList), ")" ))


			numCriteria <- 0

			# Age
			if(is.null(studySpecs$minAge)){
				minAge <- min(as.integer(dbProfile[which(dbProfile$ANALYSIS_ID == 101),]$STRATUM_1))
			}else{
				minAge <- studySpecs$minAge
				numCriteria <- numCriteria + 1
			}

			if(is.null(studySpecs$maxAge)){
				maxAge <- max(as.integer(dbProfile[which(dbProfile$ANALYSIS_ID == 101),]$STRATUM_1))
			}else{
				maxAge <- studySpecs$maxAge
				numCriteria <- numCriteria + 1
			}

			# Gender
				genderConceptIds <- studySpecs$genderConceptIds # Q - limit to these two or to all genders in the db? Means including 0
				numCriteria <- numCriteria + 1

			# Race
			if(is.null(studySpecs$raceConceptIds)){
				raceConceptIds <- dbProfile %>%
					filter(ANALYSIS_ID == 4) %>%
					select(STRATUM_1) %>%
					.[["STRATUM_1"]]
			}else{
				raceConceptIds <- studySpecs$raceConceptIds
				numCriteria <- numCriteria + 1
			}

			# Ethnicity
			if(is.null(studySpecs$ethnicityConceptIds)){
				ethnicityConceptIds <- dbProfile %>%
					filter(ANALYSIS_ID == 5) %>%
					select(STRATUM_1) %>%
					.[["STRATUM_1"]]
			}else{
				ethnicityConceptIds <- studySpecs$ethnicityConceptIds
				numCriteria <- numCriteria + 1
			}

			#Study Start Date
			if(is.null(studySpecs$studyStartDate)){
				studyStartDate <- min(dbProfile[which(dbProfile$ANALYSIS_ID == 111),]$STRATUM_1)

			}else{
				studyStartDate <- max(as.numeric(studySpecs$studyStartDate), min(dbProfile[which(dbProfile$ANALYSIS_ID == 111),]$STRATUM_1))
				numCriteria <- numCriteria + 1
			}

			#Study End Date
			if(is.null(studySpecs$studyEndDate)){
				studyEndDate <- max(dbProfile[which(dbProfile$ANALYSIS_ID == 111),]$STRATUM_1)

			}else{
				studyEndDate <- min(as.numeric(studySpecs$studyEndDate), max(dbProfile[which(dbProfile$ANALYSIS_ID == 111),]$STRATUM_1))
				numCriteria <- numCriteria + 1
			}

			# Required follow-up time
			requiredDurationDays <- studySpecs$requiredDurationDays
			numCriteria <- numCriteria + 1

			# Required domains
			requiredDomains <- studySpecs$requiredDomains
			numCriteria <- numCriteria + 1

			if("condition" %in% requiredDomains){requiredCondition <- 1}else{requiredCondition <- 0}
			if("drug" %in% requiredDomains){requiredDrug <-1}else{requiredDrug <- 0}
			if("device" %in% requiredDomains){requiredDevice <-1}else{requiredDevice <- 0}
			if("measurement" %in% requiredDomains){requiredMeasurement <-1}else{requiredMeasurement <- 0}
			if("procedure" %in% requiredDomains){requiredProcedure <-1}else{requiredProcedure <- 0}
			if("observation" %in% requiredDomains){requiredObservation <-1}else{requiredObservation <- 0}

			# Desired domains
			desiredDomains <- studySpecs$desiredDomains

			if("condition" %in% desiredDomains){desiredCondition <- 1
				numCriteria <- numCriteria + 1}else{desiredCondition <- 0}
			if("drug" %in% desiredDomains){desiredDrug <-1
				numCriteria <- numCriteria + 1}else{desiredDrug <- 0}
			if("device" %in% desiredDomains){desiredDevice <-1
				numCriteria <- numCriteria + 1}else{desiredDevice <- 0}
			if("measurement" %in% desiredDomains){desiredMeasurement <-1
				numCriteria <- numCriteria + 1}else{desiredMeasurement <- 0}
			if("procedure" %in% desiredDomains){desiredProcedure <-1
				numCriteria <- numCriteria + 1}else{desiredProcedure <- 0}
			if("observation" %in% desiredDomains){desiredObservation <-1
				numCriteria <- numCriteria + 1}else{desiredObservation <- 0}
			if("measurementValues" %in% desiredDomains){desiredMeasurementValues <-1
				numCriteria <- numCriteria + 1}else{desiredMeasurementValues <- 0}
			if("death" %in% desiredDomains){desiredDeath <-1
				numCriteria <- numCriteria + 1}else{desiredDeath <- 0}

			# Required visits
			requiredVisits <- studySpecs$requiredVisits

			if("IP" %in% requiredVisits){requiredIP <- 1}else{requiredIP <- 0}
			if("OP" %in% requiredVisits){requiredOP <-1}else{requiredOP <- 0}
			if("ER" %in% requiredVisits){requiredER <-1}else{requiredER <- 0}

			# Desired visits
			desiredVisits <- studySpecs$desiredVisits

			if("IP" %in% desiredVisits){desiredIP <- 1
			numCriteria <- numCriteria + 1}else{desiredIP <- 0}
			if("OP" %in% desiredVisits){desiredOP <-1
			numCriteria <- numCriteria + 1}else{desiredOP <- 0}
			if("ER" %in% desiredVisits){desiredER <-1
			numCriteria <- numCriteria + 1}else{desiredER <- 0}

			#target
			target <- studySpecs$targetName
			requiredTargetConcepts <- studySpecs$targetConceptIds
			numCriteria <- numCriteria + 1

			#comparator
			if(!is.null(studySpecs$comparatorConceptIds)){
				comparator <- studySpecs$comparatorName
				requiredComparatorConcepts <- studySpecs$comparatorConceptIds
				numCriteria <- numCriteria + 1
			}else{
				requiredComparatorConcepts <- NULL
			}

			#indication
			if(!is.null(studySpecs$indicationConceptIds)){
				indication <- studySpecs$indicationName
				requiredIndicationConcepts <- studySpecs$indicationConceptIds
				numCriteria <- numCriteria + 1
			}else{
				requiredIndicationConcepts <- NULL
			}

			#outcome
			if(!is.null(studySpecs$outcomeConceptIds)){
				outcome <- studySpecs$outcomeName
				requiredOutcomeConcepts <-  studySpecs$outcomeConceptIds
				numCriteria <- numCriteria + 1
			}else{
				requiredOutcomeConcepts <- NULL
			}

			# Get values for each criterion using Achilles analyses pulled in dbProfile

			# Total persons in database --------------

			numPersonsInDb <- dbProfile[which(dbProfile$ANALYSIS_ID == 1),]$COUNT_VALUE

			#Demographics ---------

			 ##Age -----------
			maxYearInDb <- as.integer(substr(max(dbProfile[which(dbProfile$ANALYSIS_ID == 111),]$STRATUM_1),1,4))
			minYearInDb <- as.integer(substr(min(dbProfile[which(dbProfile$ANALYSIS_ID == 111),]$STRATUM_1),1,4))

			minBirthYearNeeded <- minYearInDb - maxAge
			maxBirthYearNeeded <- maxYearInDb - minAge

			personsInBirthYearRange <- dbProfile %>%
				filter(ANALYSIS_ID == 3) %>%
				filter(as.numeric(STRATUM_1) <= maxBirthYearNeeded & as.numeric(STRATUM_1) >= minBirthYearNeeded) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propInAgeRange',
							 spec = case_when(is.null(studySpecs$maxAge) && !is.null(studySpecs$minAge) ~ paste("> age",studySpecs$minAge),
							 								  !is.null(studySpecs$maxAge) && is.null(studySpecs$minAge) ~ paste("< age",studySpecs$maxAge),
							 								  !is.null(studySpecs$maxAge) && !is.null(studySpecs$minAge) ~ paste("age",studySpecs$minAge,"- age",studySpecs$maxAge)),
							 evaluateThreshold = 1)

			personOutput <- personsInBirthYearRange

			# age at First Obs # did people less than maxAge
			personsWithAgeAtFirstObs <- dbProfile %>%
				filter(ANALYSIS_ID == 101) %>%
				filter(as.numeric(STRATUM_1) <= maxAge) %>%
				filter(as.numeric(STRATUM_1) >= minAge) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithAgeAtFirstObs',
							 spec = case_when(is.null(studySpecs$maxAge) && !is.null(studySpecs$minAge) ~ paste("> age",studySpecs$minAge),
							 								 !is.null(studySpecs$maxAge) && is.null(studySpecs$minAge) ~ paste("< age",studySpecs$maxAge),
							 								 !is.null(studySpecs$maxAge) && !is.null(studySpecs$minAge) ~ paste("age",studySpecs$minAge,"- age",studySpecs$maxAge)),
							 evaluateThreshold = 1)

			personOutput <- rbind(personOutput, personsWithAgeAtFirstObs)

			##Gender ----------
			personsWithGenderCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2) %>%
				filter(STRATUM_1 %in% genderConceptIds) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithGenderCriteria',
							 spec = paste(genderConceptIds, collapse = ", "),
							 evaluateThreshold = 1)

			personOutput <- rbind(personOutput, personsWithGenderCriteria)

			##Race -------------
			personsWithRaceCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 4) %>%
				filter(STRATUM_1 %in% raceConceptIds) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithRaceCriteria',
							 spec = paste(raceConceptIds, collapse = ", "),
							 evaluateThreshold = 1)

			personOutput <- rbind(personOutput, personsWithRaceCriteria)

			##Ethnicity ----------
			personsWithEthnicityCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 5) %>%
				filter(STRATUM_1 %in% ethnicityConceptIds) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithEthnicityCriteria',
							 spec = paste(ethnicityConceptIds, collapse = ", "),
							 evaluateThreshold = 1)

			personOutput <- rbind(personOutput, personsWithEthnicityCriteria)

			#Calendar Time ----------
			# This one has output that can't be incorporated with the others based on the calculation

			totalObsPeriods <- sum(dbProfile[which(dbProfile$ANALYSIS_ID == 111),]$COUNT_VALUE)

			avgObsPeriodsPerPerson <- totalObsPeriods/numPersonsInDb

			obsPeriodsWithCalendarStarts <- dbProfile %>%
				filter(ANALYSIS_ID == 111 & STRATUM_1 <= studyEndDate) %>%
				summarise(personsWithCalendarStarts = sum(COUNT_VALUE)/avgObsPeriodsPerPerson) %>%
				mutate(propWithCalendarStarts = personsWithCalendarStarts/numPersonsInDb)

			obsPeriodsWithCalendarEnds <- dbProfile %>%
				filter(ANALYSIS_ID == 112 & STRATUM_1 >= studyStartDate) %>%
				summarise(personsWithCalendarEnds = sum(COUNT_VALUE)/avgObsPeriodsPerPerson) %>%
				mutate(propWithCalendarEnds = personsWithCalendarEnds/numPersonsInDb)

			calendarTime <- (1 - ((1 - obsPeriodsWithCalendarEnds$propWithCalendarEnds) + (1 - obsPeriodsWithCalendarStarts$propWithCalendarStarts)))

			numPersonsWithCalendarTime <- calendarTime*numPersonsInDb

			personsWithCalendarTime <- as.data.frame(cbind("propWithCalendarTime",
																										 numPersonsWithCalendarTime,
																										 calendarTime)) %>%
																 rename("statistic" = "V1",
																 				"value" = "numPersonsWithCalendarTime",
																 			  "proportion" = "calendarTime") %>%
																 mutate(spec = paste(studyStartDate, studyEndDate, sep = "-"),
																 			  evaluateThreshold = 1)

			finalOutput <- personsWithCalendarTime # since this output has the final proportion, it cannot be combined with the person output

			## Longitudinality --------

			num30DayIncrements <- round(requiredDurationDays/30, digits = 0)

			personsWithLongitudinalCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 108) %>%
				filter(STRATUM_1 >= num30DayIncrements) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithLongitudinalCriteria',
							 spec = paste(requiredDurationDays," days"),
							 evaluateThreshold = 1)

			personOutput <- rbind(personOutput, personsWithLongitudinalCriteria)

			#Data Domain Coverage ----------

			bitString <- paste0(requiredCondition,requiredDrug,requiredDevice,requiredMeasurement,0,requiredProcedure,requiredObservation)

			personsWithRequiredDomain <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithRequiredDomain',
							 spec = paste(requiredDomains, collapse = ", "),
							 evaluateThreshold = 1)

			if(nrow(personsWithRequiredDomain) == 0){
				personsWithRequiredDomain[1,]$COUNT_VALUE <- 0
				personsWithRequiredDomain[1,]$statistic <- 'propWithRequiredDomain'
				personsWithRequiredDomain[1,]$spec <- paste(requiredDomains, collapse = ", ")
				personsWithRequiredDomain[1,]$evaluateThreshold <- 1
			}

			personOutput <- rbind(personOutput, personsWithRequiredDomain)

			bitString <- '1000000'

			personsWithConditionCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithConditionCriteria',
							 spec = case_when(desiredCondition == 1 ~ 'Conditions desired',
							 								  desiredCondition == 0 ~ 'Conditions not desired'),
							 evaluateThreshold = desiredCondition)

			if(nrow(personsWithConditionCriteria) == 0){
				personsWithConditionCriteria[1,]$COUNT_VALUE <- 0
				personsWithConditionCriteria[1,]$statistic <- 'propWithConditionCriteria'
				personsWithConditionCriteria <- personsWithConditionCritera %>%
					mutate(spec = case_when(desiredCondition == 1 ~ 'Conditions desired',
																	desiredCondition == 0 ~ 'Conditions not desired'),
								 evaluateThreshold = desiredCondition)

			}

			personOutput <- rbind(personOutput, personsWithConditionCriteria)

			bitString <- '0100000'

			personsWithDrugCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithDrugCriteria',
							 spec = case_when(desiredDrug == 1 ~ 'Drugs desired',
							 								 desiredDrug == 0 ~ 'Drugs not desired'),
							 evaluateThreshold = desiredDrug)

			if(nrow(personsWithDrugCriteria) == 0){
				personsWithDrugCriteria[1,]$COUNT_VALUE <- 0
				personsWithDrugCriteria[1,]$statistic <- 'propWithDrugCriteria'
				personsWithDrugCriteria <- personsWithDrugCriteria %>%
					mutate(spec = case_when(desiredDrug == 1 ~ 'Drugs desired',
																	desiredDrug == 0 ~ 'Drugs not desired'),
								 evaluateThreshold = desiredDrug)
			}

			personOutput <- rbind(personOutput, personsWithDrugCriteria)

			bitString <- '0010000'

			personsWithDeviceCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithDeviceCriteria',
							 spec = case_when(desiredDevice == 1 ~ 'Devices desired',
							 								 desiredDevice == 0 ~ 'Devices not desired'),
							 evaluateThreshold = desiredDevice)

			if(nrow(personsWithDeviceCriteria) == 0){
				personsWithDeviceCriteria[1,]$COUNT_VALUE <- 0
				personsWithDeviceCriteria[1,]$statistic <- 'propWithDeviceCriteria'
				personsWithDeviceCriteria <- personsWithDeviceCriteria %>%
					mutate(spec = case_when(desiredDevice == 1 ~ 'Devices desired',
													 desiredDevice == 0 ~ 'Devices not desired'),
								 evaluateThreshold = desiredDevice)
			}

			personOutput <- rbind(personOutput, personsWithDeviceCriteria)

			bitString <- '0001000'

			personsWithMeasurementCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithMeasurementCriteria',
							 spec = case_when(desiredMeasurement == 1 ~ 'Measurements desired',
							 								  desiredMeasurement == 0 ~ 'Measurements not desired'),
							 evaluateThreshold = desiredMeasurement)

			if(nrow(personsWithMeasurementCriteria) == 0){
				personsWithMeasurementCriteria[1,]$COUNT_VALUE <- 0
				personsWithMeasurementCriteria[1,]$statistic <- 'propWithMeasurementCriteria'
				personsWithMeasurementCriteria <- personsWithMeasurementCriteria %>%
					mutate(spec = case_when(desiredMeasurement == 1 ~ 'Measurements desired',
																	desiredMeasurement == 0 ~ 'Measurements not desired'),
								 evaluateThreshold = desiredMeasurement)
			}

			personOutput <- rbind(personOutput, personsWithMeasurementCriteria)

			bitString <- '0000100'

			personsWithDeathCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithDeathCriteria',
							 spec = case_when(desiredDeath == 1 ~ 'Death domain desired',
							 								  desiredDeath == 0 ~ 'Death domain not desired'),
							 evaluateThreshold = desiredDeath)

			if(nrow(personsWithDeathCriteria) == 0){
				personsWithDeathCriteria[1,]$COUNT_VALUE <- 0
				personsWithDeathCriteria[1,]$statistic <- 'propWithDeathCriteria'
				personsWithDeathCriteria <- personsWithDeathCriteria %>%
					mutate(spec = case_when(desiredDeath == 1 ~ 'Death domain desired',
																	desiredDeath == 0 ~ 'Death domain not desired'),
								 evaluateThreshold = desiredDeath)
			}

			personOutput <- rbind(personOutput, personsWithDeathCriteria)

			bitString <- '0000010'

			personsWithProcedureCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithProcedureCriteria',
							 spec = case_when(desiredProcedure == 1 ~ 'Procedures desired',
							 								 desiredDeath == 0 ~ 'Procedures not desired'),
							 evaluateThreshold = desiredProcedure)

			if(nrow(personsWithProcedureCriteria) == 0){
				personsWithProcedureCriteria[1,]$COUNT_VALUE <- 0
				personsWithProcedureCriteria[1,]$statistic <- 'propWithProcedureCriteria'
				personsWithProcedureCriteria <- personsWithProcedureCriteria %>%
					mutate(spec = case_when(desiredprocedure == 1 ~ 'Procedures desired',
																	desiredProcedure == 0 ~ 'Procedures not desired'),
								 evaluateThreshold = desiredProcedure)
			}

			personOutput <- rbind(personOutput, personsWithProcedureCriteria)

			bitString <- '0000001'

			personsWithObservationCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = 'propWithObservationCriteria',
							 spec = case_when(desiredObservation == 1 ~ 'Observations desired',
							 								  desiredObservation == 0 ~ 'Observations not desired'),
							 evaluateThreshold = desiredObservation)


			if(nrow(personsWithObservationCriteria) == 0){
				personsWithObservationCriteria[1,]$COUNT_VALUE <- 0
				personsWithObservationCriteria[1,]$statistic <- 'propWithObservationCriteria'
				personsWithObservationCriteria <- personsWithObservationCriteria %>%
					mutate(spec = case_when(desiredObservation == 1 ~ 'Observations desired',
								 								 desiredObservation == 0 ~ 'Observations not desired'),
								 evaluateThreshold = desiredObservation)
			}

			personOutput <- rbind(personOutput, personsWithObservationCriteria)

			#Data Domain Coverage - Measurements w/Values
			numMeasRecords <- sum(dbProfile[which(dbProfile$ANALYSIS_ID == 1801),]$COUNT_VALUE)

			numMeasRecordsWithValues <- dbProfile[which(dbProfile$ANALYSIS_ID == 1814),]$COUNT_VALUE

			propMeasRecordsWithValues <- numMeasRecordsWithValues/numMeasRecords

			measRecordsWithValues <- as.data.frame(cbind("propMeasRecordsWithValues", numMeasRecordsWithValues, propMeasRecordsWithValues)) %>%
				rename("statistic" = "V1",
							 "value" = "numMeasRecordsWithValues",
							 "proportion" = "propMeasRecordsWithValues") %>%
				mutate(spec = case_when(desiredObservation == 1 ~ 'Measurements with values desired',
																desiredObservation == 0 ~ 'Measurements with values not desired'),
							 evaluateThreshold = desiredMeasurementValues)

			finalOutput <- rbind(finalOutput, measRecordsWithValues)

			#Visit Context --------

			# identify rows were criteria is met
			personsWithIPCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 200) %>%
				filter(VISIT_ANCESTOR_CONCEPT_ID %in% c(9201, 262)) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = "propWithIPCriteria",
							 spec = case_when(requiredIP == 1 && desiredIP == 1 ~ 'Inpatient visits required and desired',
																requiredIP == 1 && desiredIP == 0 ~ 'Inpatient visits required',
																requiredIP == 0 && desiredIP == 1 ~ 'Inpatient visits desired',
																requiredIP == 0 && desiredIP == 0 ~ 'Inpatient visits not required nor desired'),
							 evaluateThreshold = case_when(requiredIP == 1 && desiredIP == 1 ~ 1,
							 															requiredIP == 1 && desiredIP == 0 ~ 1,
							 															requiredIP == 0 && desiredIP == 1 ~ 3,
							 															requiredIP == 0 && desiredIP == 0 ~ 0))

			#if no criteria is met, add a row with a 0 value
			if(nrow(personsWithIPCriteria) == 0){
				personsWithIPCriteria[1,]$statistic <- 'propWithIPCriteria'
				personsWithIPCriteria[1,]$COUNT_VALUE <- 0
				personsWithIPCriteria <- personsWithIPCriteria %>%
					mutate(spec = case_when(requiredIP == 1 && desiredIP == 1 ~ 'Inpatient visits required and desired',
													 requiredIP == 1 && desiredIP == 0 ~ 'Inpatient visits required',
													 requiredIP == 0 && desiredIP == 1 ~ 'Inpatient visits desired',
													 requiredIP == 0 && desiredIP == 0 ~ 'Inpatient visits not required nor desired'),
									evaluateThreshold = case_when(requiredIP == 1 && desiredIP == 1 ~ 1,
																								requiredIP == 1 && desiredIP == 0 ~ 1,
																								requiredIP == 0 && desiredIP == 1 ~ 3,
																								requiredIP == 0 && desiredIP == 0 ~ 0))
			}

			personOutput <- rbind(personOutput, personsWithIPCriteria)

			personsWithOPCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 200) %>%
				filter(VISIT_ANCESTOR_CONCEPT_ID %in% c(9202, 5083)) %>%
		  	select(COUNT_VALUE) %>%
				mutate(statistic = "propWithOPCriteria",
							 spec = case_when(requiredOP == 1 && desiredOP == 1 ~ 'Outpatient visits required and desired',
							 								 requiredOP == 1 && desiredOP == 0 ~ 'Outpatient visits required',
							 								 requiredOP == 0 && desiredOP == 1 ~ 'Outpatient visits desired',
							 								 requiredOP == 0 && desiredOP == 0 ~ 'Outpatient visits not required nor desired'),
							 evaluateThreshold = case_when(requiredOP == 1 && desiredOP == 1 ~ 1,
							 															requiredOP == 1 && desiredOP == 0 ~ 1,
							 															requiredOP == 0 && desiredOP == 1 ~ 3,
							 															requiredOP == 0 && desiredOP == 0 ~ 0))

			#if no criteria is met, add a row with a 0 value
			if(nrow(personsWithOPCriteria) == 0){
				personsWithOPCriteria[1,]$statistic <- 'propWithOPCriteria'
				personsWithOPCriteria[1,]$COUNT_VALUE <- 0
				personsWithOPCriteria <- personsWithOPCriteria %>%
					mutate(spec = case_when(requiredOP == 1 && desiredOP == 1 ~ 'Outpatient visits required and desired',
																	requiredOP == 1 && desiredOP == 0 ~ 'Outpatient visits required',
																	requiredOP == 0 && desiredOP == 1 ~ 'Outpatient visits desired',
																	requiredOP == 0 && desiredOP == 0 ~ 'Outpatient visits not required nor desired'),
								 evaluateThreshold = case_when(requiredOP == 1 && desiredOP == 1 ~ 1,
								 															requiredOP == 1 && desiredOP == 0 ~ 1,
								 															requiredOP == 0 && desiredOP == 1 ~ 3,
								 															requiredOP == 0 && desiredOP == 0 ~ 0))
			}

			personOutput <- rbind(personOutput, personsWithOPCriteria)

			personsWithERCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 200) %>%
				filter(VISIT_ANCESTOR_CONCEPT_ID %in% c(9203, 262)) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = "propWithERCriteria",
							 spec = case_when(requiredER == 1 && desiredER == 1 ~ 'Emergency Room visits required and desired',
							 								 requiredER == 1 && desiredER == 0 ~ 'Emergency Room visits required',
							 								 requiredER == 0 && desiredER == 1 ~ 'Emergency Room visits desired',
							 								 requiredER == 0 && desiredER == 0 ~ 'Emergency Room visits not required nor desired'),
							 evaluateThreshold = case_when(requiredER == 1 && desiredER == 1 ~ 1,
							 															requiredER == 1 && desiredER == 0 ~ 1,
							 															requiredER == 0 && desiredER == 1 ~ 3,
							 															requiredER == 0 && desiredER == 0 ~ 0))

			#if no criteria is met, add a row with a 0 value
			if(nrow(personsWithERCriteria) == 0){
				personsWithERCriteria[1,]$statistic <- 'propWithERCriteria'
				personsWithERCriteria[1,]$COUNT_VALUE <- 0
				personsWithERCriteria <- personsWithERCriteria %>%
					mutate(spec = case_when(requiredER == 1 && desiredER == 1 ~ 'Emergency Room visits required and desired',
								 								 requiredER == 1 && desiredER == 0 ~ 'Emergency Room visits required',
								 								 requiredER == 0 && desiredER == 1 ~ 'Emergency Room visits desired',
								 								 requiredER == 0 && desiredER == 0 ~ 'Emergency Room visits not required nor desired'),
								 evaluateThreshold = case_when(requiredER == 1 && desiredER == 1 ~ 1,
								 															requiredER == 1 && desiredER == 0 ~ 1,
								 															requiredER == 0 && desiredER == 1 ~ 3,
								 															requiredER == 0 && desiredER == 0 ~ 0))
			}

			personOutput <- rbind(personOutput, personsWithERCriteria)

			#Required Concepts ------

			personsWithRequiredTargetConcepts <- dbProfile %>%
				filter(ANALYSIS_ID %in% c(1800, 400, 600, 700, 800, 2100)) %>%
				filter(STRATUM_1 %in% requiredTargetConcepts) %>%
				select(COUNT_VALUE) %>%
				mutate(statistic = "propWithRequiredTargetConcepts",
							 spec = target,
							 evaluateThreshold = 2)

			if(nrow(personsWithRequiredTargetConcepts) == 0){
				personsWithRequiredTargetConcepts[1,]$statistic <- 'propWithRequiredTargetConcepts'
				personsWithRequiredTargetConcepts[1,]$COUNT_VALUE <- 0
				personsWithRequiredTargetConcepts <- personsWithRequiredTargetConcepts %>%
					mutate(spec = target,
								 evaluateThreshold = 2)
			}

			personOutput <- rbind(personOutput, personsWithRequiredTargetConcepts)


			if(is.null(studySpecs$comparatorConceptIds)){
				personsWithRequiredComparatorConcepts <- data.frame(statistic = 'propWithRequiredComparatorConcepts',
																														COUNT_VALUE = NA,
																														spec = NA, # Revised from NULL to NA since you cannot declare a column with a NULL value as the only value in the data frame.
																														evaluateThreshold = 0)
			}else{
				personsWithRequiredComparatorConcepts <- dbProfile %>%
					filter(ANALYSIS_ID %in% c(1800, 400, 600, 700, 800, 2100)) %>%
					filter(STRATUM_1 %in% requiredComparatorConcepts) %>%
					select(COUNT_VALUE) %>%
					mutate(statistic = "propWithRequiredComparatorConcepts",
								 spec = comparator,
								 evaluateThreshold = 2)

				if(nrow(personsWithRequiredComparatorConcepts) == 0){
					personsWithRequiredComparatorConcepts[1,]$statistic <- 'propWithRequiredComparatorConcepts'
					personsWithRequiredComparatorConcepts[1,]$COUNT_VALUE <- 0
					personsWithRequiredComparatorConcepts <- personsWithRequiredComparatorConcepts %>%
						mutate(spec = comparator,
									 evaluateThreshold = 2)
				}

			}

			personOutput <- rbind(personOutput, personsWithRequiredComparatorConcepts)

			if(is.null(studySpecs$indicationConceptIds)){
				personsWithRequiredIndicationConcepts <- data.frame(statistic = 'propWithRequiredIndicationConcepts',
																														COUNT_VALUE = NA,
																														spec = NA, # Revised from NULL to NA since you cannot declare a column with a NULL value as the only value in the data frame.
																														evaluateThreshold = 0)
			}else{
				personsWithRequiredIndicationConcepts <- dbProfile %>%
					filter(ANALYSIS_ID %in% c(1800, 400, 600, 700, 800, 2100)) %>%
					filter(STRATUM_1 %in% requiredIndicationConcepts) %>%
					select(COUNT_VALUE) %>%
					mutate(statistic = "propWithRequiredIndicationConcepts",
								 spec = indication,
								 evaluateThreshold = 2)

				if(nrow(personsWithRequiredIndicationConcepts) == 0){
					personsWithRequiredIndicationConcepts[1,]$statistic <- 'propWithRequiredIndicationConcepts'
					personsWithRequiredIndicationConcepts[1,]$COUNT_VALUE <- 0
					personsWithRequiredIndicationConcepts <- personsWithRequiredIndicationConcepts %>%
						mutate(spec = indication,
									 evaluateThreshold = 2)
				}
				personOutput <- rbind(personOutput, personsWithRequiredIndicationConcepts)
			}


			if(is.null(studySpecs$outcomeConceptIds)){
				personsWithRequiredOutcomeConcepts <- data.frame(statistic = 'propWithRequiredOutcomeConcepts',
																												 COUNT_VALUE = NA,
																												 spec = NA, # Revised from NULL to NA since you cannot declare a column with a NULL value as the only value in the data frame.
																												 evaluateThreshold = 0)
			}else{
				personsWithRequiredOutcomeConcepts <- dbProfile %>%
					filter(ANALYSIS_ID %in% c(1800, 400, 600, 700, 800, 2100)) %>%
					filter(STRATUM_1 %in% requiredOutcomeConcepts) %>%
					select(COUNT_VALUE) %>%
					mutate(statistic = "propWithRequiredOutcomeConcepts",
								 spec = outcome,
								 evaluateThreshold = 2)

				if(nrow(personsWithRequiredOutcomeConcepts) == 0){
					personsWithRequiredOutcomeConcepts[1,]$statistic <- 'propWithRequiredOutcomeConcepts'
					personsWithRequiredOutcomeConcepts[1,]$COUNT_VALUE <- 0
					personsWithRequiredOutcomeConcepts <- personsWithRequiredOutcomeConcepts %>%
						mutate(spec = outcome,
									 evaluateThreshold = 2)
				}
				personOutput <- rbind(personOutput, personsWithRequiredOutcomeConcepts)
			}



			# Evaluate diagnostics for recommended Dbs per study question -----------

			# Start with required/desired domains

			personOutputSum <- personOutput %>%
				group_by(statistic, spec, evaluateThreshold) %>%
				summarise(value = sum(COUNT_VALUE)) %>%
				mutate(proportion = value/numPersonsInDb)

			personOutputSum <- rbind(finalOutput, personOutputSum)

			personOutputSum <- personOutputSum %>%
													left_join(ddThresholds,
																		by = c("statistic" = "statistic"))

			# Evaluate results against thresholds

			sampleSizeValues <- personOutputSum %>%
													filter(evaluateThreshold == 1) %>%
													select("proportion")

			if(requiredIP == 1){
				ipProp <- personOutputSum %>%
					filter(statistic == "propWithIPCriteria") %>%
					select("proportion")

				sampleSizeValues <- rbind(sampleSizeValues, ipProp)
			}

			if(requiredER == 1){
				erProp <- personOutputSum %>%
					filter(statistic == "propWithERCriteria") %>%
					select("proportion")

				sampleSizeValues <- rbind(sampleSizeValues, erProp)
			}

			if(requiredOP == 1){
				opProp <- personOutputSum %>%
					filter(statistic == "propWithOPCriteria") %>%
					select("proportion")

				sampleSizeValues <- rbind(sampleSizeValues, opProp)
			}

			dataDiagnosticsOutput <- personOutputSum %>%
				mutate(status = case_when(proportion <= threshold  ~ "fail",
																	proportion > threshold  ~ "pass"),
							fail = case_when(proportion <= threshold  ~ 1,
															 proportion > threshold  ~ 0))

			if(studySpecs$includeIndicationInCalc){
				minSampleSizeProp <- min(as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredTargetConcepts'),]$proportion),
																 as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredComparatorConcepts'),]$proportion),
																 na.rm = TRUE)*prod(as.numeric(sampleSizeValues[,1]))*as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredIndicationConcepts'),]$proportion)

			}else{
				minSampleSizeProp <- min(as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredTargetConcepts'),]$proportion),
																 as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredComparatorConcepts'),]$proportion),
																 na.rm = TRUE)*prod(as.numeric(sampleSizeValues[,1]))

			}

			minSampleSize <- round(minSampleSizeProp*numPersonsInDb, digits = 0)

			minSample <- list(statistic = "minSampleSize",
												value = minSampleSize,
												proportion = minSampleSizeProp,
												spec = "> 1000",
												evaluateThreshold = 1,
												threshold = 0,
												status = 'pass',
												fail = 0)

			if(studySpecs$includeIndicationInCalc){
				maxSampleSizeProp <- min(as.numeric(sampleSizeValues[,1]),
																 as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredTargetConcepts'),]$proportion),
																 as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredComparatorConcepts'),]$proportion),
																 as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredIndicationConcepts'),]$proportion),
																 na.rm = TRUE)

			}else{
				maxSampleSizeProp <- min(as.numeric(sampleSizeValues[,1]),
																 as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredTargetConcepts'),]$proportion),
																 as.numeric(personOutputSum[which(personOutputSum$statistic == 'propWithRequiredComparatorConcepts'),]$proportion),
																 na.rm = TRUE)

			}

			maxSampleSize <- maxSampleSizeProp*numPersonsInDb

			if(maxSampleSize < 1000){
				maxSampleStatus <- 'fail'
				maxSampleFail <- 1
			}else{
				maxSampleStatus <- 'pass'
				maxSampleFail <- 0
			}

			maxSample <- list(statistic = "maxSampleSize",
												value = maxSampleSize,
												proportion = maxSampleSizeProp,
												spec = "> 1000",
												evaluateThreshold = 1,
												threshold = 1000,
												status = maxSampleStatus,
												fail = maxSampleFail)

			dataDiagnosticsOutput <- rbind(dataDiagnosticsOutput, minSample, maxSample)

			dataDiagnosticsOutput <- dataDiagnosticsOutput %>%
																filter(evaluateThreshold > 0) %>%
																mutate(analysisId = analysisId,
																			 analysisName = analysisName,
																			 releaseKey = dbName, .before = statistic)

    	if(k==1){
				dataDiagnosticsResults <- dataDiagnosticsOutput
			}else{
				dataDiagnosticsResultsNew <- rbind(dataDiagnosticsResults,dataDiagnosticsOutput)
				dataDiagnosticsResults <- dataDiagnosticsResultsNew
			}
		} # end of for loop around analysis list

		CohortGenerator::writeCsv(dataDiagnosticsResults, file.path(outputFolder,"data_diagnostics_output.csv"), append = (i != 1))

		if(i==1){
			totalResults <- dataDiagnosticsResults
		}else{
			totalResultsNew <- rbind(dataDiagnosticsResults,totalResults)
			totalResults <- totalResultsNew
		}

	} # end of for loop around database list

	dbDiagnosticsSummary <- DbDiagnostics::createDataDiagnosticsSummary(totalResults)
	CohortGenerator::writeCsv(dbDiagnosticsSummary,file.path(outputFolder,"data_diagnostics_summary.csv"))

	tempFileName <- tempfile()

	ddAnalysisToRow <- function(ddAnalysis) {
		ParallelLogger::saveSettingsToJson(ddAnalysis, tempFileName)
		row <- tibble(
			analysisId = ddAnalysis$analysisId,
			description = ddAnalysis$description,
			definition = readChar(tempFileName, file.info(tempFileName)$size)
		)
		invisible(row)
	}

	dataDiagnosticsAnalysis <- lapply(dataDiagnosticsSettingsList, ddAnalysisToRow)
	dataDiagnosticsAnalysis <- bind_rows(dataDiagnosticsAnalysis) %>%
		distinct()

	unlink(tempFileName)

	fileName <- file.path(outputFolder, "data_diagnostics_analysis.csv")
	CohortGenerator::writeCsv(dataDiagnosticsAnalysis, fileName)

 invisible(totalResults)
}
