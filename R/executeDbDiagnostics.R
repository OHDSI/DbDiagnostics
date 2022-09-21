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
#' @param connectionDetails         A connectionDetails object for connecting to the database containing the DbProfile results
#' @param resultsDatabaseSchema     The fully qualified database name of the results schema where the DbProfile results are housed
#' @param resultsTableName					The name of the table in the results schema with the DbProfile results
#' @param outputFolder              Results will be written to this directory, default = getwd()
#' @param dataDiagnosticsSettings		A list of settings created from DataDiagnostics::createDataDiagnosticsSettings() function
#'
#' @return
#'
#' @import DataQualityDashboard Achilles DatabaseConnector SqlRender dplyr magrittr
#'
#' @export

executeDbDiagnostics <- function(connectionDetails,
																 resultsDatabaseSchema,
																 resultsTableName,
																 outputFolder = getwd(),
																 dataDiagnosticsSettings) {

	# Connect to the results schema to get list of databases included in results table ---------------

	conn <- DatabaseConnector::connect(connectionDetails)

	sql <- "SELECT DISTINCT db_id, database_id
        FROM @results_database_schema.@results_table_name"

	rsql <- SqlRender::render(sql = sql,
														results_database_schema = resultsDatabaseSchema,
														results_table_name = resultsTableName)
	tsql <- SqlRender::translate(rsql, connectionDetails$dbms)

	dbNames <- DatabaseConnector::querySql(conn, tsql)

	DatabaseConnector::disconnect(conn)

	# Get the most recent release for each database -------------------------------
	for(i in 1:nrow(dbNames)){
		dbInfo <- strsplit(dbNames$DB_ID[i], split = "-")

		dbNames$DB_ABBREV[i] <- dbInfo[[1]][[1]]
		dbNames$DB_DATE[i] <- dbInfo[[1]][[2]]

		rm(dbInfo)
	}
	rm(i)

	latestDbs<- dbNames %>%
		group_by(DB_ABBREV) %>%
		slice_max(DB_DATE, n=1)

	dbNum <- nrow(latestDbs)


	# Get the specifications for all studies included in this run ------------------

	studySpecs <- dataDiagnosticsSettings

	# TODO ---------------
	# evaluate the specs input
	# look at data types and stop if target concept id is null

	# OLD - this was used with the excel where multiple studies were specified in one file
	# numDd <- ddSpecs %>%
	# 	select(-c("Category","Instructions","Abbreviation")) %>%
	# 	ncol(.)

	# Get the thresholds for the study -------------------------------------------

	ddThresholds <- read.csv(system.file("csv", "ddThresholds.csv", package = "DbDiagnostics"), stringsAsFactors = FALSE)

	# Loop through the studies specified in one file - OLD
	# OLD this was used when multiple studies were specified in one file

	# for(j in 1:numDd){
	# 	colIndex <- j+3
	#
	# 	studySpecs <- as.data.frame(t(ddSpecs[,c(1:3,colIndex)]))
	# 	colnames(studySpecs) <- studySpecs["Abbreviation",]

		# Name of this individual study
		studyName <- studySpecs$analysisName

		# Set up the output

		output <- data.frame(matrix(ncol = 25, nrow = 0))
		x <- c("studyName", "dbName", "totalPersonsInDb", "propInAgeRange", "propWithAgeAtFirstObs", "propWithGenderCriteria", "propWithRaceCriteria",
					 "propWithEthnicityCriteria", "propWithCalendarTime", "propWithLongitudinalCriteria", "propWithRequiredDomain",
					 "propWithConditionCriteria", "propWithDrugCriteria", "propWithDeviceCriteria", "propWithMeasurementCriteria", "propMeasRecordsWithValues",
					 "propWithDeathCriteria", "propWithProcedureCriteria", "propWithObservationCriteria", "propWithIPCriteria",
					 "propWithOPCriteria", "propWithERCriteria", "propWithRequiredTargetConcepts", "propWithRequiredComparatorConcepts",
					 "propWithRequiredOutcomeConcepts")
		colnames(output) <- x

		# Loop through the databases -----------------------------------------------

		for(i in 1:nrow(latestDbs)){

			dbName <- latestDbs[i,1]

			# Get the dbProfile information for the database
			sql <- "SELECT * FROM @results_database_schema.@results_table_name WHERE db_id = '@databaseName'"

			rsql <- SqlRender::render(sql, databaseName = latestDbs[i,1],
																results_database_schema = resultsDatabaseSchema,
																results_table_name = resultsTableName)

			tsql <- SqlRender::translate(rsql, connectionDetails$dbms)

			conn <- DatabaseConnector::connect(connectionDetails)

			dbProfile <- DatabaseConnector::querySql(conn, tsql)

			DatabaseConnector::disconnect(conn)

			# Set up the specs for this study/db combination. This is done after getting the dbProfile information because NULL
			# values in the specs get values from the database in order to evaluate them

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
			if(!is.null(studySpecs$comparatorName)){
				comparator <- studySpecs$comparatorName
				requiredComparatorConcepts <- studySpecs$comparatorConceptIds
				numCriteria <- numCriteria + 1
			}else{
				requiredComparatorConcepts <- NULL
			}

			#outcome
			if(!is.null(studySpecs$outcomeName)){
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
				summarise(personsInAgeRange = sum(COUNT_VALUE)) %>%
				mutate(propInAgeRange = personsInAgeRange/numPersonsInDb)

			# age at First Obs # did people less than maxAge
			personsWithAgeAtFirstObs <- dbProfile %>%
				filter(ANALYSIS_ID == 101) %>%
				filter(as.numeric(STRATUM_1) <= maxAge) %>%
				summarise(personsWithAgeAtFirstObs = sum(COUNT_VALUE)) %>%
				mutate(propWithAgeAtFirstObs = personsWithAgeAtFirstObs/numPersonsInDb)

			##Gender ----------
			personsWithGenderCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2) %>%
				filter(STRATUM_1 %in% genderConceptIds) %>%
				summarise(personsWithGenderCriteria = sum(COUNT_VALUE)) %>%
				mutate(propWithGenderCriteria = personsWithGenderCriteria/numPersonsInDb)

			### personsWithGenderCriteria/numPersonInDb = estimated proportion of ppl who meet gender criteria in db

			##Race -------------
			personsWithRaceCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 4) %>%
				filter(STRATUM_1 %in% raceConceptIds) %>%
				summarise(personsWithRaceCriteria = sum(COUNT_VALUE)) %>%
				mutate(propWithRaceCriteria = personsWithRaceCriteria/numPersonsInDb)

			##Ethnicity ----------
			personsWithEthnicityCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 5) %>%
				filter(STRATUM_1 %in% ethnicityConceptIds) %>%
				summarise(personsWithEthnicityCriteria = sum(COUNT_VALUE)) %>%
				mutate(propWithEthnicityCriteria = personsWithEthnicityCriteria/numPersonsInDb)

			#Calendar Time ----------

			totalObsPeriods <- sum(dbProfile[which(dbProfile$ANALYSIS_ID == 111),]$COUNT_VALUE)

			avgObsPeriodsPerPerson <- totalObsPeriods/numPersonsInDb

			obsPeriodsWithCalendarStarts <- dbProfile %>%
				filter(ANALYSIS_ID == 111 & STRATUM_1 >= studyStartDate) %>%
				summarise(personsWithCalendarStarts = sum(COUNT_VALUE)/avgObsPeriodsPerPerson) %>%
				mutate(propWithCalendarStarts = personsWithCalendarStarts/numPersonsInDb)

			obsPeriodsWithCalendarEnds <- dbProfile %>%
				filter(ANALYSIS_ID == 112 & STRATUM_1 <= studyEndDate) %>%
				summarise(personsWithCalendarEnds = sum(COUNT_VALUE)/avgObsPeriodsPerPerson) %>%
				mutate(propWithCalendarEnds = personsWithCalendarEnds/numPersonsInDb)

			propWithCalendarTime <- (1 - ((1 - obsPeriodsWithCalendarEnds$propWithCalendarEnds) + (1 - obsPeriodsWithCalendarStarts$propWithCalendarStarts)))

			countWithCalendarTime <- propWithCalendarTime*numPersonsInDb

			personsWithCalendarTime <- as.data.frame(cbind(countWithCalendarTime, propWithCalendarTime))

			colnames(personsWithCalendarTime) <- c( "personsWithCalendarTime", "propWithCalendarTime")

			## Longitudinality --------

			num30DayIncrements <- round(requiredDurationDays/30, digits = 0)

			personsWithLongitudinalCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 108) %>%
				filter(STRATUM_1 >= num30DayIncrements) %>%
				summarise(personsWithLongitudinalCriteria = sum(COUNT_VALUE)) %>%
				mutate(propWithLongitudinalCriteria = personsWithLongitudinalCriteria/numPersonsInDb)

			#Data Domain Coverage ----------

			bitString <- paste0(requiredCondition,requiredDrug,requiredDevice,requiredMeasurement,0,requiredProcedure,requiredObservation)

			personsWithRequiredDomain <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE, STRATUM_2) %>%
				rename(personsWithRequiredDomain = COUNT_VALUE) %>%
				rename(propWithRequiredDomain = STRATUM_2)

			bitString <- '1000000'

			personsWithConditionCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE, STRATUM_2) %>%
				rename(personsWithConditionCriteria = COUNT_VALUE) %>%
				rename(propWithConditionCriteria = STRATUM_2)

			bitString <- '0100000'

			personsWithDrugCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE, STRATUM_2) %>%
				rename(personsWithDrugCriteria = COUNT_VALUE) %>%
				rename(propWithDrugCriteria = STRATUM_2)

			bitString <- '0010000'

			personsWithDeviceCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE, STRATUM_2) %>%
				rename(personsWithDeviceCriteria = COUNT_VALUE) %>%
				rename(propWithDeviceCriteria = STRATUM_2)

			bitString <- '0001000'

			personsWithMeasurementCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE, STRATUM_2) %>%
				rename(personsWithMeasurementCriteria = COUNT_VALUE) %>%
				rename(propWithMeasurementCriteria = STRATUM_2)


			bitString <- '0000100'

			personsWithDeathCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE, STRATUM_2) %>%
				rename(personsWithDeathCriteria = COUNT_VALUE) %>%
				rename(propWithDeathCriteria = STRATUM_2)

			if(nrow(personsWithDeathCriteria) == 0){
				personsWithDeathCriteria[1,]$personsWithDeathCriteria <- 0
				personsWithDeathCriteria[1,]$propWithDeathCriteria <- 0

			}

			bitString <- '0000010'

			personsWithProcedureCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE, STRATUM_2) %>%
				rename(personsWithProcedureCriteria = COUNT_VALUE) %>%
				rename(propWithProcedureCriteria = STRATUM_2)

			bitString <- '0000001'

			personsWithObservationCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 2004) %>%
				filter(STRATUM_1 == bitString) %>%
				select(COUNT_VALUE, STRATUM_2) %>%
				rename(personsWithObservationCriteria = COUNT_VALUE) %>%
				rename(propWithObservationCriteria = STRATUM_2)


			#Data Domain Coverage - Measurements w/Values
			numMeasRecords <- sum(dbProfile[which(dbProfile$ANALYSIS_ID == 1801),]$COUNT_VALUE)

			numMeasRecordsWithValues <- dbProfile[which(dbProfile$ANALYSIS_ID == 1814),]$COUNT_VALUE

			propMeasRecordsWithValues <- numMeasRecordsWithValues/numMeasRecords

			measRecordsWithValues <- as.data.frame(cbind(numMeasRecordsWithValues, propMeasRecordsWithValues))

			colnames(measRecordsWithValues) <- c( "measRecordsWithValues", "propMeasRecordsWithValues")

			#Visit Context --------

			personsWithIPCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 200) %>%
				filter(VISIT_ANCESTOR_CONCEPT_ID %in% c(9201, 262)) %>%
				summarise(personsWithIPCriteria = sum(COUNT_VALUE)) %>%
				mutate(propWithIPCriteria = personsWithIPCriteria/numPersonsInDb)

			personsWithOPCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 200) %>%
				filter(VISIT_ANCESTOR_CONCEPT_ID %in% c(9202, 5083)) %>%
				summarise(personsWithOPCriteria = sum(COUNT_VALUE)) %>%
				mutate(propWithOPCriteria = personsWithOPCriteria/numPersonsInDb)

			personsWithERCriteria <- dbProfile %>%
				filter(ANALYSIS_ID == 200) %>%
				filter(VISIT_ANCESTOR_CONCEPT_ID %in% c(9203, 262)) %>%
				summarise(personsWithERCriteria = sum(COUNT_VALUE)) %>%
				mutate(propWithERCriteria = personsWithERCriteria/numPersonsInDb)

			#Required Concepts ------

			personsWithRequiredTargetConcepts <- dbProfile %>%
				filter(ANALYSIS_ID %in% c(1800, 400, 600, 700, 800, 2100)) %>%
				filter(STRATUM_1 %in% requiredTargetConcepts) %>%
				summarise(personsWithRequiredTargetConcepts = sum(COUNT_VALUE)) %>%
				mutate(propWithRequiredTargetConcepts = personsWithRequiredTargetConcepts/numPersonsInDb)

			if(is.null(studySpecs$comparatorConceptIds)){
				personsWithRequiredComparatorConcepts <- data.frame(matrix(ncol = 2, nrow = 1))
				y <- c("personsWithRequiredComparatorConcepts", "propWithRequiredComparatorConcepts")
				colnames(personsWithRequiredComparatorConcepts) <- y

			}else{
				personsWithRequiredComparatorConcepts <- dbProfile %>%
					filter(ANALYSIS_ID %in% c(1800, 400, 600, 700, 800, 2100)) %>%
					filter(STRATUM_1 %in% requiredComparatorConcepts) %>%
					summarise(personsWithRequiredComparatorConcepts = sum(COUNT_VALUE)) %>%
					mutate(propWithRequiredComparatorConcepts = personsWithRequiredComparatorConcepts/numPersonsInDb)
				}

			if(is.null(studySpecs$outcomeConceptIds)){
				personsWithRequiredOutcomeConcepts <- data.frame(matrix(ncol = 2, nrow = 1))
				z <- c("personsWithRequiredOutcomeConcepts", "propWithRequiredOutcomeConcepts")
				colnames(personsWithRequiredOutcomeConcepts) <- z

			}else{
				personsWithRequiredOutcomeConcepts <- dbProfile %>%
					filter(ANALYSIS_ID %in% c(1800, 400, 600, 700, 800, 2100)) %>%
					filter(STRATUM_1 %in% requiredOutcomeConcepts) %>%
					summarise(personsWithRequiredOutcomeConcepts = sum(COUNT_VALUE)) %>%
					mutate(propWithRequiredOutcomeConcepts = personsWithRequiredOutcomeConcepts/numPersonsInDb)
			}

			# Evaluate diagnostics for recommended Dbs per study question -----------

			# Start with required/desired domains

			dbDiagnostics <- cbind(studyName,
														 dbName,
														 as.data.frame(numPersonsInDb),
														 personsInBirthYearRange$propInAgeRange,
														 personsWithAgeAtFirstObs$propWithAgeAtFirstObs,
														 personsWithGenderCriteria$propWithGenderCriteria,
														 personsWithRaceCriteria$propWithRaceCriteria,
														 personsWithEthnicityCriteria$propWithEthnicityCriteria,
														 personsWithCalendarTime$propWithCalendarTime,
														 personsWithLongitudinalCriteria$propWithLongitudinalCriteria,
														 personsWithRequiredDomain$propWithRequiredDomain,
														 personsWithConditionCriteria$propWithConditionCriteria,
														 personsWithDrugCriteria$propWithDrugCriteria,
														 personsWithDeviceCriteria$propWithDeviceCriteria,
														 personsWithMeasurementCriteria$propWithMeasurementCriteria,
														 propMeasRecordsWithValues,
														 personsWithDeathCriteria$propWithDeathCriteria,
														 personsWithProcedureCriteria$propWithProcedureCriteria,
														 personsWithObservationCriteria$propWithObservationCriteria,
														 personsWithIPCriteria$propWithIPCriteria,
														 personsWithOPCriteria$propWithOPCriteria,
														 personsWithERCriteria$propWithERCriteria,
														 personsWithRequiredTargetConcepts$propWithRequiredTargetConcepts,
														 personsWithRequiredComparatorConcepts$propWithRequiredComparatorConcepts,
														 personsWithRequiredOutcomeConcepts$propWithRequiredOutcomeConcepts)

			colnames(dbDiagnostics) <- x

			# Evaluate results against thresholds

			sampleSizeValues <- c()

			if(dbDiagnostics$propInAgeRange > ddThresholds$propInAgeRange){
				dbDiagnostics$failAgeRange = 0
			}else{
				dbDiagnostics$failAgeRange = 1
			}

			sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propInAgeRange)

			if(dbDiagnostics$propWithAgeAtFirstObs > ddThresholds$propWithAgeAtFirstObs){
				dbDiagnostics$failAgeAtFirstObs = 0
			}else{
				dbDiagnostics$failAgeAtFirstObs = 1
			}

			sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithAgeAtFirstObs)

			if(dbDiagnostics$propWithGenderCriteria > ddThresholds$propWithGenderCriteria){
				dbDiagnostics$failGenderCriteria = 0
			}else{
				dbDiagnostics$failGenderCriteria = 1
			}

			sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithGenderCriteria)


			if(dbDiagnostics$propWithRaceCriteria > ddThresholds$propWithRaceCriteria){
				dbDiagnostics$failRaceCriteria = 0
			}else{
				dbDiagnostics$failRaceCriteria = 1
			}

			sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithRaceCriteria)


			if(dbDiagnostics$propWithEthnicityCriteria > ddThresholds$propWithEthnicityCriteria){
				dbDiagnostics$failEthnicityCriteria = 0
			}else{
				dbDiagnostics$failEthnicityCriteria = 1
			}

			sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithEthnicityCriteria)


			if(dbDiagnostics$propWithCalendarTime > ddThresholds$propWithCalendarTime){
				dbDiagnostics$failCalendarTime = 0
			}else{
				dbDiagnostics$failCalendarTime = 1
			}

			sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithCalendarTime)


			if(dbDiagnostics$propWithLongitudinalCriteria > ddThresholds$propWithLongitudinalCriteria){
				dbDiagnostics$failLongitudinalCriteria = 0
			}else{
				dbDiagnostics$failLongitudinalCriteria = 1
			}

			sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithLongitudinalCriteria)


			if(dbDiagnostics$propWithRequiredDomain > ddThresholds$propWithRequiredDomain){
				dbDiagnostics$failRequiredDomain = 0
			}else{
				dbDiagnostics$failRequiredDomain = 1
			}

			sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithRequiredDomain)


			if(desiredCondition == 1){
				if(dbDiagnostics$propWithConditionCriteria > ddThresholds$propWithConditionCriteria){
					dbDiagnostics$failCondition = 0
				}else{
					dbDiagnostics$failCondition = 1
				}
			}else{
				dbDiagnostics$failCondition = 0
			}

			if(desiredDrug == 1){
				if(dbDiagnostics$propWithDrugCriteria > ddThresholds$propWithDrugCriteria){
					dbDiagnostics$failDrug = 0
				}else{
					dbDiagnostics$failDrug = 1
				}
			}else{
				dbDiagnostics$failDrug = 0
			}

			if(desiredDevice == 1){
				if(dbDiagnostics$propWithDeviceCriteria > ddThresholds$propWithDeviceCriteria){
					dbDiagnostics$failDevice = 0
				}else{
					dbDiagnostics$failDevice = 1
				}
			}else{
				dbDiagnostics$failDevice = 0
			}

			if(desiredMeasurement == 1){
				if(dbDiagnostics$propWithMeasurementCriteria > ddThresholds$propWithMeasurementCriteria){
					dbDiagnostics$failMeasurement = 0
				}else{
					dbDiagnostics$failMeasurement = 1
				}
			}else{
				dbDiagnostics$failMeasurement = 0
			}

			if(desiredProcedure == 1){
				if(dbDiagnostics$propWithProcedureCriteria > ddThresholds$propWithProcedureCriteria){
					dbDiagnostics$failProcedure = 0
				}else{
					dbDiagnostics$failProcedure = 1
				}
			}else{
				dbDiagnostics$failProcedure = 0
			}

			if(desiredObservation == 1){
				if(dbDiagnostics$propWithObservationCriteria > ddThresholds$propWithObservationCriteria){
					dbDiagnostics$failObservation = 0
				}else{
					dbDiagnostics$failObservation = 1
				}
			}else{
				dbDiagnostics$failObservation = 0
			}

			if(desiredMeasurementValues == 1){
				if(dbDiagnostics$propMeasRecordsWithValues > ddThresholds$propMeasRecordsWithValues){
					dbDiagnostics$failMeasWithValues = 0
				}else{
					dbDiagnostics$failMeasWithValues = 1
				}
			}else{
				dbDiagnostics$failMeasWithValues = 0
			}

			if(desiredDeath == 1){
				if(dbDiagnostics$propWithDeathCriteria > ddThresholds$propWithDeathCriteria){
					dbDiagnostics$failDeath = 0
				}else{
					dbDiagnostics$failDeath = 1
				}
			}else{
				dbDiagnostics$failDeath = 0
			}

			if(desiredIP == 1){
				if(dbDiagnostics$propWithIPCriteria > ddThresholds$propWithIPCriteria){
					dbDiagnostics$failIP = 0
				}else{
					dbDiagnostics$failIP = 1
				}
			}else{
				dbDiagnostics$failIP = 0
			}

			if(requiredIP == 1){
				sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithIPCriteria)
			}

			if(desiredER == 1){
				if(dbDiagnostics$propWithERCriteria > ddThresholds$propWithERCriteria){
					dbDiagnostics$failER = 0
				}else{
					dbDiagnostics$failER = 1
				}
			}else{
				dbDiagnostics$failER = 0
			}

			if(requiredER == 1){
				sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithERCriteria)
			}

			if(desiredOP == 1){
				if(dbDiagnostics$propWithOPCriteria > ddThresholds$propWithOPCriteria){
					dbDiagnostics$failOP = 0
				}else{
					dbDiagnostics$failOP = 1
				}
			}else{
				dbDiagnostics$failOP = 0
			}

			if(requiredOP == 1){
				sampleSizeValues <- append(sampleSizeValues, dbDiagnostics$propWithOPCriteria)
			}

			if(length(requiredTargetConcepts)>0){
				if(dbDiagnostics$propWithRequiredTargetConcepts > ddThresholds$propWithRequiredTargetConcepts){
					dbDiagnostics$failTarget = 0
				}else{
					dbDiagnostics$failTarget = 1
				}
			}else{
				dbDiagnostics$failTarget = 0
			}

			if(!is.null(requiredComparatorConcepts)){
				if(dbDiagnostics$propWithRequiredComparatorConcepts > ddThresholds$propWithRequiredComparatorConcepts){
					dbDiagnostics$failComparator = 0
				}else{
					dbDiagnostics$failComparator = 1
				}
			}else{
				dbDiagnostics$failComparator = 0
			}

			if(!is.null(requiredOutcomeConcepts)){
				if(dbDiagnostics$propWithRequiredOutcomeConcepts > ddThresholds$propWithRequiredOutcomeConcepts){
					dbDiagnostics$failOutcome = 0
				}else{
					dbDiagnostics$failOutcome = 1
				}
			}else{
				dbDiagnostics$failOutcome = 0
			}

			dbDiagnostics$minSampleSizeProp <- min(dbDiagnostics$propWithRequiredTargetConcepts, dbDiagnostics$propWithRequiredComparatorConcepts, na.rm = TRUE)*prod(as.numeric(sampleSizeValues))
			dbDiagnostics$minSampleSize <- as.numeric(dbDiagnostics$minSampleSizeProp)*as.numeric(dbDiagnostics$totalPersonsInDb)

			dbDiagnostics$maxSampleSizeProp <- min(sampleSizeValues, dbDiagnostics$propWithRequiredTargetConcepts, dbDiagnostics$propWithRequiredComparatorConcepts, na.rm = TRUE)
			dbDiagnostics$maxSampleSize <- as.numeric(dbDiagnostics$maxSampleSizeProp)*as.numeric(dbDiagnostics$totalPersonsInDb)

			if(dbDiagnostics$minSampleSize > 1000){
				dbDiagnostics$sampleSizeStatus <- "Pass"
				failSampleSize <- 0
			}else{
				if(dbDiagnostics$maxSampleSize < 1000){
					dbDiagnostics$sampleSizeStatus <- "Fail"
					failSampleSize <- 1

				}else{
					dbDiagnostics$sampleSizeStatus <- "Pass"
					failSampleSize <- 0
				}

			}

			numCriteria <- numCriteria + 1

			dbDiagnostics$totalFailingCriteria <- sum(dbDiagnostics$failAgeRange,
																								dbDiagnostics$failAgeAtFirstObs,
																								dbDiagnostics$failGenderCriteria,
																								dbDiagnostics$failRaceCriteria,
																								dbDiagnostics$failEthnicityCriteria,
																								dbDiagnostics$failCalendarTime,
																								dbDiagnostics$failLongitudinalCriteria,
																								dbDiagnostics$failRequiredDomain,
																								dbDiagnostics$failCondition,
																								dbDiagnostics$failDrug,
																								dbDiagnostics$failDevice,
																								dbDiagnostics$failMeasurement,
																								dbDiagnostics$failDeath,
																								dbDiagnostics$failProcedure,
																								dbDiagnostics$failMeasWithValues,
																								dbDiagnostics$failObservation,
																								dbDiagnostics$failIP,
																								dbDiagnostics$failER,
																								dbDiagnostics$failOP,
																								dbDiagnostics$failTarget,
																								dbDiagnostics$failComparator,
																								dbDiagnostics$failOutcome,
																								failSampleSize
			)

			dbDiagnostics$totalPassingCriteria <- numCriteria - dbDiagnostics$totalFailingCriteria

			dbDiagnostics <- dbDiagnostics %>%
				relocate(totalFailingCriteria,
								 totalPassingCriteria,
								 minSampleSize,
								 maxSampleSize,
								 sampleSizeStatus,
								 .before = totalPersonsInDb)

			if(i==1){
				dbDiagnosticsResults <- dbDiagnostics
			}else{
				dbDiagnosticsResultsNew <- rbind(dbDiagnosticsResults,dbDiagnostics)
				dbDiagnosticsResults <- dbDiagnosticsResultsNew
			}

		}

		write.csv(dbDiagnosticsResults, paste0(outputFolder,"/dataDiagnosticsOutput_",studyName,".csv"))

 return(dbDiagnosticsResults)
}
