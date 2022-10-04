# @file createDataDiagnosticsSettings.R
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

#' createDataDiagnosticsSettings
#'
#' @param analysisId							  The identifier for the specific analysis.
#' @param analysisName							The name for the specific analysis.
#' @param minAge										The minimum age for patients included in the analysis.
#'                               		Default is the lowest age available in the database.
#' @param maxAge										The minimum age for patients included in the analysis.
#'                                  Default is the highest age available in the database.
#' @param genderConceptIds					The required sex(es) at birth for patients included in the analysis, expressed as a vector of concepts.
#'                                  List the standard OMOP concepts for the required values as found here (https://athena.ohdsi.org/search-terms/terms?domain=Gender&standardConcept=Standard&page=1&pageSize=15&query=).
#'                                  Default is c(8507, 8532) for male and female.
#' @param raceConceptIds						The required races for patients included in the analysis, expressed as a vector of concepts, e.g. c(8515,8527).
#'                                  If no restriction leave blank, else list the standard OMOP concepts for the required values as found here (https://athena.ohdsi.org/search-terms/terms?domain=Race&standardConcept=Standard&page=1&pageSize=15&query=).
#' @param ethnicityConceptIds				The required ethnicities for patients included in the analysis, expressed as a vector of concepts, e.g. c(38003564,38003563)
#'                                  If no restriction leave blank, else list the standard OMOP concepts for the required values as found here (https://athena.ohdsi.org/search-terms/terms?domain=Ethnicity&standardConcept=Standard&page=1&pageSize=15&query=).
#' @param studyStartDate            The start date of the analysis. Date format it 'YYYYMM'. If no restriction leave blank.
#' @param studyEndDate              The end date of the analysis. Date format it 'YYYYMM'. If no restriction leave blank.
#' @param requiredDurationDays      The minimum required follow-up time in days for patients included in the analysis.
#' @param requiredDomains           The data domains required for ALL patients in the analysis, expressed as a character vector.
#'                                  Valid values are condition,drug,device,measurement,measurementValues,death,procedure,observation. Default is c("condition", "drug")
#' @param desiredDomains            The domains required for SOME patients in the analysis. Meaning you need the dataset to have these data but each person is not
#'                                  required to have a record. This is most often used to identify the presence of outcomes of interest. Valid values are condition,drug,device,measurement,measurementValues,death,procedure,observation.
#'                                  If no restriction leave blank.
#' @param requiredVisits					  The visits required for ALL patients in the study, expressed as a character vector, e.g. c("IP","OP"). Valid values are IP,OP,ER. If no restriction leave blank.
#' @param desiredVisits							The visits required for SOME patients in the study, expressed as a character vector.
#'                                  Meaning you need the dataset to have these data but each person is not required to have a record. This is most often used to identify the presence of outcomes of interest. Valid values are IP,OP,ER.
#' @param targetName			          The name of the target of interest.
#' @param targetConceptIds          A vector containing the required target concepts.
#' @param comparatorName            The name of the comparator of interest.
#' @param comparatorConceptIds      A vector containing the required comparator concepts.
#' @param outcomeName               The name of the outcome of interest.
#' @param outcomeConceptIds				  A vector containing the required outcome concepts.
#'
#' @export

createDataDiagnosticsSettings <- function(analysisId,
																					analysisName = "analysis",
																					minAge = NULL,
																					maxAge = NULL,
																					genderConceptIds = c(8507,8532),
																					raceConceptIds = NULL,
																					ethnicityConceptIds = NULL,
																					studyStartDate = NULL,
																					studyEndDate = NULL,
																					requiredDurationDays = 365,
																					requiredDomains = c("condition","drug"),
																					desiredDomains = NULL,
																					requiredVisits = NULL,
																					desiredVisits = NULL,
																					targetName = "target",
																					targetConceptIds = NULL,
																					comparatorName = NULL,
																					comparatorConceptIds = NULL,
																					outcomeName = NULL,
																					outcomeConceptIds = NULL) {

	analysis <- list()
	for (name in names(formals(createDataDiagnosticsSettings))) {
		analysis[[name]] <- get(name)
	}
	class(analysis) <- "args"
	return(analysis)

}
