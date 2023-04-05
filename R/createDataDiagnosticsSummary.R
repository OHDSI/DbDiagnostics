# @file createDataDiagnosticsSummary.R
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

#' createDataDiagnosticsSummary
#'
#' @param dbDiagnosticsResults			  A dataframe with the full data diagnostics output from the `executeDbDiagnostics` function.
#' @export

createDataDiagnosticsSummary <- function(dbDiagnosticsResults) {

	dbDiagnosticsFails <- dbDiagnosticsResults %>%
		group_by(analysisId, analysisName, databaseId) %>%
		summarise(totalFails = sum(fail))

	dbDiagnosticsSampleSize <- dbDiagnosticsResults %>%
		mutate(valueRound = round(as.numeric(value), digits = 2)) %>%
		select(analysisId, analysisName, databaseId, statistic, valueRound) %>%
		pivot_wider(names_from = statistic, values_from = valueRound) %>%
		select(analysisId, analysisName, databaseId, minSampleSize, maxSampleSize)

	dbDiagnosticsSummary <- dbDiagnosticsResults %>%
		mutate(propRound = round(as.numeric(proportion), digits = 2)) %>%
		select(analysisId, analysisName, databaseId, statistic, fail) %>%
		pivot_wider(names_from = statistic, values_from = fail) %>%
		select(-minSampleSize, -maxSampleSize) %>%
		left_join(dbDiagnosticsFails,
							by = c("analysisId"="analysisId", "analysisName"="analysisName", "databaseId"="databaseId")) %>%
		left_join(dbDiagnosticsSampleSize,
							by = c("analysisId"="analysisId", "analysisName"="analysisName", "databaseId"="databaseId")) %>%
		relocate(totalFails, minSampleSize, maxSampleSize, .before = propWithCalendarTime)

	suppressMessages(attach(dbDiagnosticsSummary))

	dbDiagnosticsSummarySort <- dbDiagnosticsSummary[order(analysisId, totalFails, -minSampleSize),]

	detach(dbDiagnosticsSummary)

	return(dbDiagnosticsSummarySort)
}
