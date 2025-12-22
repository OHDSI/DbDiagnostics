# @file dbDiagnosticsUI.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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

# Validators
testDbConnection <- function(connectionDetails) {
  tryCatch({
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    DatabaseConnector::disconnect(connection)
    return(TRUE)
  }, error = function(e) {
    print(paste("Database connection error:", e$message))
    return(FALSE)
  })
}

validateNumericList <- function(input_str) {
  if (is.null(input_str) || input_str == "") {
    return(TRUE)
  }
  numbers <- trimws(strsplit(input_str, ",")[[1]])
  all(grepl("^[0-9]+$", numbers))
}

validateYYYYMM <- function(date_str) {
  if (is.null(date_str) || date_str == "") {
    return(TRUE)
  }
  grepl("^[0-9]{6}$", date_str) &&
    as.numeric(substr(date_str, 5, 6)) >= 1 &&
    as.numeric(substr(date_str, 5, 6)) <= 12
}

parseNumericList <- function(input_str) {
  if (is.null(input_str) || input_str == "") {
    return(NULL)
  }
  as.numeric(trimws(strsplit(input_str, ",")[[1]]))
}

# Main


ui <- function(aresLink) {
	shiny::fluidPage(
	shinyjs::useShinyjs(),


  #UI Styles

  shiny::tags$head(
    shiny::tags$style(shiny::HTML("

body {
  background: #f5f5f5;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', sans-serif;
  color: #333;
}

/* Help section */

.help-icon-btn {
  display: inline-block;
  width: 18px;
  height: 18px;
  border-radius: 50%;
  background-color: #6c757d;
  color: white;
  text-align: center;
  line-height: 16px;
  font-size: 12px;
  margin-left: 8px;
  cursor: pointer;
  border: none;
  vertical-align: middle;
}

.help-icon-btn:hover {
  background-color: #0066cc;
}

.floating-help-panel {
  position: absolute;
  width: 400px;
  max-height: 500px;
  background: white;
  border: 2px solid #0066cc;
  border-radius: 4px;
  box-shadow: 0 4px 12px rgba(0,0,0,0.15);
  z-index: 10000;
  display: none;
  overflow: hidden;
  margin-top: 5px;
}

.floating-help-panel.show {
  display: block;
}

.help-panel-header {
  background-color: #0066cc;
  color: white;
  padding: 10px 15px;
  font-weight: 600;
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.help-panel-close {
  background: none;
  border: none;
  color: white;
  font-size: 20px;
  cursor: pointer;
  line-height: 20px;
}

.help-panel-body {
  padding: 15px;
  max-height: 450px;
  overflow-y: auto;
  font-size: 13px;
  line-height: 1.6;
}

.help-panel-body ul {
  margin: 10px 0 10px 20px;
}

.help-panel-body a {
  color: #0066cc;
}

.container-fluid {
  background-color: #ffffff;
  border-radius: 0;
  margin-top: 0;
  padding: 20px 30px;
  box-shadow: none;
  max-width: 1600px;
}

.row {
  display: flex;
  flex-wrap: wrap;
}

.row > [class*='col-'] {
  display: flex;
  flex-direction: column;
}

h1.title {
  color: #222;
  font-weight: 400;
  font-size: 24px;
  margin-bottom: 20px;
  padding-bottom: 10px;
  border-bottom: 2px solid #333;
}

h2 {
  color: #222;
  border-bottom: 1px solid #ddd;
  padding-bottom: 8px;
  margin-bottom: 20px;
  font-weight: 400;
  font-size: 20px;
}

h3 {
  color: #333;
  margin-top: 0;
  margin-bottom: 15px;
  font-weight: 550;
  font-size: 16px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  display: flex;
  align-items: center;
}

h4 {
  color: #444;
  margin-top: 12px;
  margin-bottom: 8px;
  font-size: 14px;
  font-weight: 600;
}

h5 {
  color: #333;
  font-weight: 600;
  font-size: 14px;
  margin-bottom: 10px;
}

.section-container {
  background-color: #fff;
  padding: 20px;
  border-radius: 0;
  margin-bottom: 15px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.08);
  border: 1px solid #ddd;
  flex: 1;
}

.btn-primary {
  background-color: #0066cc;
  border-color: #0066cc;
  padding: 10px 24px;
  font-weight: 500;
  font-size: 14px;
  transition: background-color 0.2s;
  border-radius: 2px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.btn-primary:hover {
  background-color: #0052a3;
  border-color: #0052a3;
  transform: none;
  box-shadow: none;
}

.btn-primary:active {
  background-color: #004080;
  border-color: #004080;
}

.btn-add {
	background: rgb(107,114,128);
	border-color:  rgb(107,114,128);
}

.shiny-input-container {
  margin-bottom: 15px;
}

.shiny-input-container label {
  font-weight: 500;
  color: #555;
  font-size: 13px;
  margin-bottom: 5px;
}

.input-group {
  border-left: 3px solid #0066cc;
  padding-left: 15px;
  margin-bottom: 20px;
  background-color: #fafafa;
  padding: 12px 12px 12px 15px;
}

.required-field label:after {
  content: ' *';
  color: #d9534f;
  font-weight: normal;
}

.required-field h4:after {
  content: ' *';
  color: #d9534f;
  font-weight: normal;
}

.validation-error {
  color: #d9534f;
  font-size: 12px;
}

.modal-title {
  color: #222;
  font-size: 18px;
  font-weight: 500;
}

select, input[type='text'], input[type='number'], textarea {
  border: 1px solid #ccc;
  border-radius: 2px;
  padding: 6px 10px;
  transition: border-color 0.15s;
  font-size: 14px;
  background-color: #fff;
}

select:focus, input[type='text']:focus, input[type='number']:focus, textarea:focus {
  border-color: #0066cc;
  box-shadow: none;
  outline: none;
}

/* Slider */
.irs-bar {
  background: #0066cc;
  border-top: 1px solid #0066cc;
  border-bottom: 1px solid #0066cc;
}

.irs--shiny .irs-bar {
	border-top: #0066cc;
	border-bottom: #0066cc;
	background: #0066cc;
}

.irs-bar-edge {
  background: #0066cc;
  border: 1px solid #0066cc;
}

.irs-handle {
  background: #fff;
  border: 2px solid #0066cc;
  box-shadow: none;
}

.irs-from, .irs-to, .irs-single {
  background: #0066cc;
  font-weight: normal;
}

input[type='checkbox'] {
  margin-right: 6px;
}

/* Connection status */

.db-status {
  padding: 8px 15px;
  border-radius: 0;
  margin-bottom: 20px;
  font-weight: 500;
  display: inline-block;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.db-connected {
  background-color: rgb(107,114,128);
  color: #fff;
  border: none;
}

.db-disconnected {
  background-color: #d9534f;
  color: #fff;
  border: none;
}


.shiny-input-container {
  width: 100% !important;
}


.results-display {
  background-color: #f9f9f9;
  padding: 15px;
  border-radius: 0;
  margin-top: 15px;
  border: 1px solid #ddd;
  font-family: 'Consolas', 'Monaco', monospace;
  font-size: 13px;
}

.results-table {
  margin-top: 20px;
}

.summary-card {
  background-color: #f9f9f9;
  padding: 15px;
  border-radius: 0;
  border-left: 3px solid #0066cc;
  margin-bottom: 15px;
  border-top: 1px solid #ddd;
  border-right: 1px solid #ddd;
  border-bottom: 1px solid #ddd;
}

.summary-card h5 {
  margin-top: 0;
  color: #222;
  font-weight: 600;
  margin-bottom: 12px;
  text-transform: uppercase;
  font-size: 13px;
  letter-spacing: 0.5px;
}

.summary-card p {
  margin-bottom: 6px;
  color: #555;
  font-size: 13px;
  line-height: 1.5;
}

.summary-card strong {
  color: #333;
  font-weight: 600;
}

/* DataTable */
.dataTables_wrapper {
  margin-top: 20px;
  font-size: 13px;
  background-color: #fff;
  border: 1px solid #ddd;
  box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.08);
  padding: 0;
}

.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter {
  padding: 15px 20px;
  background-color: #fafafa;
  border-bottom: 1px solid #e5e5e5;
}

.dataTables_wrapper .dataTables_length {
  float: left;
}

.dataTables_wrapper .dataTables_filter {
  float: right;
}

.dataTables_filter label {
  font-weight: 500;
  color: #555;
  font-size: 13px;
  display: flex;
  align-items: center;
  gap: 8px;
}

.dataTables_filter input {
  border: 1px solid #ccc;
  border-radius: 2px;
  padding: 6px 12px;
  font-size: 13px;
  width: 200px;
  transition: all 0.15s;
  background-color: #fff;
}

.dataTables_filter input:focus {
  border-color: #0066cc;
  outline: none;
  box-shadow: 0 0 0 2px rgba(0, 102, 204, 0.1);
  width: 250px;
}

.dataTables_length label {
  font-weight: 500;
  color: #555;
  font-size: 13px;
  display: flex;
  align-items: center;
  gap: 8px;
}

.dataTables_length select {
  border: 1px solid #ccc;
  border-radius: 2px;
  padding: 6px 30px 6px 10px;
  font-size: 13px;
  background-color: #fff;
  margin: 0 5px;
}

.dataTables_wrapper .dataTables_scroll {
  clear: both;
}

.dataTables_wrapper .dataTables_scrollHead {
  border-bottom: 2px solid #0066cc;
}

.dataTables_wrapper .dataTables_scrollBody {
  border-left: none;
  border-right: none;
}

table.dataTable {
  width: 100% !important;
  border-collapse: collapse;
  margin: 0 !important;
}

table.dataTable thead {
  background: linear-gradient(to bottom, #f8f9fa, #f1f3f5);
}

table.dataTable thead th {
  font-weight: 600;
  color: #333;
  border-bottom: none;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  padding: 12px 10px;
  position: relative;
  white-space: nowrap;
}

table.dataTable thead .sorting,
table.dataTable thead .sorting_asc,
table.dataTable thead .sorting_desc {
  cursor: pointer;
  position: relative;
  padding-right: 25px;
}

table.dataTable thead .sorting:after,
table.dataTable thead .sorting_asc:after,
table.dataTable thead .sorting_desc:after {
  position: absolute;
  right: 8px;
  top: 50%;
  transform: translateY(-50%);
  font-family: 'Font Awesome 5 Free', 'FontAwesome';
  opacity: 0.5;
  font-size: 11px;
}

table.dataTable thead .sorting:after {
  content: '\f0dc';
  opacity: 0.3;
}

table.dataTable thead .sorting_asc:after {
  content: '\f0de';
  color: #0066cc;
  opacity: 1;
}

table.dataTable thead .sorting_desc:after {
  content: '\f0dd';
  color: #0066cc;
  opacity: 1;
}

table.dataTable thead .sorting:hover:after {
  opacity: 0.6;
}

table.dataTable tbody tr {
  transition: background-color 0.15s;
}

table.dataTable tbody tr:nth-child(even) {
  background-color: #fafafa;
}

table.dataTable tbody tr:hover {
  background-color: #e8f3ff;
}

table.dataTable tbody td {
  font-size: 13px;
  padding: 10px;
  color: #555;
  border-top: 1px solid #efefef;
}

table.dataTable tbody tr:first-child td {
  border-top: none;
}

table.dataTable tbody tr.selected {
  background-color: #d4e3f4;
}

table.dataTable tbody tr.selected:hover {
  background-color: #c4d9ef;
}

.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate {
  padding: 15px 20px;
  background-color: #fafafa;
  border-top: 1px solid #e5e5e5;
}

.dataTables_info {
  float: left;
  color: #666;
  font-size: 13px;
  padding-top: 8px;
}

.dataTables_paginate {
  float: right;
}

.dataTables_paginate .paginate_button {
  background: #fff !important;
  border: 1px solid #ddd !important;
  color: #555 !important;
  padding: 6px 12px !important;
  margin: 0 2px !important;
  border-radius: 2px !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  cursor: pointer !important;
  transition: all 0.15s !important;
  text-decoration: none !important;
  display: inline-block !important;
}

.dataTables_paginate .paginate_button:hover:not(.disabled):not(.current) {
  background: #0066cc !important;
  border-color: #0066cc !important;
  color: #fff !important;
  transform: translateY(-1px);
  box-shadow: 0 2px 4px rgba(0,102,204,0.2);
}

.dataTables_paginate .paginate_button.current,
.dataTables_paginate .paginate_button.current:hover {
  background: #0066cc !important;
  border-color: #0066cc !important;
  color: #fff !important;
  cursor: default !important;
  font-weight: 600 !important;
}

.dataTables_paginate .paginate_button.disabled,
.dataTables_paginate .paginate_button.disabled:hover {
  background: #f5f5f5 !important;
  border-color: #ddd !important;
  color: #999 !important;
  cursor: not-allowed !important;
}

.dataTables_paginate .paginate_button.previous,
.dataTables_paginate .paginate_button.next {
  font-weight: 600 !important;
  padding: 6px 16px !important;
}


.dataTables_empty {
  text-align: center;
  padding: 40px !important;
  color: #999;
  font-style: italic;
  background-color: #fafafa;
}

.dataTables_processing {
  background-color: #0066cc;
  color: white;
  border: none;
  padding: 10px 20px;
  border-radius: 2px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.2);
  font-size: 14px;
  font-weight: 500;
  top: 50%;
}

.dataTables_scrollBody::-webkit-scrollbar {
  height: 8px;
  width: 8px;
}

.dataTables_scrollBody::-webkit-scrollbar-track {
  background: #f1f1f1;
}

.dataTables_scrollBody::-webkit-scrollbar-thumb {
  background: #999;
  border-radius: 4px;
}

.dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
  background: #666;
}

.dt-buttons {
  padding: 15px 20px;
  background-color: #fafafa;
  border-bottom: 1px solid #e5e5e5;
}

.dt-button {
  background: #fff;
  border: 1px solid #ddd;
  color: #555;
  padding: 6px 12px;
  margin-right: 5px;
  border-radius: 2px;
  font-size: 13px;
  font-weight: 500;
  cursor: pointer;
  transition: all 0.15s;
}

.dt-button:hover {
  background: #0066cc;
  border-color: #0066cc;
  color: #fff;
}

.modal-content {
  border-radius: 0;
  border: 1px solid #999;
  box-shadow: 0 3px 9px rgba(0,0,0,0.5);
}

.modal-header {
  background-color: #f5f5f5;
  border-bottom: 1px solid #ddd;
  border-radius: 0;
}

.modal-footer {
  background-color: #f5f5f5;
  border-top: 1px solid #ddd;
  border-radius: 0;
}

input[type='checkbox'] {
  appearance: none;
  -webkit-appearance: none;
  -moz-appearance: none;
  width: 18px;
  height: 18px;
  border: 2px solid #ccc;
  border-radius: 2px;
  background-color: #fff;
  cursor: pointer;
  position: relative;
  transition: all 0.15s;
  margin-right: 8px;
  vertical-align: middle;
  margin-top: -2px;
}

input[type='checkbox']:hover {
  border-color: #999;
}

input[type='checkbox']:focus {
  outline: none;
  border-color: #0066cc;
  box-shadow: 0 0 0 2px rgba(0, 102, 204, 0.1);
}

input[type='checkbox']:checked {
  background-color: #0066cc;
  border-color: #0066cc;
}

input[type='checkbox']:checked::after {
  content: '';
  position: absolute;
  top: 2px;
  left: 5px;
  width: 5px;
  height: 9px;
  border: solid white;
  border-width: 0 2px 2px 0;
  transform: rotate(45deg);
}

input[type='checkbox']:disabled {
  background-color: #f5f5f5;
  border-color: #ddd;
  cursor: not-allowed;
}

input[type='checkbox']:disabled:checked {
  background-color: #999;
  border-color: #999;
}

input[type='checkbox'] + label,
label:has(+ input[type='checkbox']) {
  cursor: pointer;
  user-select: none;
}

.checkbox label {
display: flex;
flex-direction: row;
align-items: center;
}

.analysis-item {
  background-color: #fafafa;
  border: 1px solid #ddd;
  margin-bottom: 10px;
  border-radius: 2px;
  border-left: 3px solid #0066cc;
}

.analysis-header {
  padding: 12px 15px;
  cursor: pointer;
  display: flex;
  justify-content: space-between;
  align-items: center;
  background-color: #fafafa;
  border-bottom: 1px solid #ddd;
}

.analysis-header:hover {
  background-color: #efefef;
}

.analysis-details {
  padding: 15px;
  display: none;
}

.analysis-details.show {
  display: block;
}

.analysis-actions {
  display: flex;
  gap: 10px;
}

.btn-edit, .btn-remove {
  padding: 4px 12px;
  font-size: 12px;
  border-radius: 2px;
}

.btn-edit {
  background-color: #0066cc;
  border-color: #0066cc;
  color: white;
}

.btn-remove {
  border: 2px solid #C82333;
  background: inherit;
  color: black;
}

.btn-remove:hover {
  border: 2px solid #C82333;
  background: #C82333;
  color: white;
}

.analysis-count {
  background-color: #0066cc;
  color: white;
  padding: 2px 8px;
  border-radius: 2px;
  font-size: 12px;
  margin-left: 10px;
}

#downloadPDF {
  background-color: #0066cc;
  border-color: #0066cc;
  color: white;
  padding: 8px 16px;
  border-radius: 2px;
  font-weight: 500;
  font-size: 13px;
}

#downloadPDF:hover {
  background-color: #0052a3;
  border-color: #0052a3;
}

@media (max-width: 768px) {
  .container-fluid {
    padding: 15px;
  }

  h1.title {
    font-size: 20px;
  }

  .section-container {
    padding: 15px;
  }

  .dataTables_length,
  .dataTables_filter,
  .dataTables_info,
  .dataTables_paginate {
    float: none !important;
    text-align: center;
    margin-bottom: 10px;
  }

  .dataTables_filter input {
    width: 100%;
    max-width: 200px;
  }
}

    ")),

    # For tooltip windows positioning
    shiny::tags$script(shiny::HTML("
$(document).ready(function() {
  $(document).on('click', '.help-icon-btn', function(e) {
    e.preventDefault();
    e.stopPropagation();
    var helpTitle = $(this).data('help-title');
    var helpContent = $(this).data('help-content');

    $('.floating-help-panel').removeClass('show');

    var $button = $(this);
    var buttonOffset = $button.offset();
    var buttonHeight = $button.outerHeight();

    var fieldContainer = $(this).closest('.shiny-input-container, .required-field, .field-with-help, div');
    var containerOffset = fieldContainer.offset();

    $('#floating-help-title').text(helpTitle);
    $('#floating-help-content').html(helpContent);

    $('.floating-help-panel')
      .css({
        'position': 'absolute',
        'top': (buttonOffset.top + buttonHeight + 5) + 'px',  // Below the button
        'left': containerOffset.left + 'px'  // Align with container's left edge
      })
      .addClass('show');
  });



  $(document).on('click', '.help-panel-close', function() {
    $('.floating-help-panel').removeClass('show');
  });

  $(document).on('click', function(e) {
    if (!$(e.target).closest('.floating-help-panel, .help-icon-btn').length) {
      $('.floating-help-panel').removeClass('show');
    }
  });
});
"))
  ),

  shiny::fluidRow(
  	shiny::column(10, offset = 1,

           shiny::tags$div(
             class = "floating-help-panel",
             shiny::tags$div(
               class = "help-panel-header",
               shiny::tags$span(id = "floating-help-title", "Help"),
               shiny::tags$button(class = "help-panel-close", shiny::HTML("&times;"))
             ),
             shiny::tags$div(
               id = "floating-help-content",
               class = "help-panel-body"
             )
           ),
           shiny::tags$div(
             style = "display: flex; align-items: center; margin-bottom: 20px;",
             shiny::tags$img(
               src = "https://avatars.githubusercontent.com/u/6570077?s=200&v=4",
               alt = "Logo",
               style = "height: 35px; width: auto; margin-right: 20px;"
             ),
             shiny::h1("Database Diagnostics Results Viewer", class = "title", style = "margin: 0; border: none; padding: 0;")
           ),
           shiny::tags$hr(style = "border-top: 2px solid #333; margin-top: 0; margin-bottom: 20px;"),

           shiny::uiOutput("dbStatus"),

           # First row
           shiny::fluidRow(
           	shiny::column(12,
                    shiny::div(class = "section-container",
                        shiny::div(class = "section-header",
                            shiny::h3(
                              "Analysis Information"

                            )
                        ),
                        shiny::fluidRow(
                        	shiny::column(6,
                                 shiny::div(class = "required-field",
                                     shiny::tags$label(
                                       "Analysis ID:"

                                     ),
                                     shiny::textInput("analysisId", NULL, placeholder = "Enter numerical ID")
                                 ),
                                 shiny::uiOutput("analysisIdError")
                          ),
                        	shiny::column(6,
                                 shiny::div(class = "required-field",
                                     shiny::textInput("analysisName", "Analysis Name:",
                                               placeholder = "Enter analysis name")
                                 ),
                                 shiny::uiOutput("analysisNameError")
                          )
                        )
                    )
             )
           ),

           # Second row
           shiny::fluidRow(
           	shiny::column(6,
                    shiny::div(class = "section-container",
                        shiny::h3("Demographics",

                           shiny::tags$button(
                             class = "help-icon-btn",
                             style = "margin-left: 10px;",
                             `data-help-title` = "Demographics",
                             `data-help-content` = sprintf("

					 				 			 	<p>Fill in the demographic requirements for all persons in the study.</p>
					 				 			 	<p>Race and Ethnicity values should be inputted as comma separated lists of concepts.</p>
					 				 			 	<p>OMOP concepts for race can be found here: <a href='https://athena.ohdsi.org/search-terms/terms?domain=Race&standardConcept=Standard&page=1&pageSize=15&query=' target='_blank'>View concepts</a></p>
					 				 			 	<p>OMOP concepts for ethnicity can be found here: <a href='https://athena.ohdsi.org/search-terms/terms?domain=Ethnicity&standardConcept=Standard&page=1&pageSize=15&query=' target='_blank'>View concepts</a></p>
					 				 			 	<p>Please see the ARES application for race distribution across our licensed databases standardized to the OMOP CDM: <a href='%s' target='_blank'>View Ares</a> </p>
					 				 			 	", aresLink),
                             shiny::HTML("?")
                           )
                        ),
                        shiny::sliderInput("ageRange", "Age Range:",
                                    min = 0, width = '100%', max = 150, value = c(0, 150)),
                        shiny::fluidRow(
                        	shiny::column(6,
                                 shiny::h4("Gender"),
                                 shiny::checkboxInput("genderMale", "Male (8507)", value = TRUE),
                                 shiny::checkboxInput("genderFemale", "Female (8532)", value = TRUE)
                          ),
                        	shiny::column(6,
                                 shiny::textInput("raceConceptIds", "Race Concept IDs:",
                                           placeholder = "e.g., 12345, 54321"),
                                 shiny::uiOutput("raceError"),
                                 shiny::textInput("ethnicityConceptIds", "Ethnicity Concept IDs:",
                                           placeholder = "e.g., 12345, 54321"),
                                 shiny::uiOutput("ethnicityError")
                          )
                        )
                    )
             ),
           	shiny::column(6,
                    shiny::div(class = "section-container",
                        shiny::h3("Target and Comparator Requirements",
                           shiny::tags$button(
                             class = "help-icon-btn",
                             style = "margin-left: 10px;",
                             `data-help-title` = "Target and Comparator",
                             `data-help-content` = "
					 				 			 	<p>Fill in the target, comparator, and indication of interest in the form of comma-separated lists of concepts.
					 				 			 	These can be easily exported from an ATLAS concept set using the export tab.
					 				 			 	It is important to note that Database Diagnostics does not resolve the concept set so you have to paste in ALL included concepts,
					 				 			 	not just those in the concept set definition.</p>

					 				 			 	<p>The drug ingredient option is useful when the target and/or comparator are drug exposure phenotypes.
					 				 			 	Using drug ingredients reduces the number of people that may be counted twice as it looks at the number of people exposed to the drug ingredient rather than each formulation of the drug.</p>

					 				 			 	<p>In this section <strong>only the target is required </strong>. If you are unsure of the comparator or are performing a characterization, you only need to fill in the target.</p>
					 				 			 	",
                             shiny::HTML("?")
                           )

                        ),

                        shiny::div(class = "input-group",
                            shiny::h4("Indication"),
                            shiny::fluidRow(
                            	shiny::column(6,
                                     shiny::textInput("indicationConcepts", "Concept IDs:",
                                               placeholder = "e.g., 12345, 54321"),
                                     shiny::uiOutput("indicationConceptsError")
                              ),
                            	shiny::column(6,
                                     shiny::textInput("indicationName", "Name:"
                                     ),
                                     shiny::uiOutput("indicationNameError")
                              )
                            ),

                        ),

                        shiny::div(class = "input-group",
                        		shiny::div(class = "required-field",
                        				shiny::h4("Target"),
                        		),
                            shiny::fluidRow(
                            	shiny::column(6,
                                     shiny::textInput("targetConcepts", "Concept IDs:",
                                               placeholder = "e.g., 12345, 54321"),
                                     shiny::uiOutput("targetConceptsError")
                              ),
                            	shiny::column(6,
                                     shiny::textInput("targetName", "Name:",
                                               placeholder = "Default: 'target'"),
                                     shiny::uiOutput("targetNameError")
                              )
                            ),
                        		shiny::fluidRow(
                        			shiny::column(6,
                                     shiny::checkboxInput("targetUseDrugEra", "Drug Ingredients Only", value = FALSE),
                              )
                            )
                        ),

                        shiny::div(class = "input-group",
                            shiny::h4("Comparator"),
                            shiny::fluidRow(
                            	shiny::column(6,
                                     shiny::textInput("comparatorConcepts", "Concept IDs:",
                                               placeholder = "e.g., 12345, 54321"),
                                     shiny::uiOutput("comparatorConceptsError")
                              ),
                            	shiny::column(6,
                                     shiny::textInput("comparatorName", "Name:"
                                     ),
                                     shiny::uiOutput("comparatorNameError")
                              )
                            ),
                            shiny::fluidRow(
                            	shiny::column(6,
                                     shiny::checkboxInput("comparatorUseDrugEra", "Drug Ingredients Only", value = FALSE),
                              )
                            )
                        )
                    )
             )
           ),

           # Third row
           shiny::fluidRow(
           	shiny::column(6,
                    shiny::div(class = "section-container",
                        shiny::h3("Domains and Visits"),
                        shiny::fluidRow(
                        	shiny::column(6,
                                 shiny::tags$div(
                                   class = "shiny-input-container",
                                   shiny::tags$label(
                                     "Required Domains:",
                                     shiny::tags$button(
                                       class = "help-icon-btn",
                                       `data-help-title` = "Required Domains",
                                       `data-help-content` = "<p><strong>Required domains</strong> refer to data domains in which all patients in the analysis need data.
                                       For example, if your target is patients with Type 2 Diabetes Mellitus with exposure to canagliflozin, then all patients need at least one condition record (diabetes) and at least one drug record (canagliflozin) to be included in the study.
                                       Valid values are condition,drug,device,measurement,measurementValues, <br> death,procedure,observation.
                                       If no restriction leave blank. </p>",
                                       shiny::HTML("?")
                                     )
                                   ),
                                   shiny::selectInput("requiredDomains", NULL,
                                               choices = list("Condition" = "condition",
                                                              "Drug" = "drug",
                                                              "Procedure" = "procedure",
                                                              "Observation" = "observation",
                                                              "Measurement" = "measurement",
                                                              "Device" = "device",
                                                              "Death" = "death",
                                                              "Measurement Values" = "measurementValues"),
                                               multiple = TRUE,
                                               selected = c("condition", "drug"))
                                 )
                          ),
                        	shiny::column(6,
                                 shiny::tags$div(
                                   class = "shiny-input-container",
                                   shiny::tags$label(
                                     "Desired Domains:",
                                     shiny::tags$button(
                                       class = "help-icon-btn",
                                       `data-help-title` = "Desired Domains",
                                       `data-help-content` = "<p> <strong>Desired domains</strong> refer to data domains in which only some patients in the analysis need data, meaning data domains needed for the outcome.
                                       For example, if your outcome is a surgical procedure, then procedures must be available in the data to observe the outcome so the desired domain would be 'procedure'.
                                       Valid values are condition,drug,device,measurement,measurementValues, <br> death,procedure,observation. If no restriction leave blank. </p>",
                                       shiny::HTML("?")
                                     )
                                   ),
                                   shiny::selectInput("desiredDomains", NULL,
                                               choices = list("Condition" = "condition",
                                                              "Drug" = "drug",
                                                              "Procedure" = "procedure",
                                                              "Observation" = "observation",
                                                              "Measurement" = "measurement",
                                                              "Device" = "device",
                                                              "Death" = "death",
                                                              "Measurement Values" = "measurementValues"),
                                               multiple = TRUE)
                                 )
                          )

                        ),
                        shiny::fluidRow(
                        	shiny::column(6,
                                 shiny::tags$div(
                                   class = "shiny-input-container",
                                   shiny::tags$label(
                                     "Required Visits:",
                                     shiny::tags$button(
                                       class = "help-icon-btn",
                                       `data-help-title` = "Required Visits",
                                       `data-help-content` = "<p><strong>Required visits</strong> refer to requirements for all patients in the analysis, meaning requirements for the target and comparator. For example, if your target or comparator definition requires an IP visit, then put 'IP' in this field. Valid values are IP,OP,ER. If no restriction leave blank.</p>",
                                       shiny::HTML("?")
                                     )
                                   ),
                                   shiny::selectInput("requiredVisits", NULL,
                                               choices = list("Inpatient" = "IP",
                                                              "Outpatient" = "OP",
                                                              "Emergency" = "ER"),
                                               multiple = TRUE)
                                 )),
                        	shiny::column(6,
                                 shiny::tags$div(
                                   class = "shiny-input-container",
                                   shiny::tags$label(
                                     "Desired Visits:",
                                     shiny::tags$button(
                                       class = "help-icon-btn",
                                       `data-help-title` = "Desired Visits",
                                       `data-help-content` = "<p> <strong>Desired visits</strong> refer to requirements for only some patients in the analysis, meaning requirements for the outcome.
                                       For example, if your outcome must be observed in the inpatient setting, put IP here.
                                       Valid values are IP,OP,ER. If no restrictions leave blank. </p>",
                                       shiny::HTML("?")
                                     )
                                   ),
                                   shiny::selectInput("desiredVisits", NULL,
                                               choices = list("Inpatient" = "IP",
                                                              "Outpatient" = "OP",
                                                              "Emergency" = "ER"),
                                               multiple = TRUE)
                                 )

                          )
                        )
                    )
             ),
           	shiny::column(6,
                    shiny::div(class = "section-container",
                        shiny::h3("Temporal Criteria",

                           shiny::tags$button(
                             class = "help-icon-btn",
                             style = "margin-left: 10px;",
                             `data-help-title` = "Temporal Criteria",
                             `data-help-content` = "
					 				 			 	<p>Choose the amount of days required for lookback (defaulted to 365) and the time period of the study.
					 				 			 	If no time period is chosen Database Diagnostics will default to all-time available in the database.</p>


					 				 			 	",
                             shiny::HTML("?")
                           )

                        ),
                        shiny::fluidRow(
                        	shiny::column(6,
                                 shiny::textInput("studyStartDate", "Start Date (YYYYMM):",
                                           placeholder = "e.g., 202301"),
                                 shiny::uiOutput("startDateError")
                          ),
                        	shiny::column(6,
                                 shiny::textInput("studyEndDate", "End Date (YYYYMM):",
                                           placeholder = "e.g., 202312"),
                                 shiny::uiOutput("endDateError")
                          )
                        ),
                        shiny::numericInput("followUpDays", "Required Follow-up (days):",
                                     value = 365, min = 0)
                    )
             )
           ),

           # Fourth row
           shiny::fluidRow(
           	shiny::column(12,
                    shiny::div(class = "section-container",
                        shiny::h3("Outcome Requirements"),
                        shiny::fluidRow(
                        	shiny::column(6,
                                 shiny::textInput("outcomeConcepts", "Outcome Concept IDs:",
                                           placeholder = "e.g., 12345, 54321"),
                                 shiny::uiOutput("outcomeConceptsError")
                          ),
                        	shiny::column(6,
                                 shiny::textInput("outcomeName", "Outcome Name:"
                                 ),
                                 shiny::uiOutput("outcomeNameError")
                          )
                        )
                    )
             )
           ),

           shiny::fluidRow(shiny::column(12,

                           shiny::actionButton("addToList", "Add to List",
                                        class = "btn-primary btn-lg btn-add",
                                        style = "width: 100%")
           )
           ),

           # Analyses queue
           shiny::fluidRow(
           	shiny::column(12,
                    shiny::div(class = "section-container", style = "margin-top: 30px;",
                        shiny::h3("Queued Analyses"),
                        shiny::uiOutput("analysesListDisplay")
                    )
             )
           ),

           # Run Analysis Button
           shiny::fluidRow(
           	shiny::column(12,
                    shiny::div(style = "text-align: center; margin-top: 30px;",
                        shiny::actionButton("runAnalysis", "Run All Analyses",
                                     class = "btn-primary btn-lg")
                    )
             )
           ),
    )
  )
)}


server <- function(input, output, session, connectionDetails, aresLink, resultsDatabaseSchema, resultsTableName) {

  dbConnected <- reactiveVal(FALSE)
  analysesList <- reactiveVal(list())

  globalResults <- NULL
  globalSettings <- NULL

  shiny::observe({
    status <- testDbConnection(connectionDetails)
    dbConnected(status)
  })

  # App info/error messages

  output$dbStatus <- shiny::renderUI({
    if (dbConnected()) {
    	shiny::div(class = "db-status db-connected",
      		shiny::icon("check-circle"),
          " Database Connected")
    } else {
    	shiny::div(class = "db-status db-disconnected",
      		shiny::icon("times-circle"),
          " Database Disconnected - Check configuration")
    }
  })

  output$analysisIdError <- renderUI({
    if (!is.null(input$analysisId) && input$analysisId != "") {
      if (!grepl("^[0-9]+$", input$analysisId)) {
      	shiny::div(class = "validation-error",
        		shiny::icon("exclamation-triangle"),
            " Analysis ID must contain only numeric values")
      }
    }
  })

  output$analysisNameError <- renderUI({
    if (input$analysisId != "" && input$analysisName == "") {
    	shiny::div(class = "validation-error",
      		shiny::icon("exclamation-triangle"),
          " Analysis Name is required")
    }
  })

  output$indicationNameError <- renderUI({
    if (input$indicationConcepts != "" && input$indicationName == "") {
    	shiny::div(class = "validation-error",
      		shiny::icon("exclamation-triangle"),
          " Indication Name is required")
    }
  })

  output$targetError <- renderUI({
  	if (input$targetConcepts == "") {
  		shiny::div(class = "validation-error",
  				shiny::icon("exclamation-triangle"),
  				" Target is a mandatory field ")
  	}
  })

  output$targetNameError <- renderUI({
    if (input$targetConcepts != "" && input$targetName == "") {
    	shiny::div(class = "validation-error",
      		shiny::icon("exclamation-triangle"),
          " Target Name is required")
    }
  })

  output$comparatorNameError <- renderUI({
    if (input$comparatorConcepts != "" && input$comparatorName == "") {
    	shiny::div(class = "validation-error",
      		shiny::icon("exclamation-triangle"),
          " Comparator Name is required")
    }
  })

  output$outcomeNameError <- renderUI({
    if (input$outcomeConcepts != "" && input$outcomeName == "") {
    	shiny::div(class = "validation-error",
      		shiny::icon("exclamation-triangle"),
          " Outcome Name is required")
    }
  })

  output$raceError <- renderUI({
    if (!is.null(input$raceConceptIds) && input$raceConceptIds != "") {
      if (!validateNumericList(input$raceConceptIds)) {
      	shiny::div(class = "validation-error",
        		shiny::icon("exclamation-triangle"),
            " Must be numerical values separated by commas")
      }
    }
  })

  output$ethnicityError <- renderUI({
    if (!is.null(input$ethnicityConceptIds) && input$ethnicityConceptIds != "") {
      if (!validateNumericList(input$ethnicityConceptIds)) {
      	shiny::div(class = "validation-error",
        		shiny::icon("exclamation-triangle"),
            " Must be numerical values separated by commas")
      }
    }
  })

  output$startDateError <- renderUI({
    if (!is.null(input$studyStartDate) && input$studyStartDate != "") {
      if (!validateYYYYMM(input$studyStartDate)) {
      	shiny::div(class = "validation-error",
        		shiny::icon("exclamation-triangle"),
            " Date must be in YYYYMM format (e.g., 202301)")
      }
    }
  })

  output$endDateError <- renderUI({
    if (!is.null(input$studyEndDate) && input$studyEndDate != "") {
      if (!validateYYYYMM(input$studyEndDate)) {
      	shiny::div(class = "validation-error",
        		shiny::icon("exclamation-triangle"),
            " Date must be in YYYYMM format (e.g., 202312)")
      }
    }
  })

  output$indicationConceptsError <- renderUI({
    if (!is.null(input$indicationConcepts) && input$indicationConcepts != "") {
      if (!validateNumericList(input$indicationConcepts)) {
      	shiny::div(class = "validation-error",
        		shiny::icon("exclamation-triangle"),
            " Must be numerical values separated by commas")
      }
    }
  })

  output$targetConceptsError <- renderUI({
    if (!is.null(input$targetConcepts) && input$targetConcepts != "") {
      if (!validateNumericList(input$targetConcepts)) {
      	shiny::div(class = "validation-error",
            shiny::icon("exclamation-triangle"),
            " Must be numerical values separated by commas")
      }
    }
  })

  output$comparatorConceptsError <- renderUI({
    if (!is.null(input$comparatorConcepts) && input$comparatorConcepts != "") {
      if (!validateNumericList(input$comparatorConcepts)) {
      	shiny::div(class = "validation-error",
            shiny::icon("exclamation-triangle"),
            " Must be numerical values separated by commas")
      }
    }
  })

  output$outcomeConceptsError <- renderUI({
    if (!is.null(input$outcomeConcepts) && input$outcomeConcepts != "") {
      if (!validateNumericList(input$outcomeConcepts)) {
      	shiny::div(class = "validation-error",
            shiny::icon("exclamation-triangle"),
            " Must be numerical values separated by commas")
      }
    }
  })


  # Handler for adding to list
  shiny::observeEvent(input$addToList, {
    errors <- c()

    if (is.null(input$analysisId) || input$analysisId == "") {
      errors <- c(errors, "Analysis ID is required")
    } else if (!grepl("^[0-9]+$", input$analysisId)) {
      errors <- c(errors, "Analysis ID must be numeric")
    }

    if (is.null(input$analysisName) || input$analysisName == "") {
      errors <- c(errors, "Analysis Name is required")
    }

    currentList <- analysesList()
    if (any(sapply(currentList, function(x) x$analysisName == input$analysisName))) {
      errors <- c(errors, "Analysis Name must be unique")
    }
    if (any(sapply(currentList, function(x) x$analysisId == input$analysisId))) {
      errors <- c(errors, "Analysis ID must be unique")
    }

    if (!is.null(input$raceConceptIds) && input$raceConceptIds != "") {
      if (!validateNumericList(input$raceConceptIds)) {
        errors <- c(errors, "Race Concept IDs must be numerical values separated by commas")
      }
    }

    if (!is.null(input$ethnicityConceptIds) && input$ethnicityConceptIds != "") {
      if (!validateNumericList(input$ethnicityConceptIds)) {
        errors <- c(errors, "Ethnicity Concept IDs must be numerical values separated by commas")
      }
    }

    if (!is.null(input$studyStartDate) && input$studyStartDate != "") {
      if (!validateYYYYMM(input$studyStartDate)) {
        errors <- c(errors, "Study Start Date must be in YYYYMM format (e.g., 202301)")
      }
    }

    if (!is.null(input$studyEndDate) && input$studyEndDate != "") {
      if (!validateYYYYMM(input$studyEndDate)) {
        errors <- c(errors, "Study End Date must be in YYYYMM format (e.g., 202312)")
      }
    }

    if (!is.null(input$studyStartDate) &&
      input$studyStartDate != "" &&
      !is.null(input$studyEndDate) &&
      input$studyEndDate != "" &&
      validateYYYYMM(input$studyStartDate) &&
      validateYYYYMM(input$studyEndDate)) {
      if (as.numeric(input$studyStartDate) > as.numeric(input$studyEndDate)) {
        errors <- c(errors, "Study Start Date cannot be after Study End Date")
      }
    }

    if (!is.null(input$indicationConcepts) && input$indicationConcepts != "") {
      if (!validateNumericList(input$indicationConcepts)) {
        errors <- c(errors, "Indication Concept IDs must be numerical values separated by commas")
      }
      if (is.null(input$indicationName) || input$indicationName == "") {
        errors <- c(errors, "Indication Name is required when Indication Concept IDs are provided")
      }
    }

    if (!is.null(input$targetConcepts) && input$targetConcepts != "") {
      if (!validateNumericList(input$targetConcepts)) {
        errors <- c(errors, "Target Concept IDs must be numerical values separated by commas")
      }
      if (is.null(input$targetName) || input$targetName == "") {
        errors <- c(errors, "Target Name is required")
      }
    }

    if (is.null(input$targetConcepts) || input$targetConcepts == "") {
    		errors <- c(errors, "Target is a required field")
    }

    if (!is.null(input$comparatorConcepts) && input$comparatorConcepts != "") {
      if (!validateNumericList(input$comparatorConcepts)) {
        errors <- c(errors, "Comparator Concept IDs must be numerical values separated by commas")
      }
      if (is.null(input$comparatorName) || input$comparatorName == "") {
        errors <- c(errors, "Comparator Name is required when Comparator Concept IDs are provided")
      }
    }

    if (!is.null(input$outcomeConcepts) && input$outcomeConcepts != "") {
      if (!validateNumericList(input$outcomeConcepts)) {
        errors <- c(errors, "Outcome Concept IDs must be numerical values separated by commas")
      }
      if (is.null(input$outcomeName) || input$outcomeName == "") {
        errors <- c(errors, "Outcome Name is required when Outcome Concept IDs are provided")
      }
    }

    if (is.null(input$followUpDays) ||
      is.na(input$followUpDays) ||
      input$followUpDays < 0) {
      errors <- c(errors, "Follow-up days must not be empty")
    }

    if (input$ageRange[1] > input$ageRange[2]) {
      errors <- c(errors, "Minimum age cannot be greater than maximum age")
    }

    if (!input$genderMale && !input$genderFemale) {
      errors <- c(errors, "At least one gender must be selected")
    }

    if (length(errors) > 0) {
    	shiny::showModal(modalDialog(
        title = "Validation Errors",
        shiny::div(
        	shiny::p("Please fix the following issues before adding the analysis:"),
        	shiny::tags$ul(
            lapply(errors, function(e) shiny::tags$li(style = "color: #d9534f;", e))
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      genderIds <- c()
      if (input$genderMale) genderIds <- c(genderIds, 8507)
      if (input$genderFemale) genderIds <- c(genderIds, 8532)

      uniqueId <- paste0("analysis_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
      print(input$comparatorUseDrugEra)

      newAnalysis <- list(
        uniqueId = uniqueId,
        analysisId = input$analysisId,
        analysisName = input$analysisName,
        minAge = input$ageRange[1],
        maxAge = input$ageRange[2],
        genderIds = genderIds,
        raceIds = input$raceConceptIds,
        ethnicityIds = input$ethnicityConceptIds,
        studyStartDate = input$studyStartDate,
        studyEndDate = input$studyEndDate,
        followUpDays = input$followUpDays,
        requiredDomains = input$requiredDomains,
        desiredDomains = input$desiredDomains,
        requiredVisits = input$requiredVisits,
        desiredVisits = input$desiredVisits,
        indicationConcepts = input$indicationConcepts,
        indicationName = input$indicationName,
        targetConcepts = input$targetConcepts,
        targetName = input$targetName,
        comparatorConcepts = input$comparatorConcepts,
        comparatorName = input$comparatorName,
        outcomeConcepts = input$outcomeConcepts,
        outcomeName = input$outcomeName,
        comparatorUseDrugEra = input$comparatorUseDrugEra,
        targetUseDrugEra = input$targetUseDrugEra
      )

      # Add to list
      currentList <- analysesList()
      currentList[[length(currentList) + 1]] <- newAnalysis
      analysesList(currentList)

      shiny::updateTextInput(session, "analysisName", value = "")

      shiny::showNotification("Analysis added to queue", type = "message", duration = 3)
    }
  })

  # Render analyses list
  output$analysesListDisplay <- shiny::renderUI({
    analyses <- analysesList()

    if (length(analyses) == 0) {
      return(p("No analyses queued yet. Please add aat least one analysis to the queue then try again", style = "color: #999; font-style: italic;"))
    }

    analysisItems <- lapply(seq_along(analyses), function(i) {
      analysis <- analyses[[i]]
      uniqueId <- analysis$uniqueId


      shiny::div(class = "analysis-item",
      		shiny::div(class = "analysis-header", onclick = paste0("$('#analysis-details-", uniqueId, "').toggleClass('show')"),
          		shiny::div(
              	shiny::strong(analysis$analysisName),
                shiny::tags$span(class = "analysis-count", paste("ID:", analysis$analysisId))
              ),
          		shiny::div(class = "analysis-actions",
                  shiny::actionButton(paste0("edit_", uniqueId), "Edit", class = "btn-edit"),
                  shiny::actionButton(paste0("remove_", uniqueId), "Remove", class = "btn-remove")
              )
          ),
      		shiny::div(id = paste0("analysis-details-", uniqueId), class = "analysis-details",
          		shiny::p(shiny::strong("Age Range: "), paste(analysis$minAge, "-", analysis$maxAge)),

              if (length(analysis$genderIds) > 0) p(strong("Gender IDs: "), paste(analysis$genderIds, collapse = ", ")),
              if (analysis$raceIds != "" || analysis$raceIds != "") p(strong("Race IDs: "), paste(analysis$raceIds, collapse = ", ")),
              if (analysis$ethnicityIds != "" || analysis$ethnicityIds != "") p(strong("Ethnicity IDs: "), paste(analysis$ethnicityIds, collapse = ", ")),
              if (analysis$studyStartDate != "" || analysis$studyEndDate != "") p(strong("Study Period: "), paste(analysis$studyStartDate, "-", analysis$studyEndDate)),
              if (analysis$followUpDays > 0) p(strong("Follow-up: "), paste(analysis$followUpDays, "days")),
              if (length(analysis$requiredDomains) > 0) p(strong("Required Domains: "), paste(analysis$requiredDomains, collapse = ", ")),
              if (length(analysis$desiredDomains) > 0) p(strong("Desired Domains: "), paste(analysis$desiredDomains, collapse = ", ")),
              if (length(analysis$requiredVisits) > 0) p(strong("Required Visits: "), paste(analysis$requiredVisits, collapse = ", ")),
              if (length(analysis$desiredVisits) > 0) p(strong("Desired Visits: "), paste(analysis$desiredVisits, collapse = ", ")),
              if (analysis$indicationName != "" || analysis$indicationConcepts != "") p(strong("Indication: "), analysis$indicationName),
              if (analysis$targetName != "" || analysis$targetConcepts != "") p(strong("Target: "), analysis$targetName),
              if (analysis$comparatorName != "" || analysis$comparatorConcepts != "") p(strong("Comparator: "), analysis$comparatorName),
              if (analysis$outcomeName != "" || analysis$outcomeConcepts != "") p(strong("Outcome: "), analysis$outcomeName)
          )
      )
    })

    do.call(tagList, analysisItems)
  })

  # Analyses edit/remove handlers
  createdObservers <- shiny::reactiveVal(character())

  shiny::observeEvent(analysesList(), {
    analyses <- analysesList()
    existingObservers <- createdObservers()

    lapply(analyses, function(analysis) {
      uniqueId <- analysis$uniqueId

      if (uniqueId %in% existingObservers) {
        return(NULL)
      }

      createdObservers(c(createdObservers(), uniqueId))

      # Remove button
      shiny::observeEvent(input[[paste0("remove_", uniqueId)]], {
        currentList <- analysesList()
        keepIndices <- sapply(currentList, function(x) x$uniqueId != uniqueId)
        if (any(keepIndices)) {
          newList <- currentList[keepIndices]
        } else {
          newList <- list()
        }
        analysesList(newList)
        shiny::showNotification("Analysis removed", type = "message", duration = 2)
      }, ignoreInit = TRUE, once = TRUE)

      # Edit button
      shiny::observeEvent(input[[paste0("edit_", uniqueId)]], {
        currentList <- analysesList()
        analysisIndex <- which(sapply(currentList, function(x) x$uniqueId == uniqueId))
        if (length(analysisIndex) > 0) {
          analysis <- currentList[[analysisIndex[1]]]
          updateTextInput(session, "analysisId", value = analysis$analysisId)
          updateTextInput(session, "analysisName", value = analysis$analysisName)
          updateSliderInput(session, "ageRange", value = c(analysis$minAge, analysis$maxAge))
          updateCheckboxInput(session, "genderMale", value = 8507 %in% analysis$genderIds)
          updateCheckboxInput(session, "genderFemale", value = 8532 %in% analysis$genderIds)
          updateTextInput(session, "raceConceptIds", value = analysis$raceIds)
          updateTextInput(session, "ethnicityConceptIds", value = analysis$ethnicityIds)
          updateTextInput(session, "studyStartDate", value = analysis$studyStartDate)
          updateTextInput(session, "studyEndDate", value = analysis$studyEndDate)
          updateNumericInput(session, "followUpDays", value = analysis$followUpDays)
          updateSelectInput(session, "requiredDomains", selected = analysis$requiredDomains)
          updateSelectInput(session, "desiredDomains", selected = analysis$desiredDomains)
          updateSelectInput(session, "requiredVisits", selected = analysis$requiredVisits)
          updateSelectInput(session, "desiredVisits", selected = analysis$desiredVisits)
          updateTextInput(session, "indicationConcepts", value = analysis$indicationConcepts)
          updateTextInput(session, "indicationName", value = analysis$indicationName)
          updateTextInput(session, "targetConcepts", value = analysis$targetConcepts)
          updateTextInput(session, "targetName", value = analysis$targetName)
          updateTextInput(session, "comparatorConcepts", value = analysis$comparatorConcepts)
          updateTextInput(session, "comparatorName", value = analysis$comparatorName)
          updateTextInput(session, "outcomeConcepts", value = analysis$outcomeConcepts)
          updateTextInput(session, "outcomeName", value = analysis$outcomeName)
          updateCheckboxInput(session, 'comparatorUseDrugEra', value = analysis$comparatorUseDrugEra)
          updateCheckboxInput(session, 'targetUseDrugEra', value = analysis$comparatorUseDrugEra)

          keepIndices <- sapply(currentList, function(x) x$uniqueId != uniqueId)
          if (any(keepIndices)) {
            newList <- currentList[keepIndices]
          } else {
            newList <- list()
          }
          analysesList(newList)
          shiny::showNotification("Analysis loaded for editing", type = "message", duration = 3)
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  }, ignoreInit = TRUE)


  observeEvent(input$runAnalysis, {
    analyses <- analysesList()

    if (length(analyses) == 0 && (is.null(input$analysisId) || input$analysisId == "")) {
    	shiny::showModal(shiny::modalDialog(
        title = "No Analyses to Run",
        "Please add at least one analysis to the queue or fill in the current form.",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
      return()
    }

    # on second thought, better force user to add everything explicitly to the queue for clarity and better flow.
    # if (!is.null(input$analysisId) && input$analysisId != "" && !is.null(input$analysisName) && input$analysisName != "") {
    # 	shinyjs::click("addToList")
    # 	Sys.sleep(0.5)
    # 	analyses <- analysesList()
    # }

    if (!dbConnected()) {
    	shiny::showModal(modalDialog(
        title = "Database Connection Error",
        "Database is not connected. Please check your connection settings.",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
      return()
    }

    shiny::showModal(shiny::modalDialog(
      title = "Running Database Diagnostics",
      paste("Processing", length(analyses), "analyses... Please wait."),
      footer = NULL
    ))

    tryCatch({
      # Create DbDiagnostics settings
      ddStudiesList <- lapply(analyses, function(analysis) {
        genderIds <- analysis$genderIds
        if (length(genderIds) == 0) genderIds <- c(8507, 8532)

        DbDiagnostics::createDataDiagnosticsSettings(
          analysisId = as.numeric(analysis$analysisId),
          analysisName = analysis$analysisName,
          minAge = analysis$minAge,
          maxAge = analysis$maxAge,
          genderConceptIds = genderIds,
          raceConceptIds = parseNumericList(analysis$raceIds),
          ethnicityConceptIds = parseNumericList(analysis$ethnicityIds),
          studyStartDate = if (analysis$studyStartDate != "") as.numeric(analysis$studyStartDate) else NULL,
          studyEndDate = if (analysis$studyEndDate != "") as.numeric(analysis$studyEndDate) else NULL,
          requiredDurationDays = analysis$followUpDays,
          requiredDomains = if (length(analysis$requiredDomains) > 0) analysis$requiredDomains else c("condition", "drug"),
          desiredDomains = if (length(analysis$desiredDomains) > 0) analysis$desiredDomains else NULL,
          requiredVisits = if (length(analysis$requiredVisits) > 0) analysis$requiredVisits else NULL,
          desiredVisits = if (length(analysis$desiredVisits) > 0) analysis$desiredVisits else NULL,
          targetName = if (analysis$targetName != "") analysis$targetName else "target",
          targetConceptIds = parseNumericList(analysis$targetConcepts),
          comparatorName = if (analysis$comparatorName != "") analysis$comparatorName else NULL,
          comparatorConceptIds = parseNumericList(analysis$comparatorConcepts),
          indicationName = if (analysis$indicationName != "") analysis$indicationName else NULL,
          indicationConceptIds = parseNumericList(analysis$indicationConcepts),
          outcomeName = if (analysis$outcomeName != "") analysis$outcomeName else NULL,
          outcomeConceptIds = parseNumericList(analysis$outcomeConcepts),
          targetUseDrugEra = analysis$targetUseDrugEra,
          comparatorUseDrugEra = analysis$comparatorUseDrugEra
        )
      })

      dbDiagnosticResults <- DbDiagnostics::executeDbDiagnostics(
        connectionDetails = connectionDetails,
        resultsDatabaseSchema = resultsDatabaseSchema,
        resultsTableName = resultsTableName,
        dataDiagnosticsSettings = ddStudiesList
      )

      globalResults <<- dbDiagnosticResults
      globalSettings <<- analyses

      shiny::removeModal()

      displayContent <- shiny::tagList()

      displayContent <- shiny::tagList(
        displayContent,
        shiny::div(class = "summary-card",
        		shiny::h5("Results Summary"),

            lapply(analyses, function(analysis) {
              analysis_data <- dbDiagnosticResults[!is.na(dbDiagnosticResults$analysisName) &
                                                     dbDiagnosticResults$analysisName == analysis$analysisName,]

              unique_dbs <- unique(analysis_data$databaseId[!is.na(analysis_data$databaseId)])

              tagList(
              	shiny::h6(shiny::strong(analysis$analysisName)),

                if (length(unique_dbs) > 0) {
                  concept_types <- list(
                    T = "propWithRequiredTargetConcepts",
                    C = "propWithRequiredComparatorConcepts",
                    I = "propWithRequiredIndicationConcepts",
                    O = "propWithRequiredOutcomeConcepts"
                  )

                  tagList(
                    lapply(unique_dbs, function(db) {
                      db_results <- analysis_data[!is.na(analysis_data$databaseId) &
                                                    analysis_data$databaseId == db,]

                      div(style = "margin-bottom: 8px;",
                          strong(db), ": ",
                          lapply(names(concept_types), function(type) {
                            stat_name <- concept_types[[type]]
                            stat_row <- db_results[!is.na(db_results$statistic) &
                                                     db_results$statistic == stat_name,]

                            bg_color <- "#cccccc"
                            if (nrow(stat_row) > 0 &&
                              "status" %in% names(stat_row) &&
                              !is.na(stat_row$status[1])) {
                              if (tolower(stat_row$status[1]) == "pass") bg_color <- "#4CAF50"
                              else if (tolower(stat_row$status[1]) == "fail") bg_color <- "#F44336"
                            }

                            shiny::span(
                              style = paste0("background-color: ", bg_color,
                                             "; color: white; padding: 2px 6px; margin-right: 4px; border-radius: 3px; font-size: 12px;"),
                              type
                            )
                          })
                      )
                    })
                  )
                } else {
                	shiny::p(shiny::em("No results available."))
                },

                shiny::tags$hr()
              )
            })
        )
      )
      if (is.data.frame(dbDiagnosticResults)) {
        displayContent <- shiny::tagList(
          displayContent,
          shiny::h5("Diagnostic Results Table"),
          shiny::div(class = "results-table",
              DT::dataTableOutput("resultsTable")
          )
        )

        output$resultsTable <- DT::renderDataTable({
          data_to_display <- dbDiagnosticResults
          # convert numeric as strings to numeric for proper sorting (by default its alphabetic due to output type)
          for (i in seq_along(data_to_display)) {
            col <- data_to_display[[i]]
            if (is.character(col)) {
              num_col <- suppressWarnings(as.numeric(col))
              if (sum(is.na(num_col)) == sum(is.na(col))) {
                data_to_display[[i]] <- num_col
              }
            }
          }

          DT::datatable(
            data_to_display,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel')
            ),
            class = 'display compact',
            rownames = FALSE
          )
        })
      }


      shiny::showModal(modalDialog(
        title = "Database Diagnostics Results",
        size = "l",
        displayContent,
        easyClose = TRUE,
        footer = tagList(
        	shiny::modalButton("Close"),
        	shiny::downloadButton("downloadPDF", "Download PDF Report"),
        	shiny::downloadButton("downloadCSV", "Download raw CSV")

        ),
        shiny::tags$head(shiny::tags$style(".modal-dialog { width: 90%; max-width: 1300px; }"))
      ))

      output$downloadPDF <- downloadHandler(
        filename = function() {
          "db_diagnostics_results.pdf"
        },
        content = function(file) {
        	shiny::showModal(modalDialog(
            title = "Generating PDF Report",
            "Creating PDF report... Please wait.",
            footer = NULL
          ))

          tryCatch({
            DbDiagnostics::createPdfReport(globalResults, file)
          	shiny::removeModal()
          }, error = function(e) {
          	shiny::removeModal()
          	shiny::showNotification(
              paste("PDF generation failed:", e$message),
              type = "error",
              duration = 10
            )
          })
        },
        contentType = "application/pdf"
      )

      output$downloadCSV <- downloadHandler(
        filename = function() {
          "db_diagnostics_results.csv"
        },
        content = function(file) {
        	shiny::showModal(shiny::modalDialog(
            title = "Generating CSV Export",
            "Creating CSV file... Please wait.",
            footer = NULL
          ))
          tryCatch({
            write.csv(globalResults, file, row.names = FALSE)
          	shiny::removeModal()
          }, error = function(e) {
          	shiny::removeModal()
          	shiny::showNotification(
              paste("CSV export failed:", e$message),
              type = "error",
              duration = 10
            )
          })
        },
        contentType = "text/csv"
      )

    }, error = function(e) {
    	shiny::removeModal()
    	shiny::showModal(modalDialog(
        title = "Error",
        paste("An error occurred while running diagnostics:", e$message),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })
  })
}

#' @export
run_shiny_app <- function(
		connectionDetails = NULL,
		aresLink = Sys.getenv("ARES_INSTANCE_LINK", unset = '/'),
		resultsDatabaseSchema = Sys.getenv("RESULTS_SCHEMA", unset = "db_profile"),
		resultsTableName = Sys.getenv("TABLE_NAME", unset = "db_profile_results")
) {
	if (is.null(connectionDetails)) {
		stop("connectionDetails object must be provided. Create one using DatabaseConnector::createConnectionDetails()")
	}

	app <- shiny::shinyApp(
		ui = ui(aresLink),
		server = function(input, output, session) {
			server(input, output, session, connectionDetails, aresLink, resultsDatabaseSchema, resultsTableName)
		}
	)
	shiny::runApp(app)
}
