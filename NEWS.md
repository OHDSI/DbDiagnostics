DbDiagnostics 1.5
===========================
This release adds a question to the metadata collection about sponsored studies and corrects the instructional pages on how to run and upload the DbProfile information.

DbDiagnostics 1.4
===========================
This release adds two more summary statistics to the DbProfile; record counts by measurement_concept_id and 
value_as_concept_id and record counts by observation_concept_id and value_as_concept_id. Also fixes an issue with the column headers exported with the DbProfile output. 

DbDiagnostics 1.1
===========================
This release updates the package to use the database release key to identify databases from the dbProfile to the dbDiagnostics. It also changed the way the loop through databases are done so that the profile information is only downloaded once per database. 

DbDiagnostics 1.0.2
===========================
This release fixes a small bug related to calculating the number of people with at least one indication concept

DbDiagnostics 1.0.1
===========================
This bugfix addresses an issue where the sample sizes were all 0 if no comparator was given in the settings.

DbDiagnostics 1.0
===========================
This is the first release of the OHDSI Data Diagnostics package.
