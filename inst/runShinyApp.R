
library(DbDiagnostics)
connectionDetails <- DatabaseConnector::createConnectionDetails(
	dbms     = "postgresql",
	server   = "localhost/Synthea",
	user     = "postgres",
	password = "password",
	port     = 5432,  # port to connect to your db
	pathToDriver = "driver"
)

DbDiagnostics::run_shiny_app(connectionDetails = connectionDetails)
