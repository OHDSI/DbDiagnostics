options(connectionObserver = NULL)

# Call the `createConnectionDetails` function to create the object, inputting the information for how to connect to your database. Detailed instructions on how to do this can be found [here](http://ohdsi.github.io/DatabaseConnector/articles/Connecting.html).

connectionDetails <- DatabaseConnector::createConnectionDetails(
	dbms = Sys.getenv("dbms"),
	server = Sys.getenv("server"),
	user = Sys.getenv("user"),
	password = Sys.getenv("password"),
	pathToDriver = Sys.getenv("path_to_driver")
)

# Call the execute function. Please see the help menu or the ReadMe for detailed information about each parameter

DbProfile::execute(
	connectionDetails = connectionDetails,
	cdmDatabaseSchema = Sys.getenv("cdmDatabaseSchema"),
	resultsDatabaseSchema = Sys.getenv("resultsDatabaseSchema"),
	vocabDatabaseSchema = Sys.getenv("vocabDatabaseSchema"),
	cdmSourceName = Sys.getenv("cdmSourceName"),
	outputFolder = Sys.getenv("outputFolder")
)
