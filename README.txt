This is the Lite version of the project containing only core files to run the application (RDS and app.R). Lite versionion
improves first time download when the application is run with shiny::runGitHub().
The full version includes datasets and R scripts for data wrangling. Please refer to repository "matura-albania"

Installation instructions for R users.

1. Run on R console
library(shiny)
runGitHub("matura-albania-Lite","kreshnikxhangolli")

2. Clone (or download and unizp) the files. Necessary files to run the application are container.RDS and programs list.RDS
in RDS folder and app.R under project's root directory.
	(i) open app.R file and click Run button on top-right corner

	(ii) Using R console
		a. Set working directory where app.R is located (use setwd() command)
		b. use runApp() on the command prompt to run the application