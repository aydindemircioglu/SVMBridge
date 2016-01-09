
downloadSoftware = function (solver, verbose = FALSE) {
	
	if (verbose == TRUE) {
		cat ("Downloading and Building solver ", solver, "\n")
	}
	
	if ((solver == "BSGD") || (solver == "LLSVM")) {
		solver = "BudgetedSVM"
	}
	
	if ((solver == "CVM") || (solver == "BVM")) {
		solver = "libCVM"
	}

	# generate paths
	tmpDir = tempdir()
	softwareDir = file.path (tmpDir, solver)
	
	downloadAndBuild = function (softwareDir) {
		system2 ("svn", stdout = NULL, stderr = NULL, args = c("checkout", "--force", paste0 ("https://github.com/aydindemircioglu/SVMBridge/trunk/software/", solver), softwareDir ))
		system2 ("make", stdout = NULL, stderr = NULL, args = c("-C", softwareDir))
	}
	if (verbose == TRUE)
		downloadAndBuild(softwareDir)
	else
		suppressMessages(downloadAndBuild(softwareDir))

	return (softwareDir )
}
