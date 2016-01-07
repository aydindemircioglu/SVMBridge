
downloadSoftware = function (solver) {
	
	if ((solver == "BSGD") || (solver == "LLSVM")) {
		solver = "BudgetedSVM"
	}
	
	if ((solver == "CVM") || (solver == "BVM")) {
		solver = "libCVM"
	}

	tmpDir = tempdir()
	print (tmpDir)
	softwareDir = file.path (tmpDir, solver)
	system2 ("svn", args = c("checkout", paste0 ("https://github.com/aydindemircioglu/SVMBridge/trunk/software/", solver), softwareDir ))
	system2 ("make", args = c("-C", softwareDir))

	return (softwareDir )
}
