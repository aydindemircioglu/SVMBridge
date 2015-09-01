context("detectModelTypeFromFile")

test_that(" model type autodetection is working.", {

	solvers = c("BSGD", "SVMperf",  "LASVM", "BVM", "CVM", "LIBSVM")
	
	for (solver in solvers) {
		addSVMPackage (method = solver, verbose = FALSE)
		findSVMSoftware (solver, searchPath = "../../R", verbose = FALSE)
		
		# detect model type
		modelFile = paste0 ("../data/", solver, ".australian.model")
		modelName =  detectModelTypeFromFile (modelFile)
		
		# some models are the same
		solverName = solver
		if ((solver == "LASVM") || (solver== "BVM") || (solver == "CVM"))
			solverName = "LIBSVM"
		
		testthat::expect_equal (modelName, solverName)
	}
})