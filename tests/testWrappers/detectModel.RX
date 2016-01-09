context("detectModelTypeFromFile")

test_that(" model type autodetection is working.", {

	solvers = c("LIBSVM", "BSGD", "SVMperf",  "LASVM", "BVM", "CVM", "LIBSVM", "LLSVM")
	
	for (solver in solvers) {
		addSVMPackage (method = solver, verbose = FALSE)
		findSVMSoftware (solver, searchPath = "../../R", verbose = FALSE)
		
		# detect model type
		modelFile = paste0 ("../data/", solver, ".australian.model")
		modelName =  detectModelTypeFromFile (modelFile, defaultModel = solver)
		
		# some models are the same
		solverName = solver
		
		# we do not need this as we told the detection that the default model is the solver's model. 
		#if (solver == "LASVM")
		#	solverName = "LIBSVM"

		testthat::expect_equal (modelName, solverName)
	}
})