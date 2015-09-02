context("readModelFromFile")

test_that("reading a model is working.", {

	solvers = c("SVMperf",  "LASVM", "BVM", "CVM", "LIBSVM")
	
	for (solver in solvers) {
		addSVMPackage (method = solver, verbose = FALSE)
		findSVMWrapper (solver, searchPath = "../../R", verbose = FALSE)
	
		# some models are the same
		solverName = solver
		if ((solver == "LASVM") || (solver== "BVM") || (solver == "CVM")) {
			solverName = "LIBSVM"
			addSVMPackage (method = solverName, verbose = FALSE)
			findSVMWrapper (solverName, searchPath = "../../R", verbose = FALSE)
		}

		# detect model type
		modelFile = paste0 ("../data/", solver, ".australian.model")
		model = readModelFromFile (modelFile)

		# check only information that is there in all models
		testthat::expect_equal (model$gamma, 1.0)
	}
})

