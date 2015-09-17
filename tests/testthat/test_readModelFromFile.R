context("readModelFromFile")

test_that("reading a model is working.", {

	solvers = c("SVMperf",  "LASVM", "BVM", "CVM", "LIBSVM", "BSGD", "LLSVM")
	
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
		if (solver == "BSGD")
			model$gamma = model$gamma/2
		testthat::expect_equal (model$gamma, 1.0)
		
		
		# check that the last SV is faked
		for (j in 1:14) {
			testthat::expect_equal( model$SV[nrow(model$SV),j], j/10)
		}
		
		# fake alpha value
		if (solver == "BSGD") {
			testthat::expect_equal (model$alpha[nrow(model$alpha),2], -1)
		} else {
			testthat::expect_equal (model$alpha[length(model$alpha)], -1)
		}
	}
})

