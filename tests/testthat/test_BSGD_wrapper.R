context("BSGD")

test_that("BSGD wrapper works", {
	trainFile = ("~/SVMBridge/tests/data/BSGD.australian.model")
# 	*remove*
	solver = "BSGD"
	verbose = TRUE
	cost = runif(1)
	gamma = runif(1)
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMSoftware (solver, searchPath = "~/shark/svm_large_data/software/", verbose = FALSE)
	obj1 =  trainSVM(
			method = solver,
			trainDataFile = trainFile, 
			cost = cost, 
			gamma = gamma, 
			readModelFile = TRUE,
			verbose = verbose
		)  
	obj2 =  testSVM(
			method = solver,
			testDataFile = trainFile,
			model = obj1$model,
			verbose = verbose
		) 



})