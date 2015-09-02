context("expandTilde")

test_that(" tilde characters are expanded correctly.", {
	solver = "LIBSVM"
	verbose = TRUE
	cost = runif(1)
	gamma = runif(1)
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMSoftware (solver, searchPath = "~/shark/svm_large_data/software/", verbose = FALSE)
	trainFile = ("~/SVMBridge/tests/data/sparse.data")
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
	obj3 = readSparseData(filename = trainFile,
			      verbose = verbose
		)
})