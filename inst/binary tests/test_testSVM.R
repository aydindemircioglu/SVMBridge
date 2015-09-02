context("testSVM")

test_that(" method autodetection in testSVM is working.", {
	tmp = tempfile()
	solver = "LIBSVM"
	trainFile = ("~/SVMBridge/tests/data/sparse.data")
	verbose = TRUE
	cost = runif(1)
	gamma = runif(1)
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMSoftware (solver, searchPath = "../../../shark/svm_large_data/software/", verbose = FALSE)
	
	obj1 =  trainSVM(
			method = solver,
			trainDataFile = trainFile, 
			cost = cost, 
			gamma = gamma, 
			readModelFile = TRUE,
			verbose = verbose
		)  
# 	#no method is given as argument for testSVM
	obj2 =  testSVM(
			testDataFile = trainFile,
# 			method = solver,
			model = obj1$model,
			verbose = verbose
		) 
})