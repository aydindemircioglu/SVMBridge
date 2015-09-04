context("BSGD")

test_that("BSGD wrapper works", {
	solver = "BSGD"
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMSoftware (solver, searchPath = "~/shark/svm_large_data/software/", verbose = TRUE)
	verbose = TRUE
	C = 0.71
	gamma = 0.41
	dataset = ("../data/mnist.data")
	
	trainObj =  trainSVM(
		method = solver,
		trainDataFile = dataset,
		cost = C, 
		gamma = gamma, 
		readModelFile = FALSE,
		modelFile = "~../data/mnist.BSGD.model",
		verbose = verbose
	)  
	
	testObj =  testSVM(
			method = solver, 
			testDataFile = dataset,
			modelFile = "../data/mnist.BSGD.model",
			verbose = verbose
		) 
})