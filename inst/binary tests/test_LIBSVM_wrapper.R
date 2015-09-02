context("LIBSVM")

test_that(" readModel.LIBSM (read/write operations) work with binary models", {
	tmp = tempfile()
	solver = "LIBSVM"
	dataset = ("../data/mnist.binary.model")
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMSoftware (solver, searchPath = "../../../shark/svm_large_data/software/", verbose = TRUE)
	SVMObject = SVMBridgeEnv$packages[[solver]]
	
	svmatrix = readModel.LIBSVM(SVMObject, modelFile = dataset)
	writeModel.LIBSVM(SVMObject, svmatrix, tmp)
	svmatrix2 = readModel.LIBSVM(SVMObject, modelFile = tmp)

	expect_equal(svmatrix, svmatrix2)
})

test_that(" readModel.LIBSM (read/write operations) work with multiclass models", {
	tmp = tempfile()
	solver = "LIBSVM"
	dataset = ("../data/mnist.multi.model")
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMSoftware (solver, searchPath = "../../../shark/svm_large_data/software/", verbose = TRUE)
	SVMObject = SVMBridgeEnv$packages[[solver]]
	
	svmatrix = readModel.LIBSVM(SVMObject, modelFile = dataset)
	writeModel.LIBSVM(SVMObject, svmatrix, modelFile =tmp)
	svmatrix2 = readModel.LIBSVM(SVMObject, modelFile = tmp)

	expect_equal(svmatrix, svmatrix2)
})

test_that(" Read/Write operations on different datasets do work with LIBSVM ", {
	datasets = c("aXa")#, "protein", "poker")
	solver = "LIBSVM"
	verbose = TRUE
	
	for(d in datasets){
		tmpModel_Without = tempfile()
		tmpModel = tempfile()
		tmpPredictions_Without = tempfile()
		tmpPredictions = tempfile()
		addSVMPackage (method = solver, verbose = FALSE)
		findSVMSoftware (solver, searchPath = "../../../shark/svm_large_data/software/", verbose = FALSE)
	
		trainFile = paste ("../../../shark/svm_large_data/datasets/", d, "/", d, ".combined.scaled", sep = "")
		
		print(trainFile)
		cost = runif(1)
		gamma = runif(1)
		subsamplingrate = 0.01
		#No Read/Write Operations used
		trainObj =  trainSVM(
			method = solver,
			trainDataFile = trainFile, 
			subsamplingRate = subsamplingrate,
			cost = cost, 
			gamma = gamma, 
			#epsilon = 0.5, 
			readModelFile = FALSE,
			modelFile = tmpModel_Without,
			verbose = verbose
		)  
		testObj =  testSVM(
			method = solver,
			testDataFile = trainFile,
			modelFile = tmpModel_Without,
			predictionsFile = tmpPredictions_Without,
			verbose = verbose
		) 
		
		########################################
		
		#Use Read/Write Operations
		trainObj2 =  trainSVM(
			method = solver,
			trainDataFile = trainFile, 
			subsamplingRate = subsamplingrate,
			cost = cost, 
			gamma = gamma, 
			#epsilon = 0.5, 
			modelFile = tmpModel,
			readModelFile = TRUE,
			verbose = verbose
		)
		
		
		# get the correct object
		SVMObject = SVMBridgeEnv$packages[[solver]]
		#print(SVMObject)
		writeModel.LIBSVM(SVMObject,trainObj2$model, modelFile = tmpModel)
		
	
		testObj =  testSVM(
			method = solver,
			testDataFile = trainFile,
			modelFile = tmpModel,
			predictionsFile = tmpPredictions,
			verbose = verbose
		)  

		mod_a = file(tmpModel_Without, open = "rt")
		model_a = readLines(mod_a)
		
		mod_b = file(tmpModel, open = "rt")
		model_b = readLines(mod_b)
		
		pred_a = file(tmpPredictions_Without, open = "rt")
		prediction_a = readLines(pred_a)
		
		pred_b = file(tmpPredictions, open = "rt")
		prediction_b = readLines(pred_b)
		
# 		expect_equal(model_a, model_b)
# 		expect_equal(prediction_a, prediction_b)

		
		
		close(mod_a)
		close(mod_b)
		close(pred_a)
		close(pred_b)
	}
})