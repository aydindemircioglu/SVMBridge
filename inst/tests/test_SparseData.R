context("SparseData")




#9*remove*
#Test for equality of model files and prediction files if using readSparseData/readSparseDataFromConnection writeSparseData/writeSparseDataToConnection and for the case without those fucntions. It may be handy to note that the test will return an error incase of evaluating datasets with entries like "3.4500000", since this value will be written out as "3.45". 
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

#10 Currently there are possible tilde character inputs for testSVM, trainSVM, readSparseData, findSVMSoftware, findSVMWrapper. Thus these functions will be tested.
test_that(" tilde characters are expanded correctly.", {
	trainFile = ("~/SVMBridge/tests/data/sparse.data")
# 	*remove*
	solver = "LIBSVM"
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
# 		*remove*
	obj3 = readSparseData(filename = trainFile,
			      verbose = verbose
		)
})

#11*remove*
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

#test_that("Test: Decreased Matrix row index due to faulty zeroBased value for Reading/Writing", {
#	tryCatch({
#		tmp = tempfile()
#		S1 = readSparseData (filename = "../data/sparse.data", verbose = FALSE, zeroBased = FALSE)
#		writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = TRUE)
#		expect_message(readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE), "est")
#		expect_error(readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE) )
#	},	
#	
#	interrupt = function(ex){
#		print(ex)
#	},
#		
#	
#		error = function(e) {
#			print(paste(e))
#		}
#	
#	
#	)
#})






