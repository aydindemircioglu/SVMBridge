context("SparseData")
#source("././src/SparseData.cpp")


#1
test_that(" if given Arguments for readSparseData are expected correct ones", {
	expect_error(readSparseData(0), "expecting a string")
})

#2
test_that(" if numerous reading/writing operations with zerobased set to FALSE on the same dataset leads to precision problems", {
	tmp = tempfile()
	S1 = readSparseData (filename = "~/SVMBridge/tests/data/sparse.data", verbose = FALSE, zeroBased = FALSE)
	writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = FALSE)
	S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = FALSE) 
	expect_equal(S1$X, S2$X)
	expect_equal(S1$Y, S2$Y)
	
	for(i in 1:100)
	{
		S1 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = FALSE)
		writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = FALSE)
		S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = FALSE) 
		expect_equal(S1$X, S2$X)
 		expect_equal(S1$Y, S2$Y)
	}
})

#3
test_that(" if numerous reading/writing operations with zerobased set to TRUE on the same dataset leads to precision problems", {
	tmp = tempfile()
	S1 = readSparseData (filename = "../data/sparse.data", verbose = FALSE, zeroBased = TRUE)
	writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = TRUE)
	S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE) 
	expect_equal(S1$X, S2$X)
	expect_equal(S1$Y, S2$Y)
	
	for(i in 1:100)
	{
		S1 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE)
		writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = TRUE)
		S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE) 
		expect_equal(S1$X, S2$X)
 		expect_equal(S1$Y, S2$Y)
	}
})

#4
test_that(" Increased Matrix row index due to faulty zeroBased value for Reading/Writing", {
	tmp = tempfile()
	S1 = readSparseData (filename = "../data/sparse.data", verbose = FALSE, zeroBased = TRUE)
	column1 = ncol(S1$X)
	writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = FALSE)
	S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE) 
	column2 = ncol(S2$X)
	expect_true(column1 < column2)
	
})

# #5
test_that(" Read/Write functionality of Datasets with multiple alpha vectors", {
	f = file("../data/mnist.model") #contains 9 columns of header information
	open(f)
	readLines(f, 9)
	tmp = tempfile()
	s = readSparseDataFromConnection(f, 9)
	writeSparseData(filename = tmp, s$X, s$Y)
	s2 = readSparseData(filename = tmp)
	expect_equal(s$X, s2$X)
	expect_equal(s$Y, s2$Y)

 	for(i in 1:100)
	{
		s = readSparseData(filename = tmp)
		writeSparseData(filename = tmp, s$X, s$Y)
		s2 = readSparseData(filename = tmp)
		expect_equal(s$X, s2$X)
		expect_equal(s$Y, s2$Y)
	}
	close(f)
	
})

#6
test_that(" Read/Write operations do work on datasets with non binary labels", {
	tmp = tempfile()
	S1 = readSparseData (filename = "../data/multiclass.sparse.test")
	writeSparseData ( tmp, S1$X,  S1$Y)
	S2 = readSparseData (filename = tmp) 
	expect_equal(S1$X, S2$X)
	expect_equal(S1$Y, S2$Y)
	
})

#7
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

#8
#FIXME
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

#9
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
	solver = "LIBSVM"
	verbose = TRUE
	cost = runif(1)
	gamma = runif(1)
	addSVMPackage (method = solver, verbose = TRUE)
	findSVMSoftware (solver, searchPath = "~/shark/svm_large_data/software/", verbose = TRUE)
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

#11
test_that(" method autodetection in testSVM is working.", {
	tmp = tempfile()
	solver = "LIBSVM"
	trainFile = ("~/SVMBridge/tests/data/sparse.data")
	verbose = TRUE
	cost = runif(1)
	gamma = runif(1)
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMSoftware (solver, searchPath = "../../../shark/svm_large_data/software/", verbose = TRUE)
	
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






