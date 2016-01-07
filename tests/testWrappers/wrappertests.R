
wrappertests = function (solver, trainDataX, trainDataY, testDataX, testDataY, verbose) {
	cat("\n\n\n======= Train ", solver, ", Traindata from Memory, Model to Memory\n")
	print (getSVMObject(solver))
	trainObj =  trainSVM(
		method = solver,
		trainDataX = trainDataX, 
		trainDataY = trainDataY, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		readModelFile = TRUE,
		verbose = verbose
	)  
	expect_equal (trainObj$model$nSV, c(20, 12))
	
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX,
		testDataY = testDataY,
		model = trainObj$model,
		verbose = verbose
	)  
	expect_equal (testObj$testError, 0.06)
}

f = function () {
	
	cat("\n\n\n======= Train ", solver, ", Traindata from File, Model to Memory")
	trainObj =  trainSVM(
		method = solver,
		trainDataFile = trainDataFile,
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		readModelFile = TRUE,
		verbose = verbose
	)  

	testObj =  testSVM(
		method = solver,
		testDataFile = trainDataFile,
		model = trainObj$model,
		verbose = verbose
	)  


	
	cat ("\n\n\n======= Test ", solver, ", Testdata from Memory, Model from Memory, Predictions to Memory")
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX, 
		testDataY = testDataY, 
		verbose = verbose,
		model = trainObj$model,
		readPredictions = TRUE
	)  


	
	cat("\n\n\n======= Test ", solver, ", Testdata from Disk, Model from Memory, Predictions to Disk")
	testObj =  testSVM(
		method = solver,
		testDataFile = testDataFile,
		model = trainObj$model,
		predictionsFile = predictionsFile,
		verbose = verbose
	)  

	
	cat("\n\n\n======= Test ", solver, ", Testdata from Memory, Model from Memory, Predictions to Disk")
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX, 
		testDataY = testDataY, 
		model = trainObj$model,
		predictionsFile = predictionsFile,
		verbose = verbose,
	)  

	
	
	cat("\n\n\n======= Train ", solver, ", Traindata from Disk, Model to Disk")
	trainObj =  trainSVM(
		method = solver,
		trainDataFile = testDataFile,
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		verbose = verbose,
		modelFile = modelFile,
		readModelFile = FALSE
	)  


	cat("\n\n\n======= Train ", solver, ", Traindata from Memory, Model to Disk")
	trainObj =  trainSVM(
		method = solver,
		trainDataX = trainDataX, 
		trainDataY = trainDataY, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		verbose = verbose,
		readModelFile = FALSE,
		modelFile = modelFile
	)  
			

	cat("\n\n\n======= Test ", solver, ", Testdata from Memory, Model from Disk, Predictions to Memory")
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX, 
		testDataY = testDataY, 
		modelFile = modelFile,
		verbose = verbose
	)  

	

	cat("\n\n\n======= Test ", solver, ", Testdata from Disk, Model from Disk, Predictions to Memory")
	testObj =  testSVM(
		method = solver,
		testDataFile = testDataFile,
		modelFile = modelFile,
		verbose = verbose
	)  
	

	
	cat("\n\n\n======= Test ", solver, ", Testdata from Memory, Model from Disk, Predictions to Disk")
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX, 
		testDataY = testDataY, 
		modelFile = modelFile,
		predictionsFile = predictionsFile,
		verbose = verbose
	)  

	

	cat("\n\n\n======= Test ", solver, ", Testdata from Disk, Model from Disk, Predictions to Disk")
	testObj =  testSVM(
		method = solver,
		testDataFile = testDataFile,
		modelFile = modelFile,
		predictionsFile = predictionsFile,
		verbose = verbose
	)  
	
	cat("\n\n\n======= Finished all demo calls. Please check for any visible errors.\n\n")
}

