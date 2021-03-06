#
# SVMBridge 
#
#		(C) 2015, by Aydin Demircioglu
# 
# SVMBridge is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SVMBridge is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# Please do not use this software to destroy or spy on people, environment or things.
# All negative use is prohibited.
#


wrappertests = function (solver, trainDataX, trainDataY, testDataX, testDataY, verbose) {
	trainObj =  trainSVM(
		method = solver,
		trainDataX = trainDataX, 
		trainDataY = trainDataY, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		rank = 32,
		k = 32,
		budget = 32,
		readModelFile = TRUE,
		verbose = verbose
	)  
	
	
	if (Sys.info()["sysname"] == "Darwin") {
		expErrors = list("LIBSVM" = c(20, 12), "LASVM" = c(23, 12), "BSGD" = c(10, 8), "SVMperf" = c(20, 12), 
								"BVM" = c(20,15),  "CVM" = c(20, 15),  "LLSVM" = c(17, 15)) # maybe these things are random :/
	} else if (Sys.info()["sysname"] == "Windows") {
		expErrors = list("LIBSVM" = c(20, 12), "LASVM" = c(22, 11), "BSGD" = c(10, 8), "SVMperf" = c(21, 11), 
									"BVM" = c(20, 16),  "CVM" = c(20, 16),  "LLSVM" = c(19,13))
	} else {
		expErrors = list("LIBSVM" = c(20, 12), "LASVM" = c(22, 12), "BSGD" = c(10, 8), "SVMperf" = c(21, 11), 
									"BVM" = c(18, 16),  "CVM" = c(18, 16),  "LLSVM" = c(21, 11))
	}
	testthat::expect_equal (trainObj$model$nSV, expErrors[[solver]])
	
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX,
		testDataY = testDataY,
		model = trainObj$model,
		verbose = verbose
	)  

	if (Sys.info()["sysname"] == "Windows") {
		expErrors = c("LIBSVM" = 0.06, "LASVM" = 0.06, "BSGD" = 0.04, "SVMperf" = 0.06, "BVM" = 0.04, "CVM" = 0.04, "LLSVM" = 0.08)  
	} else {
		expErrors = c("LIBSVM" = 0.06, "LASVM" = 0.06, "BSGD" = 0.04, "SVMperf" = 0.06, "BVM" = 0.04, "CVM" = 0.04, "LLSVM" = 0.06)  
	}
	
	if (verbose == TRUE) {
		print (testObj)
	}

	testthat::expect_lte (abs(testObj$testError - expErrors[solver]), 0.001)
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

