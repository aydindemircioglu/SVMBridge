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


multiclasstests = function (solver, trainDataX, trainDataY, testDataX, testDataY, verbose) {

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
	#	modelFile = "/tmp/model",
		readModelFile = TRUE,
		verbose = verbose
	)  
	
	
	if (Sys.info()["sysname"] == "Darwin") {
		expErrors = list("LIBSVM" = c(17, 6, 15),  "BSGD" = c(2, 12, 5),
									"BVM" = c(21, 14, 22),  "CVM" = c(21, 14, 22))
	} else if (Sys.info()["sysname"] == "Windows") {
		expErrors = list("LIBSVM" = c(17, 6, 15),  "BSGD" = c(2, 12, 5),
									"BVM" = c(21, 14, 22),  "CVM" = c(21, 14, 22))
	} else { # unix
		expErrors = list("LIBSVM" = c(17, 6, 15),  "BSGD" = c(2, 12, 5),
									"BVM" = c(21, 14, 22),  "CVM" = c(21, 14, 22))
	}
#	print (trainObj$model$nSV)
#	print (expErrors[[solver]])
	testthat::expect_equal (trainObj$model$nSV, expErrors[[solver]])
	
#	writeSparseData (file="/tmp/T", X = testDataX, Y = testDataY)
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX,
		testDataY = testDataY,
		model = trainObj$model,
		readPredictions = TRUE,
		verbose = verbose
	)  

	if (Sys.info()["sysname"] == "Windows") {
		expErrors = c("LIBSVM" = 0.06, "BSGD" = 0.04, "BVM" = 0.04, "CVM" = 0.04)
	} else { #unix
		expErrors = c("LIBSVM" = 0.06, "BSGD" = 0.06, "BVM" = 0.04, "CVM" = 0.04)
	}
	
	if (verbose == TRUE) {
		print (testObj)
	}

# 		print (testObj)
# 	print (testObj$testError )
# 	print (expErrors[[solver]])
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

