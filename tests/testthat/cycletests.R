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

cycletests = function (solver, verbose) {
	
	modelFile = tempfile()
	myModelFile = tempfile()
	predictionsFile = tempfile()
	myPredictionsFile = tempfile()
	
	trainObj =  trainSVM(
		method = solver,
		trainDataFile = "../data/australian.train",
		cost = 1, 
		gamma = 1, 
		rank = 32,
		k = 32,
		budget = 32,
		subsamplingRate = 0.9,
		readModelFile = FALSE,
		modelFile = modelFile,
		verbose = verbose
	)  
		
	testObj =  testSVM(
		method = solver,
		testDataFile = "../data/australian.test",
		modelFile = modelFile,
		predictionsFile = predictionsFile ,
		verbose = verbose
	) 

	# train again this time by writing and reading the model file 
	myTrainObj =  trainSVM(
		method = solver,
		trainDataFile = "../data/australian.train",
		cost = 1, 
		gamma = 1, 
		rank = 32,
		k = 32,
		budget = 32,
		subsamplingRate = 0.9,
		readModelFile = TRUE,
		verbose = verbose
	)  
		
	myTestObj =  testSVM(
		method = solver,
		testDataFile = "../data/australian.test",
		model = myTrainObj$model,
		predictionsFile = myPredictionsFile,
		verbose = verbose
	)  

	testthat::expect_null (trainObj$model)
	#expect_equal (myTrainObj$model$modelType, solver)
	testthat::expect_equal (myTrainObj$model$gamma, 1)
	testthat::expect_equal (myTestObj$error, testObj$error)
}
	
