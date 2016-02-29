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


predicttests = function (solver, trainDataX, trainDataY, testDataX, testDataY, verbose) {
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
	
	testObj =  predictSVM(
		method = solver,
		testDataX = testDataX,
		model = trainObj$model,
		verbose = verbose,
		readPredictions = TRUE
	)  

#	print (testObj$predictions)
	
	# expect beginning and end of predictions to be -1, -1...1, -1 (works for linux right now, hopefully everywhere)
	# LASVM predict function is BROKEN, will not wirte predictions!
	if ((solver != "LASVM")) {
		testthat::expect_equal (sign(testObj$predictions[1]), -1)
		testthat::expect_equal (sign(testObj$predictions[2]), -1)
		testthat::expect_equal (sign(testObj$predictions[49]), 1)
		testthat::expect_equal (sign(testObj$predictions[50]), -1)
	}
	
	if (verbose == TRUE) {
		print (testObj)
	}
}
