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


bashEscapetests = function (solver, trainDataX, trainDataY, testDataX, testDataY, verbose) {

    tdir = tempdir()
    trainfile = file.path (tdir, "A B C.sparse")
    testfile = file.path (tdir, "B C D.sparse")
    modelfile = file.path (tdir, "D E F.sparse")
    writeSparseData (X = trainDataX, Y = trainDataY, file = trainfile)
    writeSparseData (X = testDataX, Y = testDataY, file = testfile)

	trainObj =  trainSVM(
		method = solver,
		trainDataFile = trainfile,
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		rank = 32,
		k = 32,
		budget = 32,
		modelFile = modelfile,
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
		testDataFile = testfile,
		modelFile = modelfile,
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

