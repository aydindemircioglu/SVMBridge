#
# SVMBridge
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


createTrainingArguments.LASVM = function (x,
	trainDataFile = "",
      modelFile = "",
      extraParameter = "",
      kernelCacheSize = 1024,
	cost = 1,
      useBias = FALSE,
      gamma = 1,
      epochs = 1,
      epsilon = 0.001,
      ...)
{
	# count training examples
	N = R.utils::countLines(trainDataFile)

	biasParameter = "-b 0"
	if (useBias == TRUE)
		biasParameter = "-b 1"

	args = c(
		sprintf("-m %d", kernelCacheSize), # in MB
		biasParameter,
		sprintf("-g %.16f", gamma),
		sprintf("-c %.16f", cost),
		sprintf("-e %.16f", epsilon),
		sprintf("-p %.16f", epochs),
		extraParameter,
		trainDataFile,
		modelFile
	)

	return (args)
}



createTestArguments.LASVM = function (x, testDataFile = NULL, modelFile = NULL, predictionsFile = NULL, verbose = FALSE, ...) {
    args = c(
        testDataFile,
        modelFile,
        predictionsFile
    )
    return (args)
}



extractTrainingInfo.LASVM = function (x, output, verbose) {
	pattern <- "accuracy= (\\d+\\.?\\d*).*"
	error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
	return (error)
}



extractTestInfo.LASVM = function (x, output, verbose) {
	pattern <- "accuracy= (\\d+\\.?\\d*).*"
	error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
	return (error)
}



readModel.LASVM = function (x, modelFile = './model', verbose = FALSE) {
	ret = readLIBSVMModel(modelFile = modelFile, verbose = verbose)
	return (ret)
}



writeModel.LASVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
	ret = writeLIBSVMModel (model = model, modelFile = modelFile, verbose = verbose)
	return (ret)
}



detectModel.LASVM = function (x, modelFile = NULL, verbose = FALSE) {
	checkmate::checkFlag (verbose)
	if (is.null (modelFile) == TRUE)
		return (FALSE)

	# read first lines and detect magic marker
	if (file.exists (modelFile) == FALSE)
		return (FALSE)

	line = readLines(modelFile, n = 1)
	if (line == "svm_type c_svc") {
		return (TRUE)
	}

	return (FALSE)
}



checkModel.LASVM = function (model) {
	check = TRUE
	check = check & checkmate::testNumeric (model$gamma)
	check = check & checkmate::assertInteger (model$nSV)
	return (check)
}



readPredictions.LASVM = function (x, predictionsFile = "", verbose = FALSE) {
	ret = readLIBSVMPredictions (predictionsFile = predictionsFile, verbose = verbose)
	return (ret)
}



findSoftware.LASVM = function (x, searchPath = "./", execute = FALSE, verbose = FALSE) {

	if (verbose == TRUE) {
		cat ("    LASVM Object: Executing search for software for ", x$method)
	}

	# can do now OS specific stuff here
	if(.Platform$OS.type == "unix") {
		if (verbose == TRUE) {
			cat ("    Unix binaries.\n")
		}
		trainBinaryPattern = "la_svm"
		testBinaryPattern = "la_test"
	} else {
		if (verbose == TRUE) {
			cat ("    Windows binaries.\n")
		}
		trainBinaryPattern = "la_svm.exe"
		testBinaryPattern = "la_test.exe"
	}

	# search binary
	x$trainBinaryPath = findBinaryInDirectory (trainBinaryPattern, dir = searchPath, patterns = list ('la_svm .options. training_set_file .model_file.'))
	x$testBinaryPath = findBinaryInDirectory (testBinaryPattern, dir = searchPath, patterns = list ('la_test .options. test_set_file model_file output_file'))

	return(x)
}



print.LASVM = function(x) {
	cat("Solver: ", x$method)
	cat("    Training Binary at ", x$trainBinaryPath)
	cat("    Test Binary at ", x$testBinaryPath)
}

