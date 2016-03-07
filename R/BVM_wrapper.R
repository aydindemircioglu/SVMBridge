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


createSVMWrapper.BVM = function() {
  createSVMWrapperInternal(
    name = "BVM",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("radial")),
      ParamHelpers::makeNumericLearnerParam(id = "budget",  default = 128, lower = 1),
      ParamHelpers::makeNumericLearnerParam(id = "cost",  default = 1, lower = 0),
      ParamHelpers::makeNumericLearnerParam(id = "epochs",  default = 1, lower = 1),
      ParamHelpers::makeNumericLearnerParam(id = "gamma", default = 1, lower = 0, requires = quote(kernel!="linear")),
      ParamHelpers::makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0)
    ),
    properties = c("twoclass", "multiclass"),
    note = "Ball Vector Machine"
  )
}



createTrainingArguments.BVM = function (x,
    trainDataFile = "",
    modelFile = "",
    extraParameter = "",
    kernelCacheSize = 1024,
    cost = 1,
    gamma = 1,
    epsilon = 0.001, ...) {

    args = c(
        "-s 6",                         # CVM = 6, BVM = 9
        "-t 2",
        sprintf("-c %.16f", cost),
        sprintf("-m %d", kernelCacheSize), # in MB
        sprintf("-g %.16f", gamma),
        sprintf("-e %.16f", epsilon),
        extraParameter,
        shQuote(trainDataFile),
        shQuote(modelFile)
    )

    return (args)
}



createTestArguments.BVM = function (x, testDataFile = NULL, modelFile = NULL, predictionsFile = NULL, verbose = FALSE, ...) {
    args = c(
        shQuote(testDataFile),
        shQuote(modelFile),
        shQuote(predictionsFile)
    )
    return (args)
}



extractTrainingInfo.BVM = function (x, output, verbose) {
    pattern <- "Accuracy = (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    return (err)
}



extractTestInfo.BVM = function (x, output, verbose) {
    pattern <- "Accuracy = (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    return (err)
}



readModel.BVM = function (x, modelFile = './model', verbose = FALSE) {
	ret = readLIBSVMModel (modelFile = modelFile, verbose = verbose)
	return (ret)
}



writeModel.BVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
	ret = writeLIBSVMModel (model = model, modelFile = modelFile, verbose = verbose)
	return (ret)
}



detectModel.BVM = function (x, modelFile = NULL, verbose = FALSE) {
	checkmate::checkFlag (verbose)

	if (verbose == TRUE) {
		cat ("Checking for BVM model.\n")
	}

	if (is.null (modelFile) == TRUE)
		return (FALSE)

	# read first lines and detect magic marker
	if (file.exists (modelFile) == FALSE)
		return (FALSE)

	line = readLines(modelFile, n = 1)
	if (line == "svm_type bvm") {
		return (TRUE)
	}

	return (FALSE)
}



readPredictions.BVM = function (x, predictionsFile = "", verbose = FALSE) {
	ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
	return (ret)
}



findSoftware.BVM = function (x, searchPath = "./", execute = FALSE, verbose = FALSE) {

	# can do now OS specific stuff here
	if(.Platform$OS.type == "unix") {
		if (verbose == TRUE) {
			cat ("    Unix binaries.\n")
		}
		trainBinaryPattern = "svm-train"
		testBinaryPattern = "svm-predict"
	} else {
		if (verbose == TRUE) {
			cat ("    Windows binaries.\n")
		}
		trainBinaryPattern = "svm-train.exe"
		testBinaryPattern = "svm-predict.exe"
	}

	# can do now OS specific stuff here
	x$trainBinaryPath = findBinaryInDirectory (trainBinaryPattern, dir = searchPath, patterns = list ('bvm-train .options. training_set_file .model_file.'))
	x$testBinaryPath = findBinaryInDirectory (testBinaryPattern , dir = searchPath, patterns = list ('bvm-predict .options. test_file model_file output_file'))

	return (x)
}



print.BVM = function(x) {
	cat("Solver: ", x$method)
	cat("    Training Binary at ", x$trainBinaryPath)
	cat("    Test Binary at ", x$testBinaryPath)
}
