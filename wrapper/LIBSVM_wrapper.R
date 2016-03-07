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



createSVMWrapper.LIBSVM = function() {
  createSVMWrapperInternal(
    name = "LIBSVM",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeDiscreteLearnerParam(id = "kernel", default = "radial", values = c("radial")),
      ParamHelpers::makeNumericLearnerParam(id = "budget",  default = 128, lower = 1),
      ParamHelpers::makeNumericLearnerParam(id = "cost",  default = 1, lower = 0),
      ParamHelpers::makeNumericLearnerParam(id = "epochs",  default = 1, lower = 1),
      ParamHelpers::makeNumericLearnerParam(id = "gamma", default = 1, lower = 0, requires = quote(kernel!="linear")),
      ParamHelpers::makeNumericLearnerParam(id = "tolerance", default = 0.001, lower = 0)
    ),
    properties = c("twoclass", "multiclass"),
    note = ""
  )
}


createTrainingArguments.LIBSVM = function (
	x,
	...,
	trainDataFile = "",
	modelFile = "",
	extraParameter = "",
	kernelCacheSize = 1024,
	cost = 1,
	useBias = FALSE,
	gamma = 1,
	epsilon = 0.001,
	quietMode = FALSE)
{
		svmTypeParameter = "-s 0"
		kernelTypeparameter = "-t 2"

		degreeParameter = ""

		gammaParameter = ""
		if (gamma != 1)
			gammaParameter = sprintf("-g %.16f", gamma)

		coef0Parameter = ""

		costParameter = ""
		if (cost != 1)
			costParameter = sprintf("-c %.16f", cost)

		nuParameter = ""

		epsilonParameter = ""
		if(epsilon != 0.001)
			epsilonParameter = sprintf("-p %.16f", epsilon)

		shrinkingParameter = ""

		probabilityEstimatesparameter = ""

		weightParameter = ""

		quietModeparameter = ""
		if(quietMode != FALSE)
			quietModeparameter = TRUE;

		args = c(
			svmTypeParameter,
			degreeParameter,
			"-t 2",
			sprintf("-m %d", kernelCacheSize), # in MB
			sprintf("-c %.16f", cost),         # rbf kernel
			sprintf("-g %.16f", gamma),        # gamma
			sprintf("-e %.16f", epsilon),      # epsilon tolerance
			extraParameter,
			shQuote (trainDataFile),
			shQuote (modelFile)
		)
		return (args)
}



createTestArguments.LIBSVM = function (x, testDataFile = NULL, modelFile = NULL, predictionsFile = NULL, verbose = FALSE, ...) {
    args = c(
        shQuote (testDataFile),
        shQuote (modelFile),
        predictionsFile
    )
    return (args)
}



extractTrainingInfo.LIBSVM = function (x, output, verbose) {
		pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
}



extractTestInfo.LIBSVM = function (x, output, verbose) {
		pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
}



readModel.LIBSVM = function (x,modelFile = "./model",	verbose = FALSE) {
		return (readLIBSVMModel (modelFile = modelFile, verbose = verbose) )
}



writeModel.LIBSVM = function (x,model = NA,	modelFile = "./model", verbose = FALSE) {
		return (writeLIBSVMModel (model = model, modelFile = modelFile, verbose = verbose) )
}



detectModel.LIBSVM = function (x, modelFile = NULL, verbose = FALSE) {
	checkmate::checkFlag (verbose)

	if (verbose == TRUE) {
		cat ("Checking for LIBSVM model.\n")
	}

	if (is.null (modelFile) == TRUE)
		return (FALSE)

	# read first lines and detect magic marker
	if (file.exists (modelFile) == FALSE)
		return (FALSE)

	line = readLines(modelFile, n = 12)
	if (sum(grepl("total_sv", line)) > 0) {
		return (TRUE)
	}

	return (FALSE)
}



readPredictions.LIBSVM = function (x, predictionsFile = "", verbose = FALSE) {
	# open connection
	con  <- file(predictionsFile, open = "r")

	predictions = c()
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
		predictions = c(predictions, as.numeric(oneLine))
	}

	if (verbose == TRUE) {
		print(predictions)
	}

	close (con)

	return (predictions)
}



findSoftware.LIBSVM = function (x, searchPath = "./", execute = TRUE, verbose = FALSE) {

	if (verbose == TRUE) {
		cat("    LIBSVM Object: Executing search for software for ", x$method, "\n")
	}

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

	x$trainBinaryPath = findBinaryInDirectory (trainBinaryPattern, dir = searchPath, patterns = list ('2 -- radial basis function: exp', '.q : quiet mode .no outputs'), verbose = verbose )
	x$testBinaryPath = findBinaryInDirectory (testBinaryPattern, dir = searchPath, patterns = list ('for one-class SVM only 0 is supported'), verbose = verbose )

	return(x)
}



print.LIBSVM = function(x) {
	cat("Solver: ", x$method, "\n")
	cat("    Training Binary at ", x$trainBinaryPath, "\n")
	cat("    Test Binary at ", x$testBinaryPath, "\n")
}
