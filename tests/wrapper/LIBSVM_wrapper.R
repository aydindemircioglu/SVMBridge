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


createTrainingArguments.LIBSVM = function (
	x,
	...,
	trainDataFile = "",
	modelFile = "",
	extraParameter = "",
	kernelCacheSize = 1024,
	cost = 1,
	svmType = "-1",
	useBias = FALSE,
	gamma = 1,
	epsilon = 0.001,
	degree = -1,
	coef0 = -1,
	nu = -1,
	shrinking = -1,
	probabilityEstimates = -1,
	weight = -1,
	n = -1,
	kernelType = "rbf",
	quietMode = FALSE)
{
		svmTypeParameter = ""
		if (svmType == "CSVC" || svmType == "C-SVC")
			svmTypeParameter = "-s 0"
		if (svmType == "nuSVC" || svmType == "nu-SVC")
			svmTypeParameter = "-s 1"
		if (svmType == "one-class SVM" || svmType == "oneClassSVM")
			svmTypeParameter = "-s 2"

		kernelTypeParameter = ""
		if(kernelType == "linear")
			kernelTypeparameter = "-t 0"
		if(kernelType == "polynomial")
			kernelTypeparameter = "-t 1"
		if(kernelType == "radial basis function" || kernelType == "RBF" || kernelType == "rbf")
			kernelTypeparameter = "-t 2"
		if(kernelType == "sigmoid")
			kernelTypeparameter = "-t 3"
		if(kernelType == "precomputed kernel" || kernelType == "precomputed")
			kernelTypeparameter = "-t 4"

		degreeParameter = ""
		if (degree != -1) {
			degreeParameter = sprintf("-d %d", degree)
		}

		gammaParameter = ""
		if (gamma != 1)
			gammaParameter = sprintf("-g %.16f", gamma)

		coef0Parameter = ""
		if (coef0 != -1)
			coef0Parameter = sprintf("-r %d", coef0)

		costParameter = ""
		if (cost != 1)
			costParameter = sprintf("-c %.16f", cost)

		nuParameter = ""
		if(nu != -1)
			nuParameter = sprintf("-n %f", nu)

		epsilonParameter = ""
		if(epsilon != 0.001)
			epsilonParameter = sprintf("-p %.16f", epsilon)

		shrinkingParameter = ""
		if(shrinking != -1)
			shrinkingParameter = sprintf("-h %d", shrinking)

		probabilityEstimatesparameter = ""
		if(probabilityEstimates != -1)
			probabilityEstimatesparameter = sprintf("-b %d", probabilityEstimates)

		weightParameter = ""
		if(weight != -1)
			weightParameter = sprintf("-wi %d", weight)

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
			trainDataFile,
			modelFile
		)
		return (args)
}



createTestArguments.LIBSVM = function (x,
	testDataFile = "",
	modelFile = "",
	predictionsFile = "",
	...) {
		args = c(
			testDataFile,
			modelFile,
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

