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


createTrainingArguments.CVM = function (x,
						trainDataFile = "",
						modelFile = "",
						extraParameter = "",
						primalTime = 10,
						wallTime = 8*60,
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
        trainDataFile,
        modelFile
    )

    return (args)
}



createTestArguments.CVM = function (x,
					testDataFile = "",
					modelFile = "", ...) {
    args = c(
        testDataFile,
        modelFile,
        "/dev/null"                     # outfile, not needed
    )

    return (args)
}



extractTrainingInfo.CVM = function (x, output, verbose) {
    pattern <- "Accuracy = (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    return (err)
}



extractTestInfo.CVM = function (x, output, verbose) {
    pattern <- "Accuracy = (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    return (err)
}


readModel.CVM = function (x, modelFile = './model', verbose = FALSE) {
	ret = readModel.LIBSVM (modelFile = modelFile, verbose = verbose)
	return (ret)
}


writeModel.CVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
	ret = writeModel.LIBSVM (model = model, modelFile = modelFile, verbose = verbose)
	return (ret)
}



detectModel.CVM = function (x, modelFile = NULL, verbose = FALSE) {
	checkmate::checkFlag (verbose)
	if (is.null (modelFile) == TRUE)
		return (FALSE)

	# read first lines and detect magic marker
	if (file.exists (modelFile) == FALSE)
		return (FALSE)

	line = readLines(modelFile, n = 1)
	if (line == "svm_type cvc") {
		return (TRUE)
	}

	return (FALSE)
}



readPredictions.CVM = function (x, predictionsFile = "", verbose = FALSE) {
	ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
	return (ret)
}



findSoftware.CVM = function (x, searchPath = "./", execute = FALSE, verbose = FALSE) {

	if (verbose == TRUE) {
		cat ("    CVM Object: Executing search for software for ", x$method)
	}

	# can do now OS specific stuff here
	x$trainBinaryPath = findBinaryInDirectory ("svm-train", dir = searchPath, patterns = list ('bvm-train .options. training_set_file .model_file.'))
	x$testBinaryPath = findBinaryInDirectory ("svm-predict", dir = searchPath, patterns = list ('bvm-predict .options. test_file model_file output_file'))

	return(x)
}



print.CVM = function(x) {
	cat("Solver: ", x$method)
	cat("    Training Binary at ", x$trainBinaryPath)
	cat("    Test Binary at ", x$testBinaryPath)
}

