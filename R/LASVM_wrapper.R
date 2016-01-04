#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		LASVM_wrapper.R
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

	
	
	createTestArguments.LASVM = function (x, 
		testDataFile = "",
		modelFile = "", 
		predictionsFile = "",
		...) 
	{
		args = c(
			testDataFile,
			modelFile,
			predictionsFile
		)
    
		return (args)
	}


	
	extractTrainingInfo.LASVM = function (x, output) {
		pattern <- "accuracy= (\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	
	
	
	extractTestInfo.LASVM = function (x, output) {
		pattern <- "accuracy= (\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	

	
	readModel.LASVM = function (x, modelFile = './model', verbose = FALSE) {
		ret = readModel.LIBSVM (modelFile = modelFile, verbose = verbose)
		return (ret)
	}
	
	
	
	writeModel.LASVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		ret = writeModel.LIBSVM (model = model, modelFile = modelFile, verbose = verbose)
		return (ret)
	}
	

#' Detect whether a file is a model for LASVM.
#'
#' @param	x		Object
#' @param	modelFile		File to check 
#' @param	verbose		Verbose output?
#'
#' @return	TRUE if the given modelFile exists and fits the LASVM model, or FALSE if not.
#'
#' @note	This is a very basic check, enough to distinguish the wrappers provided within the SVMBridge

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

	
	
	#
	# @param[in]	predictionsFile		file to read predictions from
	# @return		array consisting of predictions
	#
	
	
	readPredictions.LASVM = function (x, predictionsFile = "", verbose = FALSE) {
		ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
	}

	

	findSoftware.LASVM = function (x, searchPath = "./", execute = FALSE, verbose = FALSE) {

		if (verbose == TRUE) {
			cat ("    LASVM Object: Executing search for software for ", x$method)
		}

		# we only test if the train binary exists or not. if it does, we add it to the object
		trainBinaryPath = file.path (searchPath, "la_svm")
		if (file.exists (trainBinaryPath) == TRUE) {
			x$trainBinaryPath = trainBinaryPath 
			if (verbose == TRUE) {
				cat ("    Found train binary at ", trainBinaryPath) 
			}
			
			# execute if necessary
			if (execute == TRUE) {
				checkExecutionStrings (trainBinaryPath, patterns = list ('la_svm .options. training_set_file .model_file.'))
			}
		}
		
		# try predict binary now
		testBinaryPath = file.path (searchPath, "la_test")
		if (file.exists (trainBinaryPath) == TRUE) {
			x$testBinaryPath = testBinaryPath
			if (verbose == TRUE) {
				cat ("    Found test binary at ", testBinaryPath) 
			}
			
			# execute if necessary
			if (execute == TRUE) {
				checkExecutionStrings (trainBinaryPath, patterns = list ('la_test .options. test_set_file model_file output_file'))
			}
		}

		return(x)
	}
	
	
 
	print.LASVM_walltime = function(x) {
		BBmisc::messagef("--- Object: %s", x$method)
		BBmisc::messagef("       Training Binary at %s", x$trainBinaryPath)
		BBmisc::messagef("       Test Binary at %s", x$testBinaryPath)
	}
	
