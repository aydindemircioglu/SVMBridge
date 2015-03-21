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
 

	
	createTrainingArguments.LASVM = function (
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
		N = countLines(trainDataFile)

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

	
	
	createTestArguments.LASVM = function (
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
	
	
	#NEW
	extractTestInfo.LASVM = function (x, output) {
		pattern <- "accuracy= (\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	

	#NEW
	readModel.LASVM = function (x, modelFile = './model', verbose = FALSE) {
		NextMethod (modelFile = modelFile, verbose = verbose)
	}
	
	
	#NEW
	writeModel.LASVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		NextMethod (model = model, modelFile = modelFile, verbose = verbose)
	}
	
	
	
	#
	# @param[in]	predictionsFile		file to read predictions from
	# @return		array consisting of predictions
	#
	#NEW
	readPredictions.LASVM = function (x, predictionsFile = "", verbose = FALSE) {
		ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
	}

	

	findSoftware.LASVM = function (x, searchPath = "./", verbose = FALSE) {

		if (verbose == TRUE) {
			BBmisc::messagef("    LIBSVM Object: Executing search for software for %s", x$method)
		}
		
		trainBinaryPattern = "^la_svm$"
		trainBinaryOutputPattern = 'Usage: la_svm .options. training_set_file .model_file.'
		binaryPath = findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)

		# TODO: check for empty path+handling
		
		if (verbose == TRUE) {
			BBmisc::messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^la_test$"
		testBinaryOutputPattern = 'Usage: la_test .options. test_set_file model_file output_file'

		binaryPath = findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		# TODO: check for empty path+handling

		if (verbose == TRUE) {
			BBmisc::messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
	}

 
 
	print.LASVM_walltime = function(x) {
		BBmisc::messagef("--- Object: %s", x$method)
		BBmisc::messagef("       Training Binary at %s", x$trainBinaryPath)
		BBmisc::messagef("       Test Binary at %s", x$testBinaryPath)
	}
	