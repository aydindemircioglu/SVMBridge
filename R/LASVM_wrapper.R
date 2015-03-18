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


	
	extractTrainingInfo.LASVM = function (output) {
		pattern <- "accuracy= (\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	
	
	#NEW
	extractTestInfo.LASVM = function (output) {
		pattern <- "accuracy= (\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	

	#NEW
	readModel.LASVM = function (modelFile = './model', verbose = FALSE) {
		NextMethod (modelFile = modelFile, verbose = verbose)
	}
	
	
	#NEW
	writeModel.LASVM = function (model = NA, modelFile = "./model", verbose = FALSE) {
		NextMethod (model = model, modelFile = modelFile, verbose = verbose)
	}
	
	
	
	#
	# @param[in]	predictionsFile		file to read predictions from
	# @return		array consisting of predictions
	#
	#NEW
	readPredictions.LASVM = function (predictionsFilePath = "", verbose = FALSE) {
		p = NextMethod (predictionsFilePath = predictionsFilePath, verbose = verbose)
		return (p)
	}

	
	#NEW
	findSoftware.LASVM = function(x, searchPath = "./", verbose = FALSE) {
		# short way without verbose messages
		x$trainBinaryPath  = findBinary (searchPath, "^svm-train$", "Usage: svm-train .options. training_set_file .model_file.", verbose = verbose)
	}




 
