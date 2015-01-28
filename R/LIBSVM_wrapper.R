#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		LIBSVM_wrapper.R
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
 
	
	createTrainingArguments.LIBSVM = function (x,
		trainDataFile = "",
        modelFile = "",
        extraParameter = "",
        kernelCacheSize = 1024,
		cost = 1, 
        useBias = FALSE,
        gamma = 1,
        epsilon = 0.001, 
        ...) 
	{
		args = c(
			"-s 0",                         # c classification
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
		...) 
	{
		args = c(
			testDataFile,
			modelFile,
			predictionsFile
		)
    
		return (args)
	}


	
	extractTrainingInfo.LIBSVM = function (x, output) {
		pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	
	
	
	extractTestInfo.LIBSVM = function (x, output) {
		pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
	}
	

	
	readModel.LIBSVM = function (x, modelFile = './model', verbose = FALSE) {
		# open connection
		con  <- file(modelFile, open = "r")

		while ((oneLine <- readLines(con, n = 1, warn = FALSE)) != "SV") {
			# gamma value
			if (grepl("gamma", oneLine) == TRUE) {
				pattern <- "gamma (.*)"
				gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			}  
		
			# rho/bias
			if (grepl("rho", oneLine) == TRUE) {
				pattern <- "rho (.*)"
			bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			}
			
			# order of labels
			if (grepl("label", oneLine) == TRUE) {
				pattern <- "label (.*)"
				order = (sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			
				if ((order != "1 -1") && (order != "-1 1")) {
					stop ("Label ordering %s is unknown!", order)
				}
				# LABEL ORDERING IS NOT USED for libsvm!
			}  
		}
	
	
		# read and interprete data 
		# basically all data is sparse data format, but the data around this differs
		svmatrix = readSparseFormat(con)

	
		# add header information
		svmatrix$gamma = gamma
		svmatrix$bias = bias
		svmatrix$modelname = "LIBSVM"
		
		# close connection
		close(con)
		
		# return
		return (svmatrix)
	}



	writeModel.LIBSVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		if (verbose == TRUE) {
			messagef ("Writing SVM Model..")
		}
		
		# FIXME: label order
		# TODO: support multiclass
		model$nrclass = 2
		posSV = sum(model$a > 0)
		negSV = sum(model$a < 0)
		# open connection
		modelFileHandle <- file(modelFile, open = "w+")
		writeLines(paste ("svm_type c_svc", sep = ""), modelFileHandle )
		writeLines(paste ("kernel_type", "rbf", sep = " "), modelFileHandle )
		writeLines(paste ("gamma", model$gamma, sep = " "), modelFileHandle )
		writeLines(paste ("nr_class", model$nrclass, sep = " "), modelFileHandle )
		writeLines(paste ("total_sv", length(model$a), sep = " "), modelFileHandle )
		writeLines(paste ("rho", model$bias, sep = " "), modelFileHandle )
		writeLines(paste ("label 1 -1", sep = " "), modelFileHandle )
		writeLines(paste ("nr_sv", posSV, negSV, sep = " "), modelFileHandle )
		writeLines(paste ("SV", sep = ""), modelFileHandle )

		# basically all data is sparse data format, but the data around this differs
		svmatrix = dumpSparseFormat(model$a, model$X)
		writeLines(svmatrix, modelFileHandle, sep = "" )
		
		# close connection
		close(modelFileHandle)
	}
 


 	#
	# @param[in]	predictionsFile		file to read predictions from
	# @return		array consisting of predictions
	#
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

	
	
	findSoftware.LIBSVM = function (x, searchPath = "./", verbose = FALSE) {

		if (verbose == TRUE) {
			messagef("    LIBSVM Object: Executing search for software for %s", x$method)
		}
		
		trainBinaryPattern = "^svm-train$"
		trainBinaryOutputPattern = 'Usage: svm-train .options. training_set_file .model_file.'

		binaryPath = findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)

		if (verbose == TRUE) {
			messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^svm-predict$"
		testBinaryOutputPattern = 'for one-class SVM only 0 is supported'

		binaryPath = findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		if (verbose == TRUE) {
			messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
	}

	
	print.LIBSVM = function(x) {
		messagef("--- Object: %s", x$method)
		messagef("       Training Binary at %s", x$trainBinaryPath)
		messagef("       Test Binary at %s", x$testBinaryPath)
	}
	