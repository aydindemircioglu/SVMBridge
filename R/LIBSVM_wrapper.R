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
        quietMode = FALSE,
        ...) 
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
		if (verbose == TRUE) {
			BBmisc::messagef ("Reading LIBSVM model from %s.", modelFile)
		}
		
		# open connection
		con  <- file(modelFile, open = "r")
		oneLine = readLines(con, n = 1, warn = FALSE)
		while ( !is.na(oneLine) && oneLine != "SV") {
			# gamma value
			if (grepl("gamma", oneLine) == TRUE) {
				pattern <- "gamma (.*)"
				gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			}  
		
			# rho/bias
			if (grepl("rho", oneLine) == TRUE) {
				#print(oneLine)
				bias = numeric()
				rhoLine = unlist(strsplit(oneLine, split = "\\s"))
				for(value in rhoLine){
					if(value != "rho"){
						bias = c(bias, value)
					}
				}
				#pattern <- "rho (.*)"
				#bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			}
			
			# order of labels
			if (grepl("label", oneLine) == TRUE) {
				label = numeric()
				labelLine = unlist(strsplit(oneLine, split = "\\s"))
				for(value in labelLine){
					if(value != "label")
						label = c(label, value)
				}
				
				print("TEST: "
				)
				print(label)
				print(length(label))
			
			
			
			
# 				pattern <- "label (.*)"
# 				order = (sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)]))
# 				if ((order != "1 -1") && (order != "-1 1")) {
# 					stop ("Label ordering %s is unknown!", order)
# 				}
				#LABEL ORDERING IS NOT USED for libsvm!
			}  
			
			if (grepl("svm_type", oneLine) == TRUE) {
				pattern <- "rho (.*)"
				svm_type = sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])
			}
			oneLine = readLines(con, n = 1, warn = FALSE)
		}
	
	
		# read and interprete data 
		# basically all data is sparse data format, but the data around this differs
		svmatrix = readSparseDataFromConnection(con)

	
		# add header information
		if(exists("gamma"))
			svmatrix$gamma = gamma
		if(exists("label"))
			svmatrix$label = label
		if(exists("bias"))
			svmatrix$bias = bias
		svmatrix$modeltype = "LIBSVM"
		
		# close connection
		close(con)

		# return
		retObj = BBmisc::makeS3Obj("SVMModel", svmatrix)
		return (svmatrix)
	}



	writeModel.LIBSVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		if (verbose == TRUE) {
			BBmisc::messagef ("Writing SVM Model..")
		}
		
		model$alpha = model$Y
		# FIXME: label order
		# TODO: support multiclass
		model$nrclass = length(model$label)
		posSV = sum(model$alpha > 0)
		negSV = sum(model$alpha < 0)
		# open connection
		modelFileHandle <- file(modelFile, open = "w+")
		writeLines(paste ("svm_type c_svc", sep = ""), modelFileHandle )
		writeLines(paste ("kernel_type", "rbf", sep = " "), modelFileHandle )
		gammaValue = model$gamma
		if(is.numeric(gammaValue))
			writeLines(paste ("gamma", model$gamma, sep = " "), modelFileHandle )
		writeLines(paste ("nr_class", model$nrclass, sep = " "), modelFileHandle )
		writeLines(paste ("total_sv", length(model$alpha), sep = " "), modelFileHandle )
		biasvalues = paste(model$bias, collapse = " ")
		writeLines(paste ("rho", biasvalues, sep = " "), modelFileHandle )
		labelvalues = paste(model$label, collapse = " ")
		writeLines(paste ("label",labelvalues,  sep = " "), modelFileHandle )
		writeLines(paste ("nr_sv", posSV, negSV, sep = " "), modelFileHandle )
		writeLines(paste ("SV", sep = ""), modelFileHandle )

		# basically all data is sparse data format, but the data around this differs
		#svmatrix = dumpSparseFormat(model$alpha, model$X)
		#writeLines(svmatrix, modelFileHandle, sep = "" )
		writeSparseDataToConnection(modelFileHandle, model$X, model$Y)
		
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
			BBmisc::messagef("    LIBSVM Object: Executing search for software for %s", x$method)
		}
		
		trainBinaryPattern = "^svm-train$"
		trainBinaryOutputPattern = c('saveExponential : set exponential',
			'.q : quiet mode .no outputs')

		binaryPath = findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)

		if (verbose == TRUE) {
			BBmisc::messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^svm-predict$"
		testBinaryOutputPattern = 'for one-class SVM only 0 is supported'

		binaryPath = findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		if (verbose == TRUE) {
			BBmisc::messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
	}

	
	
	print.LIBSVM = function(x) {
		BBmisc::messagef("--- Object: %s", x$method)
		BBmisc::messagef("       Training Binary at %s", x$trainBinaryPath)
		BBmisc::messagef("       Test Binary at %s", x$testBinaryPath)
	}
	
