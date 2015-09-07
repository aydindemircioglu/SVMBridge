#!/usr/bin/Rscript  --vanilla 

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


#' createTrainingArguments.BSGD
#'
#' @param	x		SVM object
#' @param	trainDataFile	file to read training data from.
#' @param	modelFile	path to model, defaults to a temporary file (given by R).
#' @param	extraParameter	extra parameters for solver
#' @param	primalTime		
#' @param	wallTime		
#' @param	cost		cost parameter C.
#' @param	gamma		gamma parameter, note: RBF kernel used by pegasos is exp(-0.5 ...).
#' @param	budget		budget parameter.
#' @param	epochs		number of epochs to run.
#'
#' @return	args		arguments for training
createTrainingArguments.BSGD = function (
	x,
	trainDataFile = "",
	modelFile = "",
	extraParameter = "",
	primalTime = 10, 
	wallTime = 8*60,
	cost = 1, 
	gamma = 1, 
	budget = 128,
	epochs = 1, ...) {

		n = R.utils::countLines(trainDataFile)


		# arguments for training
		args = c(
			"-A 4",
			"-r 0",
			sprintf("-B %.16f", budget ),
			sprintf("-L %.16f", (1.0 / (n * cost))),
			sprintf("-e %.16f", epochs ),
			sprintf("-g %.16f", 2 * gamma),
			extraParameter,
			trainDataFile,
			modelFile
		)
		print(c)
		return (args)
}



#' createTestArguments.BSGD
#'
#' @param	x			SVM object
#' @param	testDataFile		file to read test data from.
#' @param	modelFile		path to model, defaults to a temporary file (given by R).
#' @param	predictionOutput	path to where to put prediction output
#'
#' @return	args		arguments for testing
createTestArguments.BSGD = function (
	x,
	testDataFile = "",
	modelFile = "", 
	predictionOutput = "/dev/null", 
	...) {
		args = c(
			"-v 1",
		#        "-o 1", only works for BSGD for now
			testDataFile,
			modelFile,
			predictionOutput 
		)
  
	return (args)
}



#' Extract training information (error rate) from command-line output
#'
#' @param		x 		object
#' @param		output		command-line output from BSGD
#'
extractTrainingInfo.BSGD = function (x, output) {
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    return (err)
}



#' Extract test information (error rate) from command-line output
#'
#' @param		x 		object
#' @param		output		command-line output from BSGD
#'
extractTestInfo.BSGD = function (x, output) {
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    return (err)
}



#' readModel.BSGD
#'
#' @param	x			SVM object
#' @param	modelFile		path to model, defaults to a temporary file (given by R).
#' @param	verbose			print messages?
#'
#' @return	svmatrix		BSGD model
readModel.BSGD = function (x, modelFile = "./model", verbose = FALSE) {
	if (verbose == TRUE) {
		cat("Reading BSGD model from ", modelFile)
	}

	# open connection
	con  <- file(modelFile, open = "r")

	# do we need to invert the labels?
	invertLabels = FALSE

	# grep needed information step by step, the bias is on the threshold line
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) 
	{
		if (grepl("MODEL", oneLine) == TRUE) break;
      
		# gamma value
		if (grepl("KERNEL_GAMMA_PARAM", oneLine) == TRUE) 
		{
		    pattern <- "KERNEL_GAMMA_PARAM: (.*)"
		    gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
		}  
	      
		# bias
		if (grepl("BIAS_TERM", oneLine) == TRUE) 
		{
		    pattern <- "BIAS_TERM: (.*)"
		    bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
		}
      
		# order of labels
		if (grepl("LABELS", oneLine) == TRUE) 
		{
			pattern <- "LABELS: (.*)"
			order = (sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
		
			if ((order != "1 -1") && (order != "-1 1")) {
				stop ("Label ordering %s is unknown!", order)
			}
		
			if (order == "1 -1") {
				invertLabels = FALSE
			}

			if (order == "-1 1") {
				invertLabels = TRUE
			 }
		}  
	}
  
  
	# read and interprete data 
	# basically all data is sparse data format, but the data around this differs
# 	model = readSparseFormat(con)
	
	# these will contain the coefficients and the  svs.
	supportvectors <- matrix()
	coefficients <- matrix()
	weights <- matrix()
	
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
	
		# remove comment if necesary
		oneLine = stringr::str_split_fixed(oneLine, pattern = '#', n = 2)[1]
		
		# split line by " "
		svec = vector(length = 1)
		parts = strsplit (oneLine, " ")
		
		# where the support vector data starts in the row
		fvpos = 1
		coeff = vector(length = 1)
		w = vector (length = 1)
		
		# read part for part until it is something positive
		for (i in seq(1, length(parts[[1]]))) {
			fparts <- strsplit (parts[[1]][i], ":")
			if (!is.na(fparts[[1]][1])) {
				ind = as.numeric(fparts[[1]][1])
				value = as.numeric(fparts[[1]][2])
		      
				# check if we have part of some feature vector
				if (ind > 0) {
					# yes, so quit the whole loop
					fvpos = i
					break
				}
				# if not, we can save it in the coeff container, which contains alpha values with negative index
				coeff[-ind] = value
			}
			else {
				stop ("Should never happen. Really.")
			}
		}
		
		# grep feature vectors one by one
		for (i in fvpos:length(parts[[1]])) {
			# split by :
			fparts <- strsplit (parts[[1]][i], ":")
			
			# if we have anything, add it to our vector
			if (!is.na(fparts[[1]][1])) {
				ind = as.numeric(fparts[[1]][1])
				value = as.numeric(fparts[[1]][2])
				svec[ind] <- value
			}
		}
    
		# make sure our vector has no NAs
		print (svec)
		svec[is.na(svec)] <- 0
		
		# stack matrices
		supportvectors <- plyr::rbind.fill.matrix(supportvectors, t(svec))
		coefficients <- plyr::rbind.fill.matrix(coefficients, t(coeff))
		weights <- plyr::rbind.fill.matrix(weights, t(w))
	}
	
	# crop first NA list (why does it even exist?..)
	supportvectors = supportvectors[-1, ]
	coefficients = coefficients[-1, ]
	weights = weights[-1, ]
	
	# remove possible NA values that occur if the very last
	# entry of a sparse vector is omitted because of sparsity
	supportvectors[is.na(supportvectors)] <- 0
	coefficients[is.na(coefficients)] <- 0 
	weights[is.na(weights)] <- 0 

	model = list("X" = supportvectors, "a" = coefficients, "w" = weights)
	
	# add header information
	model$gamma = gamma
	model$bias = bias
	model$modelname = "BSGD"
	
	
	# do we need to invert the labels? in this case we invert the coefficients
	if (invertLabels == TRUE) {
		if (verbose == TRUE)  
			cat(" Inverting Labels.")

		# invert alphas
		names(model) = replace(names(model), names(model) == "a", "alpha")
		model$alpha = -model$alpha
		
		# this is also needed.. 
		model$bias = -bias
	}

	# close connection
	close(con)
	
	# return
	return (model)
}
 


#' writeModel.BSGD
#'
#' @param	x			SVM object
#' @param	model 			Model to write
#' @param	modelFile		Path to write model to
#' @param	verbose			print messages?
#'
writeModel.BSGD = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
	if (verbose == TRUE) {
		cat ("Writing BSGD Model..\n")
	}

	# BROKEN, FIXME
	
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
# @param	predictionsFile		file to read predictions from
# @return		array consisting of predictions
#

# dummy probably
 
#' Read predictions from BSGD
#'
#' @param	predictionsFile		file to read predictions from
#' @return		array consisting of predictions
#'
readPredictions.BSGD <- function (x, predictionsFilePath = "", verbose = FALSE) {
    # open connection
    con  <- file(predictionsFilePath, open = "r")

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


#' findSoftware.BSGD
#'
#' @param	x			SVM object
#' @param	searchPath		path to search for software
#' @param	verbose			print messages?
#'
#' @return	x			binary path for the software
findSoftware.BSGD = function (x, searchPath = "./", verbose = FALSE) {

		if (verbose == TRUE) {
			BBmisc::messagef("    BSGD Object: Executing search for software for %s", x$method)
		}
		
		trainBinaryPattern = "^budgetedsvm-train$"
		trainBinaryOutputPattern = "budgetedsvm-train .options. train_file .model_file."
		binaryPath = findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)

		# TODO: check for empty path+handling
		
		if (verbose == TRUE) {
			BBmisc::messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^budgetedsvm-predict$"
		testBinaryOutputPattern = "budgetedsvm-predict .options. test_file model_file output_file"

		binaryPath = findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		# TODO: check for empty path+handling

		if (verbose == TRUE) {
			BBmisc::messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
}
