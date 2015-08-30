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



# DUMMY probably
extractTrainingInfo.BSGD = function (x, output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}

# DUMMY probably
extractTestInfo.BSGD = function (x, output) {
    
    # ---- grep the error rate
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
#' @return	svmatrix		svmmatrix object
readModel.BSGD = function (x, modelFile = "./model", verbose = FALSE) {
	if (verbose == TRUE) {
		BBmisc::messagef("Reading BSGD model from %s", modelFile)
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
	svmatrix = readSparseFormat(con)

	# add header information
	svmatrix$gamma = gamma
	svmatrix$bias = bias
	svmatrix$modelname = "BSGD"
	
	
	# do we need to invert the labels? in this case we invert the coefficients
	if (invertLabels == TRUE) {
		if (verbose == TRUE)  
			BBmisc::messagef(" Inverting Labels.")

		# invert alphas
		svmatrix$a = -svmatrix$a

		# this is also needed.. 
		svmatrix$bias = -bias
	}

	# close connection
	close(con)
	
	# return
	return (svmatrix)
}
 


# dummy for now
writeModel.BSGD = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
	if (verbose == TRUE) {
		BBmisc::messagef ("Writing BSGD Model..")
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
#' @param	predictionsFile		file to read predictions from
# @return		array consisting of predictions
#

# dummy probably
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
