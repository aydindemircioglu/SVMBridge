#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		trainSVM.R
# 
# SVMBridge is free software: you can redistribtrainSVMute it and/or modify
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
 

#' trainSVM
#'
#' @param     trainDataFile       file to read training data from 
#' @param     trainDataX		matrix to read training data from 
#' @param     trainDatay		matrix to read training label from 
#'
#' @param     trainBinaryPath		full path to the training binary to call
#'
#' @param     method		name of the SVM method/solver
#' @param     extraParameter  extra parameters for solver
#'
#' @param     model		list containing a trained SVM model
#' @param     modelFile       path to a model file
#' @param     verbose		be verbose?
#'
#' @param     trainingParameterCallBack 		 function that returns command line for training
#' @param     extractInformationCallBack 		 function that extracts information from testing output
#' @param     readModelCallBack 		 function that reads a model into a list
#'
#' @note		exclusive parameters, i.e. you cannot specify both:
#'
#' @return		SVM Object
#' @export		trainSVM
# #examples
# #	trainSVM(model = 'LIBSVM', trainDataFile = './data/australian.train') 
#'

trainSVM = function(
	# method
	method = NULL,

	# data
	trainDataFile = NULL,
	trainDataX = NULL, 
	trainDatay = NULL, 

	# full path to executable
	trainBinaryPath = NULL,
	
	# rest
	extraParameter = "",
	modelFile = NULL,
	verbose = FALSE,

	# data
	subsamplingRate = NULL,
	subsamplingMethod = "cutoff",
	
	# SVMBridge
	timeOut = -1,
	readModelFile = FALSE,
	...) {
	
	#expand possible tilde characters in the path and get rid of backslashes
	trainDataFile = path.expand(trainDataFile)
	trainDataFile = gsub("[\\]", "/", trainDataFile)
	cat(trainDataFile, "\n")
	# get the correct object
	SVMObject = SVMBridgeEnv$packages[[method]]
	
	# ask object for its path
	trainBinaryPath = SVMObject$trainBinaryPath
	trainBinary = basename(trainBinaryPath)
	
	# general modifications
	if (verbose == TRUE) {
		BBmisc::messagef("  Path of binary for training is %s", trainBinaryPath)
		BBmisc::messagef("  Binary for training is %s", trainBinary)
	}

	
	# TODO: sanity checks for parameter
	if (is.null (method) == TRUE) {
		BBmisc::stopf("No method name is given, this should never happen.")
	}
				
 	
	# did the user specify a modelfile?
	if (is.null(modelFile) == TRUE) {
		# if no, we will write the model from memory in a temp file
		modelFile = tempfile()
	}
		

	# take care of data. if testdata or traindata is given,
	# the corresponding filename must be empty.
	# finally, everything is dumped to disk.
  
	if ( (is.null(trainDataX) == FALSE) && (is.null(trainDataFile) == FALSE))
	{
		print(trainDataX)
		print(trainDataFile)
		BBmisc::stopf("Given a data frame as training data and specified a training file name. Confused. Stopping.")
	}		
	if ( (is.null(trainDataX) == TRUE) && (is.null(trainDataFile) == TRUE))
		BBmisc::stopf("Neither specified training data path nor given training data. Stopping.")

	# TODO: check for X AND y.
		
	# we got 
	if (is.null(trainDataX) == FALSE) {
		trainDataFile = tempfile()
		if (verbose == TRUE)
			BBmisc::messagef("  Writing given data as %s", trainDataFile)
		e1071::write.matrix.csr(trainDataX, trainDataFile, trainDatay)
	} 

	if (verbose == TRUE) 
		BBmisc::messagef("  Train Data is now in %s", trainDataFile)

	results = list()

	
	# subsample the file
	if (is.null(subsamplingRate) == FALSE) {
		
		# depending on method
		if (subsamplingMethod == "cutoff") {
			# simple cut off
			trainDataFile = subsampleDataByCutoff (trainDataFile, subsamplingRate = as.numeric(subsamplingRate)) 
		} else {
			# unknown method
			stop("Unknown subsampling method.")
		}
	}

	
	# create arguments for training
	args = createTrainingArguments (SVMObject, 
		trainDataFile = trainDataFile, 
		modelFile = modelFile,
		extraParameter = extraParameter,
		...)

	# now for some 'magic'.
	# some solver might create their own training script,
	# e.g. when calling matlab or similar.
	# in this case the argument list will have an attribute
	# called dynamicBinary. If this exists, it will override
	# the software path as we know it.
	
	if (!is.null(attributes(args)$dynamicBinary)) {
		trainBinaryPath = attributes(args)$dynamicBinary
	}
		
	# if timeout is specified, we HARD kill the process. 
	# else things like SVMperf may run to the cluster walltime
	# instead of stopping at the walltime specified.
	# Careful! On windows systems, this function will not work since 
	# certain necessities are not preinstalled. 
	
	if(.Platform$OS.type == "unix") {
	
		if (timeOut != -1) {
			timeOutArgs = c(sprintf("%d", timeOut), trainBinaryPath, args)
			timeOutPath = "/usr/bin/timeout"
			verbose = TRUE
			if (verbose == TRUE) 
				BBmisc::messagef("  Applying hard timeout of %f seconds.", timeOut)
			trainTime = microbenchmark::microbenchmark(s <- system3(timeOutPath, timeOutArgs, verbose = verbose), times = 1L)$time / 1e9
		} else {
			trainTime = microbenchmark::microbenchmark(s <- system3(trainBinaryPath, args, verbose = verbose), times = 1L)$time / 1e9
		}
	}
	
	if (verbose == TRUE) 
		BBmisc::messagef("Training took %f seconds.", trainTime)
		
	results[["trainTime"]] = trainTime
	
	
	if (readModelFile == TRUE) {
		if (verbose == TRUE) 
			BBmisc::messagef( "Will read model back from %s", modelFile)

		model = readModel (SVMObject, modelFile = modelFile, verbose = verbose)
		results[["model"]] = model
	}

	return (results)   
}

