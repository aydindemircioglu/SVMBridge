#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		universalSVMTrainer.R
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
 

#' testSVM
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
#' @export
#' @examples
#' 	universalSVMTrainer(trainDataFile = './data/australian.sparse') 
#'

testSVM = function(
	method = NULL,

	# data
	testDataFile = NULL,
	testDataX = NULL, 
	testDatay = NULL, 

	# model 
	model = NULL,
	modelFile = NULL,

	# rest
	extraParameter = "",
	verbose = FALSE,
	...) {

	if (verbose == TRUE) {
		messagef("--- Testing...")
	}

	# get the correct object
	SVMObject = SVMBridgeEnv$packages[[method]]
	
	# ask object for its path
	testBinaryPath = SVMObject$testBinaryPath
	testBinary = basename(testBinaryPath)
	
	
	# general modifications
	if (verbose == TRUE) {
		messagef("  Path of binary for testing is %s", testBinaryPath)
		messagef("  Binary for testing is %s", testBinary)
	}

	# TODO: sanity checks for parameter
	if (is.null (method) == TRUE) {
		stopf("No method name is given, this should never happen.")
	}
				

	# take care of data. if testdata or traindata is given,
	# the corresponding filename must be empty.
	# finally, everything is dumped to disk.
  
	if ( (is.null(testDataX) == FALSE) && (is.null(testDataFile) == FALSE))
		stopf("Given a data frame as testing data and specified a testing file name. Confused. Stopping.")
			
	if ( (is.null(testDataX) == TRUE) && (is.null(testDataFile) == TRUE))
		stopf("Neither specified testing data path nor given testing data. Stopping.")


	# test model
	if ( (is.null(model) == FALSE) && (is.null(modelFile) == FALSE))
		stopf("Given a model in memory and specified a model file name. Confused. Stopping.")
			
	if ( (is.null(model) == TRUE) && (is.null(modelFile) == TRUE))
		stopf("Neither given a model nor given a path to the model. Stopping.")


	# TODO: check for X AND y.
		
	# we got 
	if (is.null(testDataX) == FALSE) {
		testDataFile = tempfile()
		if (verbose == TRUE)
			messagef("  Writing given test data as %s", testDataFile)
		write.matrix.csr(testDataX, testDataFile, testDatay)
	} 

	if (verbose == TRUE) 
		messagef("  Test Data is now in %s", testDataFile)

	results = list()

	
	# if the user specified a model in memory, we first need to write that
	if (is.null(model) == FALSE) {
		modelFile = tempfile()
		if (verbose == TRUE)
			messagef("  Writing model in memory to disk as %s", modelFile)
		
		writeModel (SVMObject, modelFile = modelFile, verbose = verbose)
	}

	# TODO: add prediction as an option, so people can have them permanently on disk??
	predictionsFilePath = tempfile()
	
	# retrieve test parameters
	args = createTestArguments (SVMObject, 
		testDataFile = testDataFile, 
		modelFile = modelFile, 
		predictionsFilePath = predictionsFilePath,
		...)
    
	testTime = microbenchmark(s <- system3(testBinaryPath, args, verbose = verbose), times = 1L)$time / 1e9
    
	if (verbose == TRUE) 
		messagef("Testing took %f seconds.", testTime);
    
	results[["testTime"]] = testTime
	results[["testError"]] = extractTrainingInfo(SVMObject, output = s$output)
		
	# as testing was done, we want to read out the predictions
	predictions = readPredictions (SVMObject, predictionsFilePath = predictionsFilePath, verbose = verbose)
	results[["predictions"]] = predictions
	  	
	return (results)   
}


