#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		testSVM.R
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
#' @param	testDataFile		file to read training data from 
#' @param	testDataX		matrix to read training data from 
#' @param	testDatay		matrix to read training label from 
#'
#' @param	model		list containing a trained SVM model
#' @param	modelFile       path to a model file
#'
#' @param	readPredictions		if TRUE, the prediction will be read back 
#' @param	predictionsFile		the prediction will be written to this file or to a tempfile
#'
#' @param	extraParameter		extra parameters for solver
#' @param	verbose		be verbose?
#'
#' @note		exclusive parameters, i.e. you cannot specify both:
#'
#' @return		SVM Object
#' @export	testSVM
# #examples
# 	testSVM(method = 'LIBSVM', testDataFile = './data/australian.test') 
#'
#
# NOTE: make sure the modelFile is a character string and not a factor! else you might strange modelfile arguments.
# If no

testSVM = function(
	method = NULL,

	# data
	testDataFile = NULL,
	testDataX = NULL, 
	testDatay = NULL, 

	# model 
	model = NULL,
	modelFile = NULL,

	# prediction
	readPredictions = FALSE,
	predictionsFile = NULL,
	
	# rest
	extraParameter = "",
	verbose = FALSE,
	...) {
	
	if (verbose == TRUE) {
		BBmisc::messagef("--- Testing...")
	}
	
		# TODO: sanity checks for parameter
	if ( (is.null(model) == TRUE) && (is.null(modelFile) == TRUE))
			BBmisc::stopf("Neither given a model nor given a path to the model. Stopping.")	
	
	if ( (is.null(model) == FALSE) && (is.null(modelFile) == FALSE))
			BBmisc::stopf("Given a model in memory and specified a model file name. Confused. Stopping.")
			
	if (is.null (method) == TRUE) {
		# test model
		if  (is.null(model) == FALSE)
			method = model$modelType
		# else get method from model file
		else
			method = detectModelTypeFromFile (modelFile)
	}

	# get the correct object
	SVMObject = SVMBridgeEnv$packages[[method]]
	
	# ask object for its path
	testBinaryPath = SVMObject$testBinaryPath
	
	if (is.null(SVMObject)) {
		BBmisc::stopf ("Cannot find the specified SVM object. Did you include the wrapper of the method %s?", method)
	}
	
	testBinary = basename(testBinaryPath)
	
	# general modifications
	if (verbose == TRUE) {
		BBmisc::messagef("  Path of binary for testing is %s", testBinaryPath)
		BBmisc::messagef("  Binary for testing is %s", testBinary)
	}

	# take care of data. if testdata or traindata is given,
	# the corresponding filename must be empty.
	# finally, everything is dumped to disk.
  
	if ( (is.null(testDataX) == FALSE) && (is.null(testDataFile) == FALSE))
		BBmisc::stopf("Given a data frame as testing data and specified a testing file name. Confused. Stopping.")
			
	if ( (is.null(testDataX) == TRUE) && (is.null(testDataFile) == TRUE))
		BBmisc::stopf("Neither specified testing data path nor given testing data. Stopping.")

	# TODO: check for X AND y.
		
	# we got 
	
	#expand possible tilde characters in the path and get rid of backslashes
	if(is.null(testDataFile) == FALSE && grepl("~", testDataFile) == TRUE){
		testDataFile = expandTilde(path = testDataFile, verbose = verbose)
	}
	
	if (is.null(testDataX) == FALSE) {
		testDataFile = tempfile()
		if (verbose == TRUE)
			BBmisc::messagef("  Writing given test data as %s", testDataFile)
		e1071::write.matrix.csr(testDataX, testDataFile, testDatay)
	} 

	if (verbose == TRUE) 
		BBmisc::messagef("  Test Data is now in %s", testDataFile)

	results = list()

	# if the user specified a model in memory, we first need to write that
	if (is.null(model) == FALSE) {
		modelFile = tempfile()
		if (verbose == TRUE)
			BBmisc::messagef("  Writing model in memory to disk as %s", modelFile)
		
		writeModel (SVMObject, model = model, modelFile = modelFile, verbose = verbose)
	} else {
		if (verbose == TRUE)
			BBmisc::messagef("  Reading model from file %s", modelFile)
	}

	# check if we need to create a temporary file
	if (is.null(predictionsFile) == TRUE) {
		predictionsFile = tempfile()
	}
	
	# retrieve test parameters
	args = createTestArguments (SVMObject, 
		testDataFile = testDataFile, 
		modelFile = modelFile, 
		predictionsFile = predictionsFile,
		...)
    
    if (verbose == TRUE) {
		BBmisc::messagef("  Generated test arguments %s", args)
	}
      
	# probably FIXME because of windows systems
	testTime = microbenchmark::microbenchmark(s <- system3(testBinaryPath, args, verbose = verbose), times = 1L)$time / 1e9
    
	if (verbose == TRUE) 
		BBmisc::messagef("Testing took %f seconds.", testTime);
    
	results[["testTime"]] = testTime
	results[["testError"]] = extractTestInfo(SVMObject, output = s$output)
		
		
	# if user did not specify prediction file, we will read them back
	if (readPredictions == TRUE) {
		# we turn off verbose here, in order not to clutter screen
		predictions = readPredictions (SVMObject, predictionsFile = predictionsFile, verbose = FALSE)
		results[["predictions"]] = predictions
	}
	
	return (results)   
}


