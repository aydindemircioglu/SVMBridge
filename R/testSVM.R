#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
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
 

#' Test a trained SVM on given data.
#'
#' This is the main routine for testing an SVM (or doing predictions).
#'
#' @param	method		Name of the method/solver to use for testing. If NULL, a detectModelTypeFromFile will be called on the given model (or modelfile).
#' @param	testDataFile		File to read training data from. Cannot be specified at the same time as testDataX and testDataY.
#' @param	testDataX		Matrix to read training data from. Cannot be specified at the same time as testDataFile. 
#' @param	testDataY		Matrix to read training label from. Cannot be specified at the same time as testDataFile.
#' @param	model		A trained SVM model. Cannot be specified at the same time as modelFile.
#' @param	modelFile		Path of the model file. Cannot be specified at the same time as model.
#' @param	readPredictions		Shall the predictions be read back into memory and added to the model?
#' @param	predictionsFile		File to which the predictions will be written.
#' @param	extraParameter			Extra parameters for solver in form of a string.
#' @param	verbose			Be verbose?
#' @param	...		Further parameters that will be passed to the createTestArguments function of the wrapper.
#'
#' @note		testDataFile and testDataX,testDataY are exclusive parameters, i.e. you cannot specify both.
#' @note		Make sure the modelFile is a character string and not a factor! Else you might obtain strange modelfile arguments.
#'
#' @return	An SVM Test Object.
#'
#' @export

testSVM = function(
	method = NULL,
	testDataFile = NULL,
	testDataX = NULL, 
	testDataY = NULL, 
	model = NULL,
	modelFile = NULL,
	readPredictions = FALSE,
	predictionsFile = NULL,
	extraParameter = "",
	verbose = FALSE,
	...) {
	
	if (verbose == TRUE) {
		cat("Testing SVM.")
	}
	
	# sanity checks for parameters
	if ( (is.null(model) == TRUE) && (is.null(modelFile) == TRUE))
			stop("Neither given a model nor given a path to the model. Stopping.")	
	
	if ( (is.null(model) == FALSE) && (is.null(modelFile) == FALSE))
			stop("Given a model in memory and specified a model file name. Confused. Stopping.")

	if (is.null (method) == TRUE) {
		if  (is.null(model) == FALSE)
			method = model$modelType
		# else get method from model file
		else
			method = detectModelTypeFromFile (modelFile)
	}
	
	checkmate::assertString (method)
	if (is.null (testDataFile) == FALSE)
		checkmate::assertString (testDataFile)
		
	if (is.null (testDataX) == FALSE) {
		checkmate::assertMatrix (testDataX)
		checkmate::assertMatrix (testDataY)
	}
	
	if (is.null (predictionsFile) == FALSE) {
		checkmate::checkString (predictionsFile)
	}
	
	checkmate::assertFlag (readPredictions)
	
	checkmate::assertString (extraParameter)
	checkmate::assertFlag (verbose)

	# get the correct object
	SVMObject = getSVMObject (method)

	# check for the object
	if (is.null (SVMObject) == TRUE) {
		stop ("Could not find wrapper for given model. Did you include the wrapper of the method?")
	}
	
	# ask object for its path
	testBinaryPath = SVMObject$testBinaryPath
	checkmate::assertString (testBinaryPath)

	# get basename
	testBinary = basename(testBinaryPath)
	
	# general modifications
	if (verbose == TRUE) {
		cat("  Path of binary for testing is ", testBinaryPath)
		cat("  Binary for testing is ", testBinary)
	}

	# take care of data. if testdata or traindata is given,
	if ( (is.null(testDataX) == FALSE) && (is.null(testDataFile) == FALSE))
		stop("Given a data frame as testing data and specified a testing file name. Confused. Stopping.")
			
	if ( (is.null(testDataX) == TRUE) && (is.null(testDataFile) == TRUE))
		stop("Neither specified testing data path nor given testing data. Stopping.")


	#expand possible tilde characters in the path and get rid of backslashes
	if(is.null(testDataFile) == FALSE && grepl("~", testDataFile) == TRUE){
		checkmate::assertString (testDataFile)
		testDataFile = expandTilde(path = testDataFile, verbose = verbose)
	}
	
	if (is.null(testDataX) == FALSE) {
		testDataFile = tempfile()
		if (verbose == TRUE)
			cat("  Writing given test data as ", testDataFile)
		writeSparseData (filename = testDataFile, X = testDataX, Y = testDataY)
		#e1071::write.matrix.csr(testDataX, testDataFile, testDataY)
	} 

	if (verbose == TRUE) 
		cat("  Test Data is now in ", testDataFile)

	results = list()

	# if the user specified a model in memory, we first need to write it to disk
	if (is.null(model) == FALSE) {
		modelFile = tempfile()
		if (verbose == TRUE)
			cat("  Writing model in memory to disk as ", modelFile)
		
		writeModel (SVMObject, model = model, modelFile = modelFile, verbose = verbose)
	} else {
		if (verbose == TRUE)
			cat("  Reading model from file ", modelFile)
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
		cat("  Generated test arguments ", args)
	}
      
	# probably FIXME because of windows systems
	testTime = microbenchmark::microbenchmark(s <- system3(testBinaryPath, args, verbose = verbose), times = 1L)$time / 1e9
    
	if (verbose == TRUE) 
		cat("Testing took ", testTime, " seconds.");
    
	results[["testTime"]] = testTime
	results[["testError"]] = extractTestInfo(SVMObject, output = s$output, verbose = verbose)
		
		
	# if user did not specify prediction file, we will read them back
	if (readPredictions == TRUE) {
		# we turn off verbose here, in order not to clutter screen. this is mainly a debug switch.
		predictions = readPredictions (SVMObject, predictionsFile = predictionsFile, verbose = FALSE) 
		results[["predictions"]] = predictions
	}
	
	return (results)   
}

