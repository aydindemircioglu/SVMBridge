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
 

#' Do prediction with a trained SVM on given data.
#'
#' This is the main routine for predictions. If needed, it will add dummy labels to the data given and then call testSVM.
#'
#' @param	method		Name of the method/solver to use for testing. If NULL, a detectModelTypeFromFile will be called on the given model (or modelfile).
#' @param	testDataFile		File to read training data from. Cannot be specified at the same time as testDataX and testDataY. See notes.
#' @param	testDataX		Matrix to read training data from. Cannot be specified at the same time as testDataFile. 
#' @param	testDataY		Matrix to read training label from. Cannot be specified at the same time as testDataFile. Optional, see notes.
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
#' @note		Prediction can work without labels. If labels are not provided, dummy labels will be created, as testSVM expects labels
#' Keep in mind that as the whole workflow is by command line, the unlabeled data on disk has to be first read, then labeled
#' with a dummy label and then written back to disk. This will take some I/O time. 
#'
#' @return	An SVM Test Object, but with testError removed.
#'
#' @export

predictSVM = function(
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
		cat("Predicting with SVM", method, ".\n")
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
	}
	
	if (is.null (predictionsFile) == FALSE) {
		checkmate::checkString (predictionsFile)
	}
	
	checkmate::assertFlag (readPredictions)
	
	checkmate::assertString (extraParameter)
	checkmate::assertFlag (verbose)

	
	# we need to check: if we do not get labels, we need to create dummy labels.
	if (is.null (testDataFile) == FALSE) {
		# first case: the data is on disk. check if we have labels
		
		# to avoid problems, we only read the first line and check that
		hasLabels = TRUE
		con  <- file (testDataFile, open = "r")
		if (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
			oneLine = stringr::str_split_fixed(oneLine, pattern = '#', n = 2)[1]
			parts = strsplit (oneLine, " ")
			ptab = min (regexpr(" ", oneLine), regexpr("\t", oneLine))
			pcol = regexpr(":", oneLine)
			if (pcol < ptab) {
				hasLabels = FALSE
			}
		}

		
		if (hasLabels == FALSE) {
			# no labels, so we read the whole file, add labels and write to a temp file
			if (verbose == TRUE) {
				cat ("    No labels was found in file to predict. Adding dummy labels.\n")
			}
			
			# read the file, add dummy labels (remember: labels are always integers) and write back to tempname
			r = readSparseData (testDataFile)
			r$Y = matrix(0, nrow(r$X), 1)
			testDataFile = tempfile()
			writeSparseData (filename = testDataFile, X = r$X, Y = r$Y)
		}
	} else {
		if (is.null (testDataY) == TRUE) {
			# labels are always integer
			testDataY = matrix(0, nrow(testDataX))
		}
	}


	retObj = testSVM (method = method, 
		testDataFile = testDataFile,
		testDataX = testDataX, 
		testDataY = testDataY, 
		model = model,
		modelFile = modelFile,
		readPredictions = readPredictions ,
		predictionsFile = predictionsFile,
		extraParameter = extraParameter,
		verbose = verbose, ...)

	# remove test error as it is nothing meaningful.
	retObj$testError = NULL 
		
	return (retObj)
}

