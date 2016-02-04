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
 


#' Train an SVM.
#'
#' This is the main routine that trains an SVM, e.g. it will make sure that all
#' data is on the disk, create the training arguments and then call the corresponding binary.
#'
#' @param	method		name of the SVM method/solver
#' @param	trainDataFile		Filename to read training data from. This cannot be used at the same time as trainDataX/trainDataY.
#' @param	trainDataX		Matrix comprising the data. This cannot be used at the same time as trainDataFile.
#' @param	trainDataY		Vector comprising the labels for the data. This cannot be used at the same time as trainDataFile.
#' @param	trainBinaryPath		Full path to the training binary to call.
#' @param	extraParameter		Extra parameters for solver, passed to the createTrainingArguments function of the wrapper.
#' @param	modelFile		Path to the model file to create. If none specified, a temporary file will be created and read into memory.
#' @param	verbose		Be verbose?
#' @param	subsamplingRate		Rate to subsample the data. 
#' @param	subsamplingMethod		Method to subsample the data with.
#' @param	timeOut		Value to time out at, so that e.g. solvers that do stall/do an endless loop can be stopped nonetheless.
#' @param	readModelFile		if TRUE, will read the model back (model is saved in the created object)
#' @param	...		Further parameters that will be passed to the wrapper.
#'
#' @note		trainDataFile, trainDataX/trainDataY are exclusive parameters, i.e. you cannot specify both.
#' @note		timeOut might not work on all platforms yet.
#'
#' @return		An SVM Object.
#'
#' @export

trainSVM = function(
	method = NULL,
	trainDataFile = NULL,
	trainDataX = NULL, 
	trainDataY = NULL, 
	trainBinaryPath = NULL,
	extraParameter = "",
	modelFile = NULL,
	subsamplingRate = NULL,
	subsamplingMethod = "cutoff",
	verbose = FALSE,
	timeOut = -1,
	readModelFile = FALSE,
	...) {
	
	#expand possible tilde characters in the path and get rid of backslashes
	if(is.null(trainDataFile) == FALSE && grepl("~", trainDataFile) == TRUE){
		checkmate::assertString (trainDataFile)
		trainDataFile = expandTilde(path = trainDataFile, verbose = verbose)
	}
	
	
	checkmate::assertString (method)
		
	
	# get the correct object
	SVMObject = getSVMObject (method)

	# check for the object
	if (is.null (SVMObject) == TRUE) {
		stop ("Could not find wrapper for given model. Did you include the wrapper of the method?")
	}
	
	# ask object for its path
	trainBinaryPath = SVMObject$trainBinaryPath
	
	if (is.null (trainBinaryPath) == TRUE) {
		stop ("Sorry, no train binary was given. Cannot train an SVM without an executable.\n")
	}
	
	trainBinary = basename(trainBinaryPath)
	
	# general modifications
	if (verbose == TRUE) {
		cat("    Path of binary for training is ", trainBinaryPath)
		cat("    Binary for training is ", trainBinary)
	}

	
	# TODO: sanity checks for parameter
	if (is.null (method) == TRUE) {
		stop("No method name is given, this should never happen.")
	}
				
 	
	# did the user specify a modelfile?
	if (is.null(modelFile) == TRUE) {
		# if no, we will write the model from memory in a temp file
		modelFile = tempfile()
	}
		

	# take care of data. if traindataFile or traindataX is given,
	# the corresponding filename must be empty.
	# finally, everything is dumped to disk.
	if ( (is.null(trainDataX) == FALSE) && (is.null(trainDataFile) == FALSE)) {
		stop("Given a data frame as training data and specified a training file name. Confused. Stopping.")
	}		
	if ( (is.null(trainDataX) == TRUE) && (is.null(trainDataFile) == TRUE)) {
		stop("Neither specified training data path nor given training data. Stopping.")
	}
	

	
	if (is.null (trainDataFile) == FALSE)
		checkmate::assertString (trainDataFile)
		
	if (is.null (trainDataX) == FALSE) {
		checkmate::assertMatrix (trainDataX)
		checkmate::assertMatrix (trainDataY)
	}	

		
	# we got 
	if (is.null(trainDataX) == FALSE) {
		trainDataFile = tempfile()
		if (verbose == TRUE)
			cat("    Writing given data as ", trainDataFile)
		#e1071::write.matrix.csr(trainDataX, trainDataFile, trainDataY)
		writeSparseData (filename = trainDataFile, X = trainDataX, Y = trainDataY)
	} 

	if (verbose == TRUE) 
		cat("    Train Data is now in ", trainDataFile)

	
	# at this point we need to have a train data file on disk
	checkmate::assertString (trainDataFile)
	checkmate::assertFile (trainDataFile)
	
	results = list()

	# subsample the file
	if (is.null(subsamplingRate) == FALSE) {
		checkmate::assertString (subsamplingMethod)
		checkmate::assertNumeric (subsamplingRate)

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
	
	
	#Unsure if this works on windows systems, trainTime variable would not be present and other stuff.. thus FIXME probably
# 	if(.Platform$OS.type == "unix") {
	
		if (timeOut != -1) {
			timeOutArgs = c(sprintf("%d", timeOut), trainBinaryPath, args)
			timeOutPath = "/usr/bin/timeout"
			
			verbose = TRUE
			if (verbose == TRUE) 
				cat("  Applying a hard timeout of ", timeOut, "%f seconds\n")
			trainTime = microbenchmark::microbenchmark(s <- system3(timeOutPath, timeOutArgs, verbose = verbose), times = 1L)$time / 1e9
		}
		 else {
			trainTime = microbenchmark::microbenchmark(s <- system3(trainBinaryPath, args, verbose = verbose), times = 1L)$time / 1e9
		}
# 	}
	
	if (verbose == TRUE) 
		cat("Training took ", trainTime, " seconds\n")
		
	results[["trainTime"]] = trainTime
	
	
	if (readModelFile == TRUE) {
		if (verbose == TRUE) 
			cat( "    Will read model back from ", modelFile, "\n")

		model = readModel (SVMObject, modelFile = modelFile, verbose = verbose)
		results[["model"]] = model
	}

	return (results)   
}

