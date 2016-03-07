# dummy wrapper


createSVMWrapper.threeSVM = function() {
  createSVMWrapperInternal(
    name = "threeSVM",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericLearnerParam(id = "budget",  default = 128, lower = 1)
    ),
    properties = c("twoclass", "multiclass"),
    note = ""
  )
}

createTrainingArguments.threeSVM = function (x, ...) {
	cat ("Creating Training Arguments for threeSVM was called.\n")
	args = c("threeSVM Training Arguments")
	return (args)
}

	
createTestArguments.threeSVM = function (x, ...) {
	cat ("Creating Test Arguments for threeSVM was called.\n")
	args = c("threeSVM Test Arguments")
	return (args)
}


extractTrainingInfo.threeSVM = function (x,  output) {
	pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
	error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
	return (error)
}
	
	
extractTestInfo.threeSVM = function (
	x,
	output) {
		pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
}
	

readModel.threeSVM = function (x,
	modelFile = "./model",
	verbose = FALSE) {
		return (readthreeSVMModel (modelFile = modelFile, verbose = verbose) )
}


writeModel.threeSVM = function (x,
	model = NA,
	modelFile = "./model",
	verbose = FALSE) {
		return (writethreeSVMModel (model = model, modelFile = modelFile, verbose = verbose) )
}


detectModel.threeSVM = function (x, modelFile = NULL, verbose = FALSE) {
	checkmate::checkFlag (verbose)
	if (is.null (modelFile) == TRUE) 
		return (FALSE)
	
	# read first lines and detect magic marker
	if (file.exists (modelFile) == FALSE) 
		return (FALSE)
		
	line = readLines(modelFile, n = 12)
	if (sum(grepl("total_sv", line)) > 0) {
		return (TRUE)
	}
	
	return (FALSE)
}


readPredictions.threeSVM = function (x, predictionsFile = "", verbose = FALSE) {
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


findSoftware.threeSVM = function (x, searchPath = "./", execute = FALSE, verbose = FALSE) {
	
	# we only test if the train binary exists or not. if it does, we add it to the object
	trainBinaryPath = file.path (searchPath, "threeSVMtrain")
	if (file.exists (trainBinaryPath) == TRUE) {
		x$trainBinaryPath = trainBinaryPath 
		# wont execute
	}
	
	# try predict binary now
	testBinaryPath = file.path (searchPath, "threeSVMtest")
	if (file.exists (trainBinaryPath) == TRUE) {
		x$testBinaryPath = testBinaryPath
	}

	return(x)
}
	

print.threeSVM = function(x) {
	cat("--- Object: ", x$method, "\n")
	cat("       Training Binary at ", x$trainBinaryPath, "\n")
	cat("       Test Binary at ", x$testBinaryPath, "\n")
}
	