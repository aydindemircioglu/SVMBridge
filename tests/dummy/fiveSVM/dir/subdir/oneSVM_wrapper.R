# dummy wrapper

createTrainingArguments.oneSVM = function (x, ...) {
	cat ("Creating Training Arguments for oneSVM was called.\n")
	args = c("oneSVM Training Arguments")
	return (args)
}

	
createTestArguments.oneSVM = function (x, ...) {
	cat ("Creating Test Arguments for oneSVM was called.\n")
	args = c("oneSVM Test Arguments")
	return (args)
}


extractTrainingInfo.oneSVM = function (x,  output) {
	pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
	error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
	return (error)
}
	
	
extractTestInfo.oneSVM = function (
	x,
	output) {
		pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
		error = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
		return (error)
}
	

readModel.oneSVM = function (x,
	modelFile = "./model",
	verbose = FALSE) {
		return (readoneSVMModel (modelFile = modelFile, verbose = verbose) )
}


writeModel.oneSVM = function (x,
	model = NA,
	modelFile = "./model",
	verbose = FALSE) {
		return (writeoneSVMModel (model = model, modelFile = modelFile, verbose = verbose) )
}


detectModel.oneSVM = function (x, modelFile = NULL, verbose = FALSE) {
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


readPredictions.oneSVM = function (x, predictionsFile = "", verbose = FALSE) {
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


findSoftware.oneSVM = function (x, searchPath = "./", execute = FALSE, verbose = FALSE) {
	
	# we only test if the train binary exists or not. if it does, we add it to the object
	trainBinaryPath = file.path (searchPath, "oneSVM-learn")
	if (verbose == TRUE) {
		cat ("    Checking ", trainBinaryPath, "\n")
	}
	if (file.exists (trainBinaryPath) == TRUE) {
		x$trainBinaryPath = trainBinaryPath 
		# wont execute
	}
	
	# try predict binary now
	testBinaryPath = file.path (searchPath, "oneSVM-predict")
	if (verbose == TRUE) {
		cat ("    Checking ", testBinaryPath, "\n")
	}
	if (file.exists (trainBinaryPath) == TRUE) {
		x$testBinaryPath = testBinaryPath
	}

	return(x)
}
	

print.oneSVM = function(x) {
	cat("--- Object: %s", x$method, "\n")
	cat("       Training Binary at %s", x$trainBinaryPath, "\n")
	cat("       Test Binary at %s", x$testBinaryPath, "\n")
}
	