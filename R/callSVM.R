loadThings <- function ()
{
  library(BBmisc)
  library(stringr)
  library(R.utils)
  library(microbenchmark)
  library(e1071)
  
  source ("./system3.R")
  source ("./subsampleData.R")
  source ("./models/readModel.R")
}
suppressMessages(loadThings())




source ("./lsdir.R")


# every wrapper has its own object that is derived from the universalWrapper.
# we create all these at initialization time of the package.
# then, when e.g. paths are updated, we have a nice place to store these.

# load all wrapper
source ("./wrapper/LIBSVM_wrapper.R")
source ("./wrapper/LIBCVM_wrapper.R")


initPackage = function () {
	# our array of objects
	SVMObjects = c(evalLIBSVM)

	# 	for (SVMObject in SVMObjects) {
# 		print(SVMObject)
# 	}
	
# 	stopf("A")
}



callSVM <- function (
	method = '', 
	...) {
	# depending on method, take one of the wrappers

	returnValue = NA
    if (method == "libBVM") {
      returnValue = evalLibBVM(examplePath, examplePath, cost = cost, gamma = gamma, epsilon = 0.001, 
                       subsamplingRate = subsamplingRate,
                      modelFile = modelFile)
    }
    
    if (method == "libCVM") {
      returnValue = evalLibCVM(examplePath, examplePath, cost = cost, gamma = gamma, epsilon = 0.001, 
                       subsamplingRate = subsamplingRate,
                      modelFile = modelFile)
    }

    if (method == "LIBSVM") {
		returnValue= evalLIBSVM(method = method, ...)
    }
    
    if (is.na(returnValue)) {
		stopf("Either there was a severe error executing the SVM wrapper, or you misspelled the method name.")
	}
    return(returnValue)
}


#
# @param 	searchPath	 	search the given path for the SVM binaries recursively.
# @param	verbose			print messages while searching?
#
# @note		To make sure that the binary is correct, it will be executed!
# 					Furthermore, many SVM packages derive from libSVM. as such, they
#					often do not change the prediction binary. We will try to sort these out,
#					but it might be hopeless. With luck, the found binary will be left untouched,
#					and thus work, if not, you must set the path by hand.
# @note		If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.


# internal
findBinary <- function (searchPath, pattern, outputPattern, solver, verbose = FALSE) {
	if (verbose == TRUE) { messagef("  Checking for pattern %s", pattern) }
	files <- list.files (searchPath, pattern = pattern, recursive = TRUE)
    foundBinary = ''
    for (binary in files) {
		binaryPath = file.path(searchPath, binary)
		if (verbose == TRUE) { messagef("    -Found binary at %s", binaryPath) }
		stdout = system3(binaryPath, args = c(), verbose = FALSE)
		if (length(grep(outputPattern, stdout$output)) != 0) {
			if (verbose == TRUE) { messagef("     This is %s", solver) }
			foundBinary = binaryPath
		}
    } 
    return (foundBinary)
}


# i do not really like R.
	SVMBridge.LIBSVM.trainBinary <<- "." 
	SVMBridge.LIBSVM.testBinary <<- "." 
	SVMBridge.BVM.trainBinary <- "."
	SVMBridge.BVM.testBinary <- "."



findSVMSoftware <- function (searchPath = NA, verbose = FALSE) {
	if (is.na(searchPath)) {
		stopf("No search path is given!")
	}
	
	# TODO: move these into the   lib.._wrapper.R
	
	# TODO: to get better tests, maybe we need an option like "TEST = true", which will
	# take a demo-data-file and compute the model. so actuallly its like a unittest, but
	# it is executed during use, to make sure everything is as it should be.
	
	# iterate over all directories and search for the corresponding binaries

	solver = "SVM"
	pattern = '^svm-train$'
	outputPattern = 'Usage: svm-train .options. training_set_file .model_file.'
	binaryPath = findBinary (searchPath, pattern, outputPattern, solver, verbose = verbose)
	#assign(SVMBridge.LIBSVM.trainBinary, binaryPath, envir = .GlobalEnv)
	options("SVMBridge.LIBSVM.trainBinary" = binaryPath)

	solver = "SVM"
	pattern = '^svm-predict$'
	outputPattern = 'for one-class SVM only 0 is supported'
	binaryPath = findBinary (searchPath, pattern, outputPattern, solver, verbose = verbose)
	options("SVMBridge.LIBSVM.testBinary" = binaryPath)

	solver = "BVM"
	pattern = '^svm-train$'
	outputPattern = '6 -- CVM \\(sqr. hinge-loss'
	binaryPath = findBinary (searchPath, pattern, outputPattern, solver, verbose = verbose)
	options("SVMBridge.BVM.trainBinary" = binaryPath)

	solver = "BVM"
	pattern = '^svm-predict$'
	outputPattern = 'bvm-predict'
	binaryPath = findBinary (searchPath, pattern, outputPattern, solver, verbose = verbose)
	options("SVMBridge.BVM.testBinary" = binaryPath)
}



outputAllSVMSoftwarePaths <- function () {
	# TODO: loop over solver

	messagef("Currently known solver:")
	
	messagef("	LIBSVM Train: %s", getOption("SVMBridge.LIBSVM.trainBinary" ))
	messagef("	LIBSVM Test: %s", getOption("SVMBridge.LIBSVM.testBinary"))
	messagef("	BVM Train: %s", getOption("SVMBridge.BVM.trainBinary"))
	messagef("	BVM Test: %s", getOption("SVMBridge.BVM.testBinary"))
}

