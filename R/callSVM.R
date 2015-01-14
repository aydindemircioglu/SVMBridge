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
 
loadThings <- function ()
{
	library(microbenchmark)

	source ("./system3.R")
}
suppressMessages(loadThings())


# global object :/
# our array of objects
SVMPackages = c()


initPackage <- function () {
	# load all wrapper 
	# TODO: activate
#	addSVMPackage (filePath = "./LIBSVM_wrapper.R")
#	addSVMPackage (filePath = "./LIBCVM_wrapper.R")
	return (NULL)
}



# every wrapper has its own object that is derived from the universalWrapper.
# we create all these at initialization time of the package.
# then, when e.g. paths are updated, we have a nice place to store these.


addSVMPackage <- function (filePath = NA, softwarePath = NA, verbose = FALSE)
{
	if (is.na(filePath) == TRUE) {
		stopf("Please provide a file path for SVM package to add.")
	}
	
	source (filePath, local = FALSE)
	
	# now we make a crucial assumption here,
	# to make everything 'magical',
	# assume that the filename already denotes the
	# method name and (all the callback names if needed)
	# as well as the caller name (evalXXX).
	baseName = basename (filePath)
	methodName = strsplit(baseName, "_")[[1]][1]
	SVMPackages <<- c(SVMPackages, methodName)
	
	# add software path
	if (is.na(softwarePath) == FALSE) {
		findSVMSoftware (method = methodName, searchPath = softwarePath, verbose = verbose) 
	}
}



callSVM <- function (
	method = '', 
	...) {
	
	returnValue = NULL

	# TODO: try catch
	
	# depending on method, take one of the wrappers
	callerName = paste ('eval', method, sep = "")
	returnValue= do.call(callerName, list(method = method, ...))

    if (is.null(returnValue)) {
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



findAllSVMSoftware <- function (searchPath = NA, verbose = FALSE) {
	for (package in SVMPackages) {
		if (verbose == TRUE) {
			messagef("  Searching for software for SVM package %s:", package)
		}
		findSVMSoftware (method = package, searchPath = searchPath, verbose = verbose)
	}
}



findSVMSoftware <- function (method = NA, searchPath = NA, verbose = FALSE) {
	if (is.na(searchPath)) {
		stopf("No search path is given!")
	}
	
	if (is.na(method)) {
		stopf ("No method name is given")
	}
	
	# TODO: to get better tests, maybe we need an option like "TEST = true", which will
	# take a demo-data-file and compute the model. so actuallly its like a unittest, but
	# it is executed during use, to make sure everything is as it should be.

	trainBinaryPattern = paste ("^", do.call(paste(method, "TrainBinary", sep = ""), args = list()), "$", sep = '')
	trainBinaryOutputPattern = do.call(paste(method, "TrainBinaryOutputPattern", sep = ""), args = list())
	binaryPath = findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, method, verbose = verbose)
	if (verbose == TRUE) {
		messagef("--> Found train binary at %s", binaryPath) 
	}
	trainOptionName = paste('SVMBridge', method, "trainBinary", sep = ".")
	tmpList = list()
	tmpList[[trainOptionName]] = binaryPath
	do.call(options, tmpList)
	
	testBinaryPattern = paste ("^", do.call(paste(method, "TestBinary", sep = ""), args = list()), "$", sep = '')
	testBinaryOutputPattern = do.call(paste(method, "TestBinaryOutputPattern", sep = ""), args = list())
	binaryPath = findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, method, verbose = verbose)
	if (verbose == TRUE) {
		messagef("--> Found test binary at %s", binaryPath) 
	}
	testOptionName = paste('SVMBridge', method, "testBinary", sep = ".")
	tmpList = list()
	tmpList[[testOptionName]] = binaryPath
	do.call(options, tmpList)
}



outputAllSVMSoftwarePackages <- function () {
	messagef("Currently known solver:")
	for (package in SVMPackages) {
		messagef("  %s:", package)
		trainOptionName = paste('SVMBridge', package, "trainBinary", sep = ".")
		messagef("    TrainBinary: %s", getOption(trainOptionName))
		testOptionName = paste('SVMBridge', package, "testBinary", sep = ".")
		messagef("    TestBinary: %s", getOption(testOptionName))
	}
}

