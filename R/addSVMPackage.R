#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		addSVMPackage.R
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


#' add package to the bridge
#'
#' @param 	method		name of solver
#' @param	filePath		path to wrapper
#' @param	softwarePath		path where to find the solver.
#' @param 	trainBinaryPath		if not NULL this will override any search option
#' @param 	testBinaryPath		if not NULL this will override any search option
#' @param	wrapperPath		if not NULL this will override any search option
#' @param	verbose		be verbose?
#'
#' @note	 	first the given train and testBinaryPaths will be directly checked.
#' if the binary does not exist there, the softwarePath will be added and rechecked
#' and only if this does not work, the software will be searched via softwarePath.
#' so one can override the search by specifiying train-/testBinaryPath.
#'
#' @export	addSVMPackage
addSVMPackage <- function (method = NA, filePath = NA, softwarePath = NA, 
	trainBinaryPath = NA, testBinaryPath = NA, wrapperPath = NA, verbose = FALSE)
{
	# we always need a method
	if (is.na(method) == TRUE) {
		BBmisc::stopf("Please provide the name of the SVM package to create")
	}

	# load definition of SVM object
	if (is.na(filePath) == FALSE) {
		source (filePath, local = FALSE)
	}

	
	# TODO: test if the object specified by the name exists
	
	
	# create an object now
	if (verbose == TRUE) {
		BBmisc::messagef ("Creating SVM Wrapper for method %s: ", method)
	}
	SVMObject = createSVMWrapper( method = method)
	SVMBridgeEnv$packages[[method]] = SVMObject
	if (verbose == TRUE) {
		BBmisc::messagef ("Registered Objects")
		for (i in seq(1, length(SVMBridgeEnv$packages))) {
			s = SVMBridgeEnv$packages[i]
			print (s)
		}
	}

	# check for test and train binaries
	if ( (is.null (trainBinaryPath) == FALSE) && (is.na(trainBinaryPath) == FALSE)) {
		if (file.exists (trainBinaryPath) == FALSE) {
			# append software path to it
			if (is.na(softwarePath) == FALSE) {
				if (verbose == TRUE)
					cat ("Appending softwarePath to train binary")
					
				trainBinaryPath = file.path (softwarePath, trainBinaryPath)
				if (file.exists (trainBinaryPath) == FALSE) {
					# ok nothing helped. do a search
					if (verbose == TRUE) {
						cat ("  Specified software path, so searching for software package: ")
					}
					findSVMSoftware (method = method, searchPath = softwarePath, verbose = verbose) 
				}
			} else {
				if (verbose == TRUE)
					cat ("Did not find training binary.\n")
			}
		} else {
			if (verbose == TRUE)
				cat ("Found train binary", trainBinaryPath, "\n")
		}
	}

	
	# check for test and test binaries
	if ( (is.null (testBinaryPath) == FALSE) && (is.na(testBinaryPath) == FALSE)) {
		if (file.exists (testBinaryPath) == FALSE) {
			# append software path to it
			if (is.na(softwarePath) == FALSE) {
				if (verbose == TRUE)
					cat ("Appending softwarePath to test binary")
					
				testBinaryPath = file.path (softwarePath, testBinaryPath)
				if (file.exists (testBinaryPath) == FALSE) {
					# ok nothing helped. do a search
					if (verbose == TRUE) {
						cat ("  Specified software path, so searching for software package: ")
					}
					findSVMSoftware (method = method, searchPath = softwarePath, verbose = verbose) 
				}
			} else {
				if (verbose == TRUE)
					cat ("Did not find testing binary.\n")
			}
		} else {
			if (verbose == TRUE)
				cat ("Found test binary", testBinaryPath, "\n")
		}
	}
	
	
	# check for wrapper
	if ((is.null(wrapperPath) == FALSE) && (is.na(wrapperPath) == FALSE) ) {
		if (file.exists(wrapperPath) == FALSE) {
			if (verbose == TRUE)
				cat ("Did not find wrapper, appending softwarePath.\n")
					
			wrapperPath = file.path (softwarePath, wrapperPath)
			if (file.exists (wrapperPath) == FALSE) {
				# ok nothing helped. do a search
				if (verbose == TRUE) 
					cat ("Specified software path, so searching for wrapper: ")
				findSVMWrapper (method = method, searchPath = softwarePath, verbose = verbose) 
			} else {
				if (verbose == TRUE)
					cat ("Found wrapper at", wrapperPath, "\n")
			}
		} else {
			if (verbose == TRUE)
				cat ("Found wrapper at", wrapperPath, "\n")
		}
	}
	

	if ( (is.null(wrapperPath) == FALSE) & (is.na(wrapperPath) == FALSE)) {
		if (verbose == TRUE)
			cat ("Sourcing wrapper..\n")
		source (wrapperPath, local = FALSE)
	}
	
	# TODO: correct?
	# what ever the outcome, we put the binary into our structure
	SVMBridgeEnv$packages[[method]]$trainBinaryPath = trainBinaryPath
	SVMBridgeEnv$packages[[method]]$testBinaryPath = testBinaryPath
}



#' dump all known package infos
#'
#' @export	outputAllSVMSoftwarePackages
outputAllSVMSoftwarePackages <- function () {
	BBmisc::messagef("Currently known solver:")
	for (package in SVMBridgeEnv$packages) {
		print (package)
	}
}



#' dump specific package info
#'
#' @param	method		name of package/method
#' @result	SVM object for the given method
#'
#' @export	getSVMInstance
getSVMInstance <- function ( method = method) {
	return (SVMBridgeEnv$packages[[method]])
}
