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


#' Make a new SVM package known to the bridge.
#'
#' This function will create an object corresponding to the SVM object.
#' By using the findSVMSoftware/findSVMWrapper functions., one can add 
#' search for the corresponding wrappe and the software.
#' To avoid lengthy searches,  this function can be used to directly 'shortcut'
#' the search, by specifiying the directories where the wrapper/software lies.
#' If this is successful, later findSVM.. calls are unneccessary.
#'
#' @param 	method		name of solver
#' @param	wrapperName		name of the wrapper (as filename). if none given, method_wrapper.R will be used.
#' @param	wrapperPath		file path to the wrapper. if none is given, findSVMWrapper needs to be called
#' @param	softwarePath		path where to find the solver (software), if none is given, findSVMSoftware has to be called
#' @param	verbose		be verbose?
#'
#' @note	 	first the given train and testBinaryPaths will be directly checked.
#' if the binary does not exist there, the softwarePath will be added and rechecked
#' and only if this does not work, the software will be searched via softwarePath.
#' so one can override the search by specifiying train-/testBinaryPath.
#' @note		If the wrapper searches for software, it will NOT execute it, existance is enough at this point.
#' 
#' @export	addSVMPackage
addSVMPackage = function (method = NA, 
	wrapperName = NA,
	wrapperPath = NA,
	softwarePath = NA, 
	verbose = FALSE)
{
	checkmate::assertString (method)

	# remove any old object and create a new one by overwriting
	if (verbose == TRUE) {
		cat ("Creating new SVM object for method ", method, "\n")
	}

	SVMObject = createSVMWrapper (method = method)
	
	# add wrapper name, if not provided.
	if (checkmate::testString (wrapperName) == FALSE) {
		SVMObject$wrapperName = paste0 (method, "_wrapper.R")
	}

	setSVMObject (method, SVMObject)

	
	# 1. check if we are given a wrapper path directly.
	#		if so, we load it. 
	#		it not, we need to check if we are given a softwarePath.
	#			if so, we search the softwarePath for the wrapper
	#			if not, we cannot load any wrapper, thats ok too.
	if (checkmate::testString (wrapperPath) == TRUE) {
		# could be a file directly, not just a path
		if ((file.exists (wrapperPath) == TRUE) & (dir.exists (wrapperPath) == FALSE)) {
			if (verbose == TRUE)
				cat ("Found wrapper at", wrapperPath, "\n")
			source (wrapperPath, local = FALSE)
			SVMObject$wrapperPath = wrapperPath
		} else {
			# if not, we try to find the default wrapper
			wrapperPath = file.path (wrapperPath, SVMObject$wrapperName)
			if ((file.exists (wrapperPath) == TRUE) & (dir.exists (wrapperPath) == FALSE)) {
				if (verbose == TRUE)
					cat ("Found wrapper at", wrapperPath, "\n")
				source (wrapperPath, local = FALSE)
				SVMObject$wrapperPath = wrapperPath
			} 
		}
	} else {
		# test if we can detect it easily
		if (checkmate::testString (softwarePath) == TRUE) {
			wrapperPath = file.path (softwarePath, SVMObject$wrapperName)
			if ((file.exists (wrapperPath) == TRUE) & (dir.exists (wrapperPath) == FALSE)) {
				if (verbose == TRUE)
					cat ("Found wrapper at", wrapperPath, "\n")
				source (wrapperPath, local = FALSE)
				SVMObject$wrapperPath = wrapperPath
			} else {
				# finally, we have no clue.
				if (verbose == TRUE) {
					cat ("Not able to find the wrapper. Sorry. \n")
				}
			}
		} else {
			# we are not given any softwarePath. 
			# so, what should we do anyway?
			if (verbose == TRUE) {
				cat ("Not able to find the wrapper. Sorry.\n")
			}
		}
	}


	# now, if a software path is given, then we should check it and try to find
	# the binaries. if that doesnt work, its not our problem.
	
	if (checkmate::testString (softwarePath) == TRUE) {
		# ask the wrapper if it can load the binaries from the given path
		SVMObject = findSoftware (SVMObject, searchPath = softwarePath, verbose = verbose)	
		
		# assume here that predict and test are in the same directory. if not, the user has to do magic by findSVM... or manually 
		if ((is.null(SVMObject$trainBinaryPath) == TRUE) | (is.null(SVMObject$testBinaryPath) == TRUE)) {
			SVMObject = findSoftware (SVMObject, searchPath = file.path (softwarePath, "bin"), verbose = verbose)	
		}
	} 

	SVMBridgeEnv$packages[[method]] = SVMObject
}
