#
# SVMBridge 
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



#' Find software  for an SVM package.
#'
#' Given a search path, it will try to find the corresponding software packages
#' for the given method.
#'
#' @param	method	   	Name of the SVM method
#' @param   searchPath   Search path for the SVM binaries of the given SVM method.
#' @param	verbose		Print messages while searching?
#' 
#' @note    Calls the findSoftware routine of the corresponding wrapper.
#'
#' @export

findSVMSoftware <- function (method = NA, searchPath = NA, verbose = FALSE) {

	checkmate::assertString (method)
	checkmate::assertString (searchPath)
	checkmate::assertFlag (verbose)

	if (verbose == TRUE) {
		cat("Finding software for ", method, "\n")
	}
	
	# replace tilde characters by expanding them
	searchPath = expandTilde(path = searchPath, verbose = verbose)
	
	# iterate over all search paths until we hit the target
	dirList = list.dirs(searchPath, recursive = TRUE)
    
    SVMObject = getSVMObject (method)
    if (is.null (SVMObject) == TRUE) {
		if (verbose == TRUE) {
			cat ("No SVM Object found. Did you create the wrapper?\n")
			return (FALSE)
		}
	}
	
    if (is.null(SVMObject$wrapperPath) == FALSE) {
		for (dir in dirList) {
			# call the find software method of the solver
			if (verbose == TRUE) {
				cat ("    Scanning directory: ", dir, "\n")
			}
			tmpSVMObject = findSoftware (SVMObject, searchPath = dir, verbose = verbose)
			
			if (is.null (tmpSVMObject) == TRUE) {
				warning ("The wrapper did not return any SVM Object. This seems to be an error in the wrapper code.")
			}

			# additional check if the object is somewhat valid (TODO: can we have a 'isSVMObjectValid(object)'?
			if (is.null(tmpSVMObject$wrapperPath) == TRUE) {
				warning ("The wrapper did not return a valid SVM Object. This seems to be an error in the wrapper code.")
			}
			
			# we need both binaries 
			if (is.null (tmpSVMObject$trainBinaryPath) == FALSE) {
				if (verbose == TRUE) {
					cat ("    Found train binary in", dir, ": ", SVMObject$trainBinaryPath, "\n")
				}
				SVMObject$trainBinaryPath = tmpSVMObject$trainBinaryPath
			}
			
			if (is.null (tmpSVMObject$testBinaryPath) == FALSE) {
				if (verbose == TRUE) {
					cat ("    Found test binary in", dir, ": ", SVMObject$testBinaryPath, "\n")
				}
				SVMObject$testBinaryPath = tmpSVMObject$testBinaryPath
			}
		}
	} else {
		if (verbose == TRUE) {
			cat ("No wrapper path was found in method ", method, ". Cannot search for a binary.\n")
		}
	}
		
	found = (is.null (SVMObject$testBinaryPath) == FALSE) & (is.null (SVMObject$trainBinaryPath) == FALSE)
	if ((found == TRUE) & (verbose == TRUE)) {
		cat ("FOUND: Both test and train binaries. for method", method, "\n")
	}
	
	setSVMObject (method, SVMObject)
	return (found)
}

