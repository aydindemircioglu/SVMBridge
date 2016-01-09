#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		findSVMSoftware.R
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
#' @param	method	   name of the SVM method
#' @param   searchPath   search the given path for the SVM binaries of the given SVM method.
#' @param	verbose		print messages while searching?
#' 
#' @note    calls the findSoftware routine of the corresponding wrapper.
#'
#' @export	findSVMSoftware
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
    if (is.null(SVMObject$wrapperPath) == FALSE) {
		for (dir in dirList) {
			# call the find software method of the solver
			if (verbose == TRUE) {
				cat ("    Scanning directory: ", dir, "\n")
			}
			SVMObject = findSoftware (SVMObject, searchPath = dir, verbose = verbose)
			if (is.null (SVMObject$trainBinaryPath) == FALSE) {
				if (verbose == TRUE) {
					cat ("    Found binaries in", dir, "\n")
					cat ("       ", SVMObject$trainBinaryPath, "\n")
					cat ("       ", SVMObject$testBinaryPath, "\n")
				}
				setSVMObject (method, SVMObject)
				break
			}
		}
	}
		
	return (is.null (SVMObject$trainBinaryPath) == FALSE)
}

