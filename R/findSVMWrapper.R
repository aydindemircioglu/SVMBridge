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


#' Find a given SVM Wrapper inside a search path.
#'
#' Given a search path, it will try to find the corresponding wrapper for the given method.
#' This routine will also source the file, if specified.
#'
#' @param	method		Name of the SVM method
#' @param	recursive		Recursive search?
#' @param	source		Source the found wrapper?
#' @param 	searchPath	 	Search the given path for the SVM binaries of all known SVM packages.
#' @param	verbose			Print messages while searching?
#'
#' @export

findSVMWrapper <- function (method = NA, searchPath = NA, recursive = TRUE, source = TRUE, verbose = FALSE) {

	checkmate::assertFlag (verbose)
	checkmate::assertString (method)
	checkmate::assertString (searchPath)

	if (verbose == TRUE) {
		cat("Finding wrapper for ", method, "\n")
	}

	# look for tilde characters and expand them
	searchPath = expandTilde(path = searchPath, verbose = verbose)

	# get the SVM object
	SVMObject = getSVMObject (method)
	if (checkmate::testNull (SVMObject) == TRUE) {
		stop ("Cannot find SVM Object for method ", method, ". Please create it first.")
	}
	checkmate::assertString (SVMObject$wrapperName)

	# create pattern to look for
	pattern = paste( "^", SVMObject$wrapperName, "$", sep = "")

	if (verbose == TRUE) {
		cat("    Looking for a wrapper with regex ", pattern, "\n")
	}

	files = list.files (searchPath, pattern = pattern, recursive = recursive)
	if (length(files) > 1) {
		warning ("Found multiple wrappers. Taking the first one: ", files[1])
	}

	if (length(files) == 0) {
		warning ("No wrapper found. Please specify correct path.")
		return (FALSE)
	}

	wrapperPath = file.path (searchPath, files[1])
	if (verbose == TRUE) {
		cat ("    Found wrapper at ", wrapperPath, "\n")
	}


	# save where we go the wrapper from. TODO: is there more we need to catch from
	# the dummy/current SVM OBject?
	wrapperName = SVMObject$wrapperName

	# now we got to source it.
	if (source == TRUE) {
		if (verbose == TRUE) {
			cat ("    Sourcing ", wrapperPath, "\n")
		}
		source (wrapperPath, local = FALSE)
	}


	constructor = utils::getS3method("createSVMWrapper", class = method)
	SVMObject = do.call(constructor, list())

	SVMObject$wrapperPath = wrapperPath
	SVMObject$wrapperName = wrapperName
	setSVMObject (method, SVMObject)

	return (TRUE)
}
