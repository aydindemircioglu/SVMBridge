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


#' Make a new SVM package known to the bridge.
#'
#' This function will create an object corresponding to the SVM object.
#' By using the findSVMSoftware/findSVMWrapper functions., one can add
#' search for the corresponding wrappe and the software.
#' To avoid lengthy searches,  this function can be used to directly 'shortcut'
#' the search, by specifiying the directories where the wrapper/software lies.
#' If this is successful, later findSVM.. calls are unneccessary.
#'
#' @param 	method			Name of solver
#' @param	wrapperName		Name of the wrapper (as filename). if none given, method_wrapper.R will be used.
#' @param	wrapperPath			File path to the wrapper. if none is given, findSVMWrapper needs to be called
#' @param	softwarePath		Path where to find the solver (software), if none is given, findSVMSoftware has to be called
#' @param	verbose			Be verbose?
#'
#' @note	 	first the given train and testBinaryPaths will be directly checked.
#' if the binary does not exist there, the softwarePath will be added and rechecked
#' and only if this does not work, the software will be searched via softwarePath.
#' so one can override the search by specifiying train-/testBinaryPath.
#' @note		If the wrapper searches for software, it will NOT execute it, existance is enough at this point.
#'
#' @export

addSVMPackage = function (method = NA, wrapperName = NA, wrapperPath = NA, softwarePath = NA, verbose = FALSE) {
	checkmate::assertString (method)

	# remove any old object and create a new one by overwriting
	if (verbose == TRUE) {
		cat ("Creating new SVM object for method ", method, "\n")
	}

	# create a dummy s3 object first, as we might have no wrapper yet
	SVMObject = BBmisc::makeS3Obj(c(method, "SVMWrapper"), method = method)

	# add wrapper name, if not provided.
	if (checkmate::testString (wrapperName) == FALSE) {
		SVMObject$wrapperName = paste0 (method, "_wrapper.R")
	}

	setSVMObject (method, SVMObject)

	# just to be sure
	SVMObject$wrapperPath = NULL

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
			if (verbose == TRUE) {
				cat ("    Generated wrapper name via wrapper path ", wrapperPath, " as given one was no file.\n")
			}
			if ((file.exists (wrapperPath) == TRUE) & (dir.exists (wrapperPath) == FALSE)) {
				if (verbose == TRUE)
					cat ("    Found wrapper at", wrapperPath, "\n")
				source (wrapperPath, local = FALSE)
				SVMObject$wrapperPath = wrapperPath
			} else {
				if (verbose == TRUE) {
					cat ("    Not able to find the wrapper. Really tried to find the wrapper. Sorry. \n")
				}
			}
		}
	} else {
		# if we have not found the wrapper yet, we search at the software path
		if (checkmate::testString (softwarePath) == TRUE) {
			if (verbose == TRUE) {
				cat ("    Searching wrapper at software path.\n")
			}

			wrapperPath = file.path (softwarePath, SVMObject$wrapperName)
			if ((file.exists (wrapperPath) == TRUE) & (dir.exists (wrapperPath) == FALSE)) {
				if (verbose == TRUE)
					cat ("     Found wrapper at", wrapperPath, "\n")
				source (wrapperPath, local = FALSE)
				SVMObject$wrapperPath = wrapperPath
			}
		}

	}

    # finally search the prepackaged wrappers, if nothing else goes
    if (is.null (SVMObject$wrapperPath) == TRUE) {
        wrapperPath = file.path (path.package("SVMBridge"), "wrapper")
        if (verbose == TRUE) {
            cat ("    Generated default wrapper path ", wrapperPath, " as no wrapper path was given.\n")
        }

        wrapperPath = file.path (wrapperPath, SVMObject$wrapperName)
        if (file.exists (wrapperPath) == TRUE) {
            if (verbose == TRUE)
                cat ("    Found prepackaged wrapper at", wrapperPath, "\n")
            source (wrapperPath, local = FALSE)
            SVMObject$wrapperPath = wrapperPath
        }
    }

	if (is.null (SVMObject$wrapperPath) == TRUE) {
		if (verbose == TRUE) {
			cat ("    Not able to find the wrapper. Sorry. \n")
		}
	} else {
		# if we have a wrapper, we can create the true object now

		# double check
		if (exists (paste0("createSVMWrapper", ".", method)) == TRUE) {
			# save where we go the wrapper from
			wrapperPath = SVMObject$wrapperPath

			constructor = utils::getS3method("createSVMWrapper", class = method)
			SVMObject = do.call(constructor, list())

			# add wrapper name, if not provided.
			if (checkmate::testString (wrapperName) == FALSE) {
				SVMObject$wrapperName = paste0 (method, "_wrapper.R")
			}
			SVMObject$wrapperPath = wrapperPath
		} else {
			if (verbose == TRUE) {
				cat ("Broken wrapper? Found a wrapper, but no corresponding constructor.\n")
			}
		}
	}



	# now, if a software path is given, then we should check it and try to find
	# the binaries. if that doesnt work, its not our problem.

	# but if we have no wrapper, we cannot find any software..
	if (checkmate::testString (softwarePath) == TRUE) {
		if ((is.null(wrapperPath) == FALSE) & (file.exists(wrapperPath))) {
			# ask the wrapper if it can load the binaries from the given path
			SVMObject = findSoftware (SVMObject, searchPath = softwarePath, verbose = verbose)

			# assume here that predict and test are in the same directory. if not, the user has to do magic by findSVM... or manually
			if ((is.null(SVMObject$trainBinaryPath) == TRUE) | (is.null(SVMObject$testBinaryPath) == TRUE)) {
				SVMObject = findSoftware (SVMObject, searchPath = file.path (softwarePath, "bin"), verbose = verbose)
			}
		} else {
			warning ("    No software path is given, so did not search for the software.")
		}
	} else {
		if (verbose == TRUE) {
			cat ("    Not testing for a default binary, as no software path was given.\n")
		}
	}

	setSVMObject (method, SVMObject)
}
