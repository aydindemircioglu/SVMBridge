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



#' Find a given binary in given directory.
#'
#' Given a file pattern and an std-out-pattern it will try to find a binary matching
#' the file pattern (which contains std-out-pattern as output if called without arguments)
#' in the path given.
#'
#' @param	binaryName		Search name for the SVM binary
#' @param	dir	 	Search path for the SVM binary.
#' @param	patterns		Pattern for output of the binary file
#' @param	verbose			Print messages while searching?
#'
#' @return	name of the found binary matching the pattern
#'
#' @note		To make sure that the binary is correct, it will be executed!
#'					This happens only if this routine is given some pattterns.
# 					Furthermore, many SVM packages derive from libSVM. as such, they
#					often do not change the prediction binasry. We will try to sort these out,
#					but it might be hopeless. With luck, the found binary will be left untouched,
#					and thus work, if not, you must set the path by hand.
#' @note		If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.

findBinaryInDirectory = function (binaryName = NULL, dir = NULL, patterns = NULL, verbose = FALSE) {

	# check all the parameters
	checkmate::assertString (binaryName)
	checkmate::assertList (patterns)
	checkmate::assertFlag (verbose)
	checkmate::assertString (dir)
	for (l in patterns) {
		checkmate::assertString (l)
	}

	if (verbose == TRUE) {
		cat("    Searching for ", binaryName, " in directory ", dir,"\n")
	}

	# is the binary there?
	binaryPath = file.path (dir, binaryName)
	if (file.exists (binaryPath) == TRUE) {
		if (verbose == TRUE) {
			cat("    Found matching ", binaryPath, "\n")
		}

		# do we have any patterns to check?
		if (length(patterns) > 0) {
			# if yes, we have to execute the thing. if they do not match, we delete the path (removed applyKeyFix = applyKeyFix, )
			if (checkExecutionStrings (binaryPath, patterns = patterns, verbose = verbose) == FALSE) {
				binaryPath = NULL
			} else {
				# everything is ok, binary path is not deleted
			}
		} else {
			# everything is ok, binary path is not deleted
		}
    } else {
		# no binary. so we return
		binaryPath = NULL
	}

    return (binaryPath)
}
