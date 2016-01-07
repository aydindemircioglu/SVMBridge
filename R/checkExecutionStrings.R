#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		checkExecutionStrings.R
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
 
#' checkExecutionStrings
#'
#'		given a file pattern and an std-out-pattern it will try to find a binary matching
#'		the file pattern (which contains std-out-pattern as output if called without arguments)
#'		in the path given.
#'
#' @param	searchPath	 	search the given path for the SVM binaries recursively.
#' @param	pattern		pattern for the binary file
#' @param	outputPattern		pattern for the stdout output of the binary
#' @param	verbose			print messages while searching?
#' @return	name of the found binary matching the pattern
#' @note		To make sure that the binary is correct, it will be executed!
# 					Furthermore, many SVM packages derive from libSVM. as such, they
#					often do not change the prediction binasry. We will try to sort these out,
#					but it might be hopeless. With luck, the found binary will be left untouched,
#					and thus work, if not, you must set the path by hand.
#' @note		If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.
#' @export
checkExecutionStrings = function (trainBinaryPath = NULL, patterns = NULL, applyKeyFix = FALSE, verbose = TRUE) {

	if (verbose == TRUE) { 
		cat("Checking if binary has the correct output patterns.\n") 
	}
	checkmate::checkString (trainBinaryPath)
	
	# if we do not need to check anything, everything is good
	if (is.null (patterns) == TRUE) {
		if (verbose == TRUE) { 
			cat("    No patterns given.\n") 
		}
		return (TRUE)
	}

	# execute 
	if (verbose == TRUE) { 
		cat ("    Executing binary now.") 
	}
	args = c()
	
	# add echo 1 to circumvent the stupid wait-for-key tactic in SVMperf. most stupid program ever.
	# for now: implement as a HOTFIX
	if (applyKeyFix == TRUE) {
		if (verbose == TRUE) { 
			cat ("    Applying key (aka SVMperf) fix") 
		}
		stdout = system3 ("/bin/echo", args = c("1", "|", trainBinaryPath), verbose = FALSE)
	} else {
		stdout = system3 (trainBinaryPath, args = c(), verbose = FALSE)
	}

	matches = 0
	for (o in patterns) {
		if (length(grep(o, stdout$output)) != 0) {
			matches = matches + 1
		}
	}

	if (matches == length(patterns)) {
		if (verbose == TRUE) { 
			cat ("    Output matches all the patterns.\n") 
		}
		return (TRUE)
	}

	return (FALSE)
}

