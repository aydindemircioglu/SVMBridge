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
 
 
 
#' Check whether a binary's output contains given strings.
#'
#' Given a pattern, this function will check whether the given binary has these patterns
#' in its output string, when called without any arguments. This helps all wrappers in identifying 
#' the correct binary.
#'
#' @param	trainBinaryPath		Path of binary to check.
#' @param	patterns		List of patterns to check for. Checking is done via grep.
#' @param	verbose			Be verbose?
#'
#' @return	True, if all patterns matches, False if not.
#'
#' @note		To make sure that the binary is correct, it will be executed!
# 	Furthermore, many SVM packages derive from libSVM. as such, they
#	often do not change the prediction binary, so care must be taken (if the 
#' binary is really not exchangeable).
#' @note		Our problem child, svmperf, waits for an key. therefore a 
#' flag applyKeyFix was handed before to simulate a keypress. but this 
#' seemed to be platform dependent. On non-unix platforms this simply 
#' did not work out. Instead now this function will load the whole binary 
#' into memory and grep for the strings there. But by reading docs-- rtfm--
#' it became clear that system2 has an 'input' parameter that just sends
#' the simulated key to the binary-- this must be platform independent.
#' so we remove applyKeyFix for the user, and later from code too.
#'
#' @export

checkExecutionStrings = function (trainBinaryPath = NULL, patterns = NULL, verbose = FALSE) {

	if (verbose == TRUE) { 
		cat("    Checking if binary has the correct output patterns.\n") 
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
		cat ("    Executing binary now.\n") 
	}
	args = c()
	
	# add echo 1 to circumvent the stupid wait-for-key tactic in SVMperf. most stupid program ever.
	# for now: implement as a HOTFIX
	applyKeyFix  = FALSE
	if (applyKeyFix == TRUE) {
		if (verbose == TRUE) { 
			cat ("    Applying key (aka SVMperf) fix.\n") 
		}
		
		# this is platform dependent
		if(.Platform$OS.type == "unix") {
			stdout = system3 ("echo", args = c("1", "|", trainBinaryPath), verbose = FALSE)
		} else {
			c = file(trainBinaryPath, "rb")
			stdout = list()
			stdout$output = readBin(c, character(), n = 100000000) # assume the string is within the first 100mb of the file.
			close (c)
		}
	} else {
		# simulate keypress by input parameter
		stdout = system3 (trainBinaryPath, args = c(), verbose = FALSE, input = "A\nB\nC")
	}

	
	matches = 0
	for (o in patterns) {
		if (length(suppressWarnings(grep(o, stdout$output))) != 0) {
			matches = matches + 1
		}
	}

	if (matches == length(patterns)) {
		if (verbose == TRUE) { 
			cat ("    FOUND. Output matches all the patterns!\n") 
		}
		return (TRUE)
	}

	return (FALSE)
}

