#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		findBinary.R
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
 

# findBinary
#		given a file pattern and an std-out-pattern it will try to find a binary matching
#		the file pattern (which contains std-out-pattern as output if called without arguments)
#		in the path given.
#
# @param 	searchPath	 	search the given path for the SVM binaries recursively.
# @param	pattern		pattern for the binary file
# @param	outputPattern		pattern for the stdout output of the binary
# @param	verbose			print messages while searching?
#
# @note		To make sure that the binary is correct, it will be executed!
# 					Furthermore, many SVM packages derive from libSVM. as such, they
#					often do not change the prediction binary. We will try to sort these out,
#					but it might be hopeless. With luck, the found binary will be left untouched,
#					and thus work, if not, you must set the path by hand.
# @note		If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.

findBinary <- function (searchPath, pattern, outputPattern, verbose = FALSE) {
	if (verbose == TRUE) { BBmisc::messagef("  Checking for pattern %s", pattern) }
	files <- list.files (searchPath, pattern = pattern, recursive = TRUE)
    foundBinary = ''
    for (binary in files) {
		binaryPath = file.path(searchPath, binary)
		if (verbose == TRUE) { BBmisc::messagef("    -Found binary at %s", binaryPath) }
		
		# add echo 1 to circumvent the stupid wait-for-key tactic in SVMperf. most stupid program ever.
		# for now: implement as a HOTFIX
		args = c()
		if (length(grep( "svm_perf", binaryPath)) != 0) {
			if (verbose == TRUE) { BBmisc::messagef("    -Applied SVMperf fix") }
			stdout = system3 ("/bin/echo", args = c("1", "|", binaryPath), verbose = FALSE)
		} else {
			stdout = system3 (binaryPath, args = c(), verbose = FALSE)
		}
		
		matches = 0
		for (o in outputPattern) {
			if (length(grep(o, stdout$output)) != 0) {
				foundBinary = binaryPath
				matches = matches + 1
			}
		}
		# stop after the first match we find
		if (matches == length(outputPattern)) {
			if (verbose == TRUE) { BBmisc::messagef ("    -This binary %s matches all the patterns!", foundBinary) }
			break;
		}
    } 
    
    # TODO:: should we here throw an error? or leave it to the upper class?
    
    return (foundBinary)
}


