#!/usr/bin/Rscript  --vanilla 
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
	if (verbose == TRUE) { messagef("  Checking for pattern %s", pattern) }
	files <- list.files (searchPath, pattern = pattern, recursive = TRUE)
    foundBinary = ''
    for (binary in files) {
		binaryPath = file.path(searchPath, binary)
		if (verbose == TRUE) { messagef("    -Found binary at %s", binaryPath) }
		stdout = system3(binaryPath, args = c(), verbose = FALSE)
		if (length(grep(outputPattern, stdout$output)) != 0) {
			foundBinary = binaryPath
		}
    } 
    
    # TODO:: should we here throw an error? or leave it to the upper class
    
    return (foundBinary)
}



# findAllSVMSoftware 
#		given a search path, it will try to find the corresponding software packages
#		for all registered SVM packages.
#
# @param 	searchPath	 	search the given path for the SVM binaries of all known SVM packages.
# @param	verbose			print messages while searching?
#
# @note		To make sure that the binary is correct, it will be executed! (see findBinary for more infos)
# @note		If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.

	findAllSVMSoftware <- function (searchPath = NA, verbose = FALSE) {
		if (verbose == TRUE) {
			messagef("API: Searching for all software packages:")
		}
		
		for (i in seq(1, length(SVMBridgeEnv$packages))) {
			method = SVMBridgeEnv$packages[[i]]$method
			if (verbose == TRUE) {
				messagef("  Searching for software for SVM package %s:", method)
			}
			findSVMSoftware (method = method, searchPath = searchPath, verbose = verbose)
		}
	}



# findAllSVMSoftware 
#		given a search path, it will try to find the corresponding software packages
#		for all registered SVM packages.
#
# @param 	searchPath	 	search the given path for the SVM binaries of all known SVM packages.
# @param	verbose			print messages while searching?
#
# @note		To make sure that the binary is correct, it will be executed! (see findBinary for more infos)
# @note		If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.

	findSVMSoftware <- function (method = NA, searchPath = NA, verbose = FALSE) {
		if (verbose == TRUE) {
			messagef("API: Finding software for %s", method)
		}
		
		if (is.na(searchPath)) {
			stopf("No search path is given!")
		}
		
		if (is.na(method)) {
			stopf ("No method name is given")
		}
		
		if (verbose == TRUE) {
			messagef("  Try to find binaries for %s", method) 
		}

		# call the find software method of the solver
		SVMBridgeEnv$packages[[method]] = findSoftware (SVMBridgeEnv$packages[[method]], searchPath = searchPath, verbose = verbose)
		
		# TODO: to get better tests, maybe we need an option like "TEST = true", which will
		# take a demo-data-file and compute the model. so actuallly its like a unittest, but
		# it is executed during use, to make sure everything is as it should be.
	}

