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
			BBmisc::messagef("API: Searching for all software packages:")
		}
		
		for (i in seq(1, length(SVMBridgeEnv$packages))) {
			method = SVMBridgeEnv$packages[[i]]$method
			if (verbose == TRUE) {
				BBmisc::messagef("  Searching for software for SVM package %s:", method)
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
			BBmisc::messagef("API: Finding software for %s", method)
		}
		
		if (is.na(searchPath)) {
			BBmisc::stopf("No search path is given!")
		}
		
		if (is.na(method)) {
			BBmisc::stopf ("No method name is given")
		}
		
		if (verbose == TRUE) {
			BBmisc::messagef("  Try to find binaries for %s", method) 
		}
		
		#look for tile characters and expand them
		if(grepl("~", searchPath) == TRUE){
			if(.Platform$OS.type == "windows"){
				firstPart = path.expand("~")
				firstPart = substr(firstPart, 1, nchar(firstPart) - 10)
				secondPart = substr(searchPath, 2, nchar(searchPath))
				searchPath = paste(firstPart, secondPart, sep="")
			}
			else
				searchPath = path.expand(searchPath)
			searchPath = gsub("[\\]", "/", searchPath)
			
			if(verbose == TRUE){
				BBmisc::messagef("  Expanded path to dataset: %s", searchPath)
			}
		}

		# call the find software method of the solver
		SVMBridgeEnv$packages[[method]] = findSoftware (SVMBridgeEnv$packages[[method]], searchPath = searchPath, verbose = verbose)
		
		# TODO: to get better tests, maybe we need an option like "TEST = true", which will
		# take a demo-data-file and compute the model. so actuallly its like a unittest, but
		# it is executed during use, to make sure everything is as it should be.
	}

