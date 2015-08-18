#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		findSVMWrapper.R
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
 


# findSVMWrapper
#		given a search path, it will try to find the corresponding wrapper for the given method.
#
# @param 	searchPath	 	search the given path for the SVM binaries of all known SVM packages.
# @param	verbose			print messages while searching?
#
# @note		To make sure that the binary is correct, it will be executed! (see findBinary for more infos)
# @note		If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.

	findSVMWrapper <- function (method = NA, name = NA, searchPath = NA, verbose = FALSE) {
		if (verbose == TRUE) {
			BBmisc::messagef("API: Finding wrapper for %s", method)
		}
		
		if (is.na(searchPath)) {
			BBmisc::stopf("No search path is given!")
		}
		
		#look for tilde characters and expand them
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
		
		if (is.na(method)) {
			BBmisc::stopf ("No method name is given")
		}
		
		if (verbose == TRUE) {
			BBmisc::messagef("  Trying to find wrapper for %s", method) 
		}

		if (is.na (name) == TRUE) {
			pattern = paste( "^", method, "_wrapper.R$", sep = "")
		} else {
			pattern = paste( "^", name, "$", sep = "")
		}

		if (verbose == TRUE) {
			BBmisc::messagef("  Looking for a wrapper with regex %s.", pattern)
		}
		
		files <- list.files (searchPath, pattern = pattern, recursive = TRUE)
		foundWrapper = ''
		for (file in files) {
			wrapperPath = file.path(searchPath, file)
			if (verbose == TRUE) { 
				BBmisc::messagef("    -Found wrapper at %s", wrapperPath) 
			}
			source (wrapperPath, local = FALSE)
			break;
		} 
    
		# WHAT should happen if we do not find one.
    
		# TODO: to get better tests, maybe we need an option like "TEST = true", which will
		# take a demo-data-file and compute the model. so actuallly its like a unittest, but
		# it is executed during use, to make sure everything is as it should be.
	}


	

# findAllSVMWrapper
#		given a search path, it will try to find the corresponding wrapper
#		for all registered SVM packages.
#
# @param 	searchPath	 	search the given path for the SVM wrappers of all known SVM packages.
# @param	verbose			print messages while searching?
#
# @note		There is no extra check on the found wrapper. It will simply be sourced.
# @note		If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.

	findAllSVMWrapper <- function (searchPath = NA, verbose = FALSE) {
		if (verbose == TRUE) {
			BBmisc::messagef("API: Searching for all software wrappers:")
		}
		
		for (i in seq(1, length(SVMBridgeEnv$packages))) {
			method = SVMBridgeEnv$packages[[i]]$method
			if (verbose == TRUE) {
				BBmisc::messagef("  Searching for wrapper for SVM package %s:", method)
			}
			findSVMWrapper (method = method, searchPath = searchPath, verbose = verbose)
		}
	}


	