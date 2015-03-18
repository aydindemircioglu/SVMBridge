#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		addSVMPackage.R
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
 
 
 
addSVMPackage <- function (method = NA, filePath = NA, softwarePath = NA, verbose = FALSE)
{
	# we always need a method
	if (is.na(method) == TRUE) {
		BBmisc::stopf("Please provide the name of the SVM package to create")
	}

	# load definition of SVM object
	if (is.na(filePath) == FALSE) {
		source (filePath, local = FALSE)
	}

	
	# TODO: test if the object specified by the name exists
	
	
	# create an object now
	if (verbose == TRUE) {
		BBmisc::messagef ("Creating SVM Wrapper for method %s: ", method)
	}
	SVMObject = createSVMWrapper( method = method)
	SVMBridgeEnv$packages[[method]] = SVMObject
	if (verbose == TRUE) {
		BBmisc::messagef ("Registred Objects")
		for (i in seq(1, length(SVMBridgeEnv$packages))) {
			s = SVMBridgeEnv$packages[i]
			print (s)
		}
	}

	
	# add software path
	if (is.na(softwarePath) == FALSE) {
		if (verbose == TRUE) {
			BBmisc::messagef ("  Specified software path, so searching for software package: ")
		}
		findSVMSoftware (method = method, searchPath = softwarePath, verbose = verbose) 
	}
}



outputAllSVMSoftwarePackages <- function () {
	BBmisc::messagef("Currently known solver:")
	for (package in SVMBridgeEnv$packages) {
		print (package)
	}
}


getSVMInstance <- function ( method = method) {
	return (SVMBridgeEnv$packages[[method]])
}
