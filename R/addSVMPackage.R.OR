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
 
 


# every wrapper has its own object that is derived from the universalWrapper.
# we create all these at initialization time of the package.
# then, when e.g. paths are updated, we have a nice place to store these.


addSVMPackage <- function (filePath = NA, softwarePath = NA, verbose = FALSE)
{
	if (is.na(filePath) == TRUE) {
		stopf("Please provide a file path for SVM package to add.")
	}
	
	source (filePath, local = FALSE)
	
	# now we make a crucial assumption here,
	# to make everything 'magical',
	# assume that the filename already denotes the
	# method name and (all the callback names if needed)
	# as well as the caller name (evalXXX).
	baseName = basename (filePath)
	methodName = strsplit(baseName, "_")[[1]][1]
	SVMBridgeEnv$packages = c(SVMBridgeEnv$packages, methodName)
	SVMBridgeEnv[[methodName]] = list()
	
	# FIXME 
	SVMBridgeEnv[[methodName]]$trainBinaryPattern = "svm-train"
	SVMBridgeEnv[[methodName]]$trainBinaryOutputPattern = "Usage: svm-train .options. training_set_file .model_file."
	SVMBridgeEnv[[methodName]]$testBinaryPattern = "svm-predict"
	SVMBridgeEnv[[methodName]]$testBinaryOutputPattern = "for one-class SVM only 0 is supported"

	
	# add software path
	if (is.na(softwarePath) == FALSE) {
		findSVMSoftware (method = methodName, searchPath = softwarePath, verbose = verbose) 
	}
}



outputAllSVMSoftwarePackages <- function () {
	messagef("Currently known solver:")
	for (package in SVMBridgeEnv$packages) {
		messagef("  %s:", package)
		messagef("    TrainBinary: %s", SVMBridgeEnv[[package]]$trainBinaryPath )
		messagef("    TestBinary: %s", SVMBridgeEnv[[package]]$testBinaryPath )
	}
}

