#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		SVMWrapper.R
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



#' General class for any SVM wrapper.
#'
#' Description... 
#'

	createSVMWrapper = function(method, trainingBinary = '', ...) {
		
		# check that trainingBinary exists, if given
		# ...    
		
		# register the newly created object in our environment
		
		# create S3 Object
		svmPackage = BBmisc::makeS3Obj(c(method, "SVMWrapper"),
			method = method,
			trainingBinary = trainingBinary
		)

		# TODO: replace existing object in package list
		
		# add object to our internal list of created wrappers
	#	SVMBridgeEnv$packages = c(SVMBridgeEnv$packages, svmPackage)
		
		return (svmPackage)
	}
	

	

	createTrainingArguments = function (x,
		trainDataFile = "",
		modelFile = "",
		extraParameter = "",
		kernelCacheSize = 1024,
		cost = 1, 
		useBias = FALSE,
		gamma = 1,
		epsilon = 0.001, 
		...) 
	{
		UseMethod("createTrainingArguments")
	}

	
	
	createTestArguments = function (x,
		testDataFile = "",
		modelFile = "", 
		predictionsFile = "",
		...) 
	{
		UseMethod("createTestArguments")
	}


	
	extractTrainInfo = function (x, output) {
		UseMethod ("extractTrainInfo")
	}
	
	
	
	extractTestInfo = function (x, output) {
		UseMethod ("extractTestInfo")
	}
	

	
	readModel = function (x, modelFile = './model', verbose = FALSE) {
		UseMethod("readModel")
	}



	writeModel = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		UseMethod("writeModel")
	}


	
	readPredictions = function (x, predictionsFile = "", verbose = FALSE) {
		UseMethod ("readPredictions")
	}

	
	
	findSoftware = function (x, searchPath = "./", verbose = FALSE) {
		UseMethod ("findSoftware")
	}
	
	