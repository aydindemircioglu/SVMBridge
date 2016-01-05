#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#
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

#library (SVMBridge)
	library(methods)
	library(devtools)
	load_all (".")

	# 2. alle datasets
	datasets = c("aXa")#, "protein", "poker")
	solver = "LIBSVM"
		
	# as the libary already loads default wrappers this works
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMSoftware (solver, searchPath = "../../shark/svm_large_data/software/", verbose = TRUE)

	
	for (d in datasets) {	
		# assemble test and train paths
		trainFile = paste ("../../shark/svm_large_data/datasets/", d, "/", d, ".combined.scaled", sep = "")
		print(trainFile)
		
	
		verbose = TRUE
		cost = runif(1)
		gamma = runif(1)
		subsamplingrate = 0.05
		
		# do this without reading/writing the model
		trainObj =  trainSVM(
			method = solver,
			trainDataFile = trainFile, 
			subsamplingRate = subsamplingrate,
			cost = cost, 
			gamma = gamma, 
			#epsilon = 0.5, 
			readModelFile = FALSE,
			modelFile = "/tmp/model_without.txt",
			verbose = verbose
		)  
		
		testObj =  testSVM(
			method = solver,
			testDataFile = trainFile,
			modelFile = "/tmp/model_without.txt",
			predictionsFile = "/tmp/predictions_without.txt",
			verbose = verbose
		) 

		trainObj =  trainSVM(
			method = solver,
			trainDataFile = trainFile, 
			subsamplingRate = subsamplingrate,
			cost = cost, 
			gamma = gamma, 
			#epsilon = 0.5, 
			readModelFile = TRUE,
			verbose = verbose
		)  
		
		SVMObject = SVMBridgeEnv$packages[[solver]]
		writeModel(SVMObject, "/tmp/model.txt", ...)
		
		testObj =  testSVM(
			method = solver,
			testDataFile = trainFile,
			model = trainObj$model,
			predictionsFile = "/tmp/predictions.txt",
			verbose = verbose
		)  
		
		# compare both predictions
		# comapre both models
		# 1. they must be equal.
	}
