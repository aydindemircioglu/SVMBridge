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
	library(BBmisc)
	library(devtools)
	load_all (".")
# 	build_vignettes(".")
# 	document(".")
# 	# devtools::build_win()	
# 	# is done by document/loadall anyway?
# 	library(Rcpp)
# 	compileAttributes()

	# run tests
	#devtools::test()
#	devtools::check()
# R CMD check ./SVMBridge_1.0.tar.gz

char_vec = c( "LIBSVM") 


for(solver in char_vec)
{
	# as the libary already loads default wrappers this works
		addSVMPackage (method = solver, verbose = FALSE)
		findSVMSoftware (solver, searchPath = "../../../svm_large_scale/software/", verbose = TRUE)

		verbose = FALSE
		
	# load iris  for now
		shufflediris = iris[sample(nrow(iris)),]

		trainDataX = data.matrix(shufflediris [1:100,1:4])
		trainDatay = data.matrix(as.numeric(shufflediris [1:100, 5]))
		testDataX = data.matrix(shufflediris [-(1:100),1:4])
		testDatay = data.matrix(as.numeric(shufflediris [-(1:100), 5]))

		# FIXME: for now we only accept binary labels
		trainDatay[trainDatay==3] = 1
		testDatay[testDatay==3] = 1
		trainDatay[trainDatay==2] = -1
		testDatay[testDatay==2] = -1
#		writeSparseData (X = trainDataX, Y = trainDatay, file = "iris.sparse")
		
		C = 0.71
		gamma = 0.41
		
		messagef("\n\n\n======= Train %s, Traindata from Memory, Model to Memory", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataX = trainDataX, 
			trainDatay = trainDatay, 
			cost = C, 
			gamma = gamma, 
			epsilon = 0.0000000001, 
#			modelFile = "./tmpMo",
			readModelFile = TRUE,
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			verbose = verbose
		)  
		
		# extract optimization values from model
		oV = optimizationValues(X = trainDataX, Y = trainDatay, model = trainObj$model, C = C, values = c(), verbose = FALSE)

		source("./tests/computeOptimizationValuesLibSVM.R")
		data = list()
		data$x = trainDataX
		data$y = trainDatay
		trainObj$model$L = 1
		trainObj$model$C = C
		trainObj$model$X = trainObj$model$SV
#		trainObj$model$bias = -trainObj$model$bias
		pV = computeOptimizationValuesLibSVM (trainObj$model, NULL, data = data,  predictionOutput = NULL, verbose = FALSE)
		cat (oV$primal, " ", oV$dual, "\n")
		cat (pV$primal, " ", pV$dual, "\n")
}
    

    
   # ~/svm_large_scale/software/libSVM/bin/svm-predict -c 0.71 -o iris.sparse iris.sparse tmpMo A 
# Obtained cost C: 0.710000
# Obtained gamma g: 0.410000
# Current rho: [0.064655]
# Current hingeLoss: [83.120949]
# Current weight: [2.025757]
# Computed primal value: [61.067719]
# Computed dual value: [2.171971]
