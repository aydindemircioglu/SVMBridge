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
	build_vignettes(".")
	document(".")
	# devtools::build_win()	
	# is done by document/loadall anyway?
	library(Rcpp)
	compileAttributes()

	# run tests
	#devtools::test()
#	devtools::check()
# R CMD check ./SVMBridge_1.0.tar.gz

char_vec = c( "LASVM", "LIBSVM", "SVMperf" ,"BSGD", "BVM", "CVM")# "LLSVM", "Pegasos")


for(solver in char_vec)
{
	# as the libary already loads default wrappers this works
		addSVMPackage (method = solver, verbose = FALSE)
		findSVMSoftware (solver, searchPath = "../../../svm_large_scale/software/", verbose = TRUE)

	# wird alle bekannte software-pakete suchen also SVMperf, libSVM, ...
	#FIXME: allow path like ~/
		outputAllSVMSoftwarePackages ()
	# 

		verbose = TRUE
		
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

		
		
		messagef("\n\n\n======= Train %s, Traindata from Memory, Model to Memory", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataX = trainDataX, 
			trainDatay = trainDatay, 
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			readModelFile = TRUE,
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			verbose = verbose
		)  
		

		messagef("\n\n\n======= Train %s, Traindata from File, Model to Memory", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataFile = './tests/data/australian.test',
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			readModelFile = TRUE,
			verbose = verbose
		)  

		
		messagef("\n\n\n======= Test %s, Testdata from Disk, Model from Memory, Predictions to Memory", solver)
		testObj =  testSVM(
			method = solver,
			testDataFile = './tests/data/australian.test',
			model = trainObj$model,
			verbose = verbose,
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
		)  

		
		messagef("\n\n\n======= Test %s, Testdata from Memory, Model from Memory, Predictions to Memory", solver)
		testObj =  testSVM(
			method = solver,
			testDataX = testDataX, 
			testDatay = testDatay, 
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			verbose = verbose,
			model = trainObj$model
		)  

		
		messagef("\n\n\n======= Test %s, Testdata from Disk, Model from Memory, Predictions to Disk", solver)
		testObj =  testSVM(
			method = solver,
			testDataFile = './tests/data/australian.test',
			model = trainObj$model,
			predictionsFile = "./tmp/predictions.txt",
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			verbose = verbose
		)  

		
		messagef("\n\n\n======= Test %s, Testdata from Memory, Model from Memory, Predictions to Disk", solver)
		testObj =  testSVM(
			method = solver,
			testDataX = testDataX, 
			testDatay = testDatay, 
			model = trainObj$model,
			predictionsFile = "./tmp/predictions.txt",
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			verbose = verbose,
		)  

		
		
		
		
		messagef("\n\n\n======= Train %s, Traindata from Disk, Model to Disk", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataFile = './tests/data/australian.test',
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			verbose = verbose,
			modelFile = "/tmp/libsvm_model.txt",
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			readModelFile = FALSE
		)  


		messagef("\n\n\n======= Train %s, Traindata from Memory, Model to Disk", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataX = trainDataX, 
			trainDatay = trainDatay, 
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			verbose = verbose,
			readModelFile = FALSE,
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			modelFile = "/tmp/libsvm_model.txt"
		)  
				

		messagef("\n\n\n======= Test %s, Testdata from Memory, Model from Disk, Predictions to Memory", solver)
		testObj =  testSVM(
			method = solver,
			testDataX = testDataX, 
			testDatay = testDatay, 
			modelFile = "/tmp/libsvm_model.txt",
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			verbose = verbose
		)  

		

		messagef("\n\n\n======= Test %s, Testdata from Disk, Model from Disk, Predictions to Memory", solver)
		testObj =  testSVM(
			method = solver,
			testDataFile = './tests/data/australian.test',
			modelFile = "/tmp/libsvm_model.txt",
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			verbose = verbose
		)  
		

		
		messagef("\n\n\n======= Test %s, Testdata from Memory, Model from Disk, Predictions to Disk", solver)
		testObj =  testSVM(
			method = solver,
			testDataX = testDataX, 
			testDatay = testDatay, 
			modelFile = "/tmp/libsvm_model.txt",
			predictionsFile = "./tmp/predictions.txt",
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			verbose = verbose
		)  

		

		messagef("\n\n\n======= Test %s, Testdata from Disk, Model from Disk, Predictions to Disk", solver)
		testObj =  testSVM(
			method = solver,
			testDataFile = './tests/data/australian.test',
			modelFile = "/tmp/libsvm_model.txt",
			predictionsFile = "./tmp/predictions.txt",
			subsamplingRate = 0.5,
			subsamplingMethod = "cutoff",
			verbose = verbose
		)  
		
		messagef("\n\n\n======= Finished all demo calls successfully.", solver)
		
}
    
