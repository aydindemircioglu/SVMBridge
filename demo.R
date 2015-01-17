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

library(devtools)
load_all (".")

#library (SVMBridge)

# add new wrappers -- this is done in initPackage for wrappers known to the package
# (so this is currently done twice)
addSVMPackage (filePath = "./R/LIBSVM_wrapper.R", softwarePath = "../../../svm_large_scale/software/libSVM", verbose = TRUE)
addSVMPackage (filePath = "./R/CVM_wrapper.R", softwarePath = "../../../svm_large_scale/software/libCVM", verbose = FALSE)


# alternatively, addSVMPackage without softwarePath and search all in a given path
#findAllSVMSoftware (searchPath = "../../../../svm_large_scale/software/", verbose = FALSE) 


# wird alle bekannte software-pakete suchen also SVMperf, libSVM, ...
#FIXME: allow path like ~/
outputAllSVMSoftwarePackages ()


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
	
#  this will train from memory.
# as no modelfile was given, callSVM will create a temporay model file
# and read this after training, so it will be in svmObj$model
	svmObj =  trainSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = TRUE
    )  


# or, if preferred, load sparse data from disk
    svmObj =  trainSVM(
		method = "LIBSVM",
		trainDataFile = './tests/data/australian.train',
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = TRUE
    )  

    
# now evaluate from model in memory 
    svmObj =  testSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
        verbose = TRUE,
        model = svmObj$model
    )  


# train from memory, but save the model file to disk
    svmObj =  trainSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE,
        modelFile = "/tmp/libsvm_model.txt"
    )  
		    
# predict from memory, model from disc
    svmObj =  testSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
        modelFilePath = "/tmp/libsvm_model.txt"
    )  

    
# predict from file, model from file
    svmObj =  testSVM(
		method = "LIBSVM",
		testDataFile = './tests/data/australian.train',
        modelFilePath = "/tmp/libsvm_model.txt",
        verbose = TRUE
    )  
    
