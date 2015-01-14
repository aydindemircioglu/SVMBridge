library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)
library(mco)

load_all(".", reset=TRUE)

set.seed(6)


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
 

# replace this by libary (SVMBridge)
source ("./callSVM.R")

# should be called by itself
tmp = initPackage()

# add new wrappers -- this is done in initPackage for wrappers known to the package
# (so this is currently done twice)
addSVMPackage (filePath = "./LIBSVM_wrapper.R", softwarePath = "../../../../svm_large_scale/software/libSVM", verbose = FALSE)
addSVMPackage (filePath = "./LIBCVM_wrapper.R", softwarePath = "../../../../svm_large_scale/software/libCVM", verbose = FALSE)

# alternatively, addSVMPackage without softwarePath and search all in a given path
findAllSVMSoftware (searchPath = "../../../../svm_large_scale/software/", verbose = FALSE) 

# wird alle bekannte software-pakete suchen also SVMperf, libSVM, ...
#FIXME: allow path like ~/
#outputAllSVMSoftwarePackages ()


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
	
#  this will train and test from memory.
# as no modelfile was given, callSVM will create a temporay model file
# and read this after training, so it will be in svmObj$model
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		testDataX = testDataX, 
		testDatay = testDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = TRUE
    )  
    

# or, if preferred, load sparse data from disk
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataFile = './datasets/australian/australian.combined.scaled', 
		testDataFile = './datasets/australian/australian.combined.scaled',
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE
    )  

    
# train only from memory (similar from disk)
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE
    )  


# train from memory, but save the model file to disk
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE,
        modelFile = "./tmp/libsvm_model.txt"
    )  

    
# predict only from memory, model from memory
    svmObj =  callSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE,
        model = svmObj$model
    )  
    
    
# predict only from memory (similar from disk), model from disk
    svmObj =  callSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE,
        modelFile = "./tmp/libsvm_model.txt"
    )  

    
# train only
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataFile = './datasets/australian/australian.combined.scaled', 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE
    )  

    
# predict only from disk, model from memory
    svmObj =  callSVM(
		method = "LIBSVM",
		testDataFile = './datasets/australian/australian.combined.scaled',
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE,
        model = svmObj$model
    )  

