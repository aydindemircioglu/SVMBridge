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
 

 
# stupid R
loadThings <- function ()
{
  library(BBmisc)
  library(stringr)
  library(R.utils)
  library(microbenchmark)
  library(e1071)
  library(methods)
  library(SparseM)
  
  source ("./system3.R")
}
suppressMessages(loadThings())





# universalWrapper
#
# @param[in]    trainfile       file to read training data from
# @param[in]    testfile        file to read test data from
# @param[in]    extraParameter  extra parameters for solver
# @param[in]    bindir          relativ path to the binaries, defaults to.. default.
# @param[in]    modelFile       path to model, defaults to a temporary file (given by R)
#

universalWrapper = function(
	# data
	trainDataFile = NULL,
	testDataFile = NULL,
	trainDataX = NULL, 
	trainDatay = NULL, 
	testDataX = NULL, 
	testDatay = NULL, 

	# full path to executable
	trainBinaryPath = NULL,
	testBinaryPath = NULL,
	
	# rest
	method = NULL,
	extraParameter = "",
	modelFilePath = NULL,
	model = NULL,
	verbose = FALSE,

	# call backs
	trainingParameterCallBack = trainingParameterCallBack(),
	testParameterCallBack = testParameterCallBack(),
	extractInformationCallBack = extractInformationCallBack(),
	readModelCallBack = readModelCallBack(),
	writeModelCallBack = writeModelCallBack(),
	predictionsCallBack = predictionsCallBack(),
	...) {

	trainBinary = basename(trainBinaryPath)
	testBinary = basename(testBinaryPath)
	
	# general modifications
	if (verbose == TRUE) {
		messagef("  Path of binary for training is %s", trainBinaryPath)
		messagef("  Path of binary for testing is %s", testBinaryPath)
		messagef("  Binary for training is %s", trainBinary)
		messagef("  Binary for testing is %s", testBinary)
	}

	
	# TODO: sanity checks for parameter
	if (is.null (method) == TRUE) {
		stopf("No method name is given, this should never happen.")
	}
	
	# user cannot give us a model in memory and train a model. (<-- in that case he should call callSVM twice)
	if ((is.null(modelFilePath) == FALSE) && (is.null(model) == FALSE)) {
		stopf ("A model in memory as well a model path was given. Please call twice, if test with different model is needed.")
	}
			
 	readModelFile = TRUE
	if (is.null(modelFilePath) == TRUE) {
		# user did not specify a modelfile
		# so we will write the model in a temp file
		# and read it back after training
		readModelFile = TRUE
		modelFilePath = tempfile()
	}
		

	# take care of data. if testdata or traindata is given,
	# the corresponding filename must be empty.
	# finally, everything is dumped to disk.
  
  
	# traindata given?
	doTraining = TRUE
	if (is.null(trainDataX) == FALSE) {
		# yes, we are given in memory data
		if (is.null(trainDataFile) == FALSE)
			stopf("Given a data frame as training data and specified a training file name. Confused. Stopping.")
			
		# we need to get a tempfile and dump our data into there
		trainDataFile = tempfile()
		if (verbose == TRUE)
			messagef("  Writing given data as %s", trainDataFile)
		write.matrix.csr(trainDataX, trainDataFile, trainDatay)
	} else {
		# no, we have no in memory data. so we need to create a data file 
		if (is.null(trainDataFile) == TRUE)
			doTraining = FALSE
		
		# ok, so we have a training data file, and everything is good.
	}	

	if (verbose == TRUE) 
		messagef("  Train Data is now in %s", trainDataFile)
	
	if (doTraining == TRUE)
		if (verbose == TRUE)
			messagef("  Will perform Training")

		
		
	# testdata given?
	doTesting = TRUE
	if (is.null(testDataX) == FALSE) {
		# yes, we are given in memory data
		if (is.null(testDataFile) == FALSE)
			stopf("Given a data frame as testing data and specified a testing file name. Confused. Stopping.")
			
		# we need to get a tempfile and dump our data into there
		testDataFile = tempfile()
		if (verbose == TRUE)
			messagef("  Writing given data as %s", testDataFile)
		write.matrix.csr(testDataX, testDataFile, testDatay)
	} else {
		# no, we have no in memory data. so we need to create a data file 
		if (is.null(testDataFile) == TRUE)
			doTesting = FALSE
		
		# ok, so we have a testing data file, and everything is good.
	}	

	if (verbose == TRUE) 
		messagef("  Test data is now in %s", testDataFile)
	
	if (doTesting == TRUE)
		if (verbose == TRUE)
			messagef("  Will perform Testing")


	# given us a model in memory makes only sense,
	# if the user does not want to train, but to test
	if ((is.null(model) == FALSE) && (doTraining == TRUE)) {
		stopf ("Cannot train, if given a memory model. Would need to discard that.")
	}

  

	results = list()
	if (doTraining == TRUE) {
		args = eval (trainingParameterCallBack( 
			trainDataFile = trainDataFile, 
			modelFile = modelFilePath,
			extraParameter = extraParameter,
			...))

		trainTime = microbenchmark(s <- system3(trainBinaryPath, args, verbose = verbose), times = 1L)$time / 1e9
	
		if (verbose == TRUE) 
			messagef("Training took %f seconds.", trainTime)
			
		results[["trainTime"]] = trainTime
	}

	
	if (doTesting == TRUE) {
	
		# if the user specified a model in memory, we first need to write that
		if (is.null(model) == FALSE) {
			writeModelCallBack (model, modelFilePath, verbose = verbose)
		}

		# TODO: add prediction as an option, so people can have them permanently on disk??
		predictionsFilePath = tempfile()
	
		# retrieve test parameters
		args = eval (testParameterCallBack( 
			testDataFile = testDataFile, 
			modelFile = modelFilePath, 
			predictionsFilePath = predictionsFilePath,
			...))
    
		testTime = microbenchmark(s <- system3(testBinaryPath, args, verbose = verbose), times = 1L)$time / 1e9
    
		if (verbose == TRUE) 
		messagef("Testing took %f seconds.", testTime);
    
		results[["testTime"]] = testTime
		results[["testError"]] = eval (extractInformationCallBack(output = s$output))
		
		# as testing was done, we want to read out the predictions
		predictions = predictionsCallBack(predictionsFilePath = predictionsFilePath, verbose = verbose)
		results[["predictions"]] = predictions
	}

	
	if (readModelFile == TRUE) {
		if (verbose == TRUE) 
			messagef( "Will read model back from %s", modelFilePath)
		model = readModelCallBack (modelFilePath = modelFilePath, verbose = verbose)
		results[["model"]] = model
	}
  
  return (results)   
}


# dummy, should be overwritten
extractInformationCallBack = function (output) {
  err = 1 - as.numeric(str_extract(output, "\\d+\\.?\\d*")) / 100
}


# dummies, must be overwritten
trainingParameterCallBack <- function (...) {}
testParameterCallBack <- function (...) {}
readModelCallBack <- function (...) {} 
writeModelCallBack <- function (...) {} 
predictionsCallBack <- function (...) {}

