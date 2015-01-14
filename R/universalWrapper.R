#!/usr/bin/Rscript  --vanilla 

# stupid R
loadThings <- function ()
{
  library(BBmisc)
  library(stringr)
  library(R.utils)
  library(microbenchmark)
  library(e1071)
  
  source ("./system3.R")
  source ("./subsampleData.R")
  source ("./models/readModel.R")
}
suppressMessages(loadThings())





# universalWrapper
#
# @param[in]    trainfile       file to read training data from
# @param[in]    testfile        file to read test data from
# @param[in]    cost            cost parameter C
# @param[in]    gamma           gamma parameter, note: RBF kernel used by pegasos is exp(-0.5 ...)
# @param[in]    epsilon         epsilon=precision parameter on loss
# @param[in]    extraParameter  extra parameters for solver
# @param[in]    bindir          relativ path to the binaries, defaults to.. default.
# @param[in]    modelFile       path to model, defaults to a temporary file (given by R)
# @param[in]    wallTime        walltime in minutes. solver will quit roughly at this time (-5 min spare),
#                               default = -1, no walltime
# @param[in]    primalTime      time in minutes to compute current primal value. default = -1, no computation.
#

universalWrapper = function(
	# data
	trainDataFile = NA,
	testDataFile = NA,
	trainDataX = NA, 
	trainDatay = NA, 
	testDataX = NA, 
	testDatay = NA, 

	# full path to executable
	trainBinaryPath = NA,
	testBinaryPath = NA,
	
	# rest
	method = NA,
	extraParameter = "",
	modelFile = NA,
	verbose = FALSE,

	# call backs
	trainingParameterCallBack = trainingParameterCallBack(),
	testParameterCallBack = testParameterCallBack(),
	extractInformationCallBack = extractInformationCallBack(),
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

	
	# TODO: really?
	# need to unpack the epsilon from the ... parameters
	eP = list(...)
	epsilon = 0
	if (!is.null (eP[["epsilon"]])) {
		epsilon = eP[["epsilon"]]
	}
	
	# TODO: sanity checks for parameter
	if (is.na (method) == TRUE) {
		stopf("No method name is given, this should never happen.")
	}
	
	readModelFile = FALSE
	if (is.na(modelFile) == TRUE) {
		# user did not specify modelfile
		# so we will write the model in a temp file
		# and read it back after training
		readModelFile = TRUE
		modelFile = tempfile()
	}

	
	# take care of data. if testdata or traindata is given,
	# the corresponding filename must be empty.
	# finally, everything is dumped to disk.
  
  
	# traindata given?
	doTraining = TRUE
	if (is.na(trainDataX) == FALSE) {
		# yes, we are given in memory data
		if (is.na(trainDataFile) == FALSE)
			stopf("Given a data frame as training data and specified a training file name. Confused. Stopping.")
			
		# we need to get a tempfile and dump our data into there
		trainDataFile = tempfile()
		if (verbose == TRUE)
			messagef("  Writing given data as %s", trainDataFile)
		write.matrix.csr(trainDataX, trainDataFile, trainDatay)
	} else {
		# no, we have no in memory data. so we need to create a data file 
		if (is.na(trainDataFile) == TRUE)
			doTraining = FALSE
		
		# ok, so we have a training data file, and everything is good.
	}	

	if (verbose == TRUE) 
		messagef("  Train Data is now in %s", trainDataFile)
	
	if (doTraining == TRUE)
		messagef("  Will TRAIN")

		
		
	# testdata given?
	doTesting = TRUE
	if (is.na(testDataX) == FALSE) {
		# yes, we are given in memory data
		if (is.na(testDataFile) == FALSE)
			stopf("Given a data frame as testing data and specified a testing file name. Confused. Stopping.")
			
		# we need to get a tempfile and dump our data into there
		testDataFile = tempfile()
		if (verbose == TRUE)
			messagef("  Writing given data as %s", testDataFile)
		write.matrix.csr(testDataX, testDataFile, testDatay)
	} else {
		# no, we have no in memory data. so we need to create a data file 
		if (is.na(testDataFile) == TRUE)
			doTesting = FALSE
		
		# ok, so we have a testing data file, and everything is good.
	}	

	if (verbose == TRUE) 
		messagef("  Test data is now in %s", testDataFile)
	
	if (doTesting == TRUE)
		messagef("  Will TEST")


		
		
	# make sure epsilon works (technical problem)
	epsilon = as.numeric(as.character(epsilon))
  

	results = list()
	if (doTraining == TRUE) {
		args = eval (trainingParameterCallBack( 
			trainDataFile = trainDataFile, 
			modelFile = modelFile,
			extraParameter = extraParameter,
			...))

		trainTime = microbenchmark(s <- system3(trainBinaryPath, args, verbose = verbose), times = 1L)$time / 1e9
	
		if (verbose == TRUE) 
			messagef("Training took %f seconds.", trainTime)
			
		results[["trainTime"]] = trainTime
	}

	
	if (doTesting == TRUE) {
		# retrieve test parameters
		args = eval (testParameterCallBack( 
			testDataFile = testDataFile, 
			modelFile = modelFile, ...))
    
		testTime = microbenchmark(s <- system3(testBinaryPath, args, verbose = verbose), times = 1L)$time / 1e9
    
		if (verbose == TRUE) 
		messagef("Testing took %f seconds.", testTime);
    
		results[["testTime"]] = testTime
		results[["testError"]] = eval (extractInformationCallBack(output = s$output))
	}

	
	if (readModelFile == TRUE) {
		model = readModel (modelFilePath = modelFile, method = method, verbose = verbose)
		results[["model"]] = model
	}
  
  return (results)   
}


# dummy, should be overwritten
extractInformationCallBack = function (output) {
  err = 1 - as.numeric(str_extract(output, "\\d+\\.?\\d*")) / 100
}


# dummy, must be overwritten
trainingParameterCallBack <- function (...) {}


# dummy, must be overwritten
testParameterCallBack <- function (...) {}

