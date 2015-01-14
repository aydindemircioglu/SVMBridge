#!/usr/bin/Rscript  --vanilla 

source ("./universalWrapper.R")

library(BBmisc)



evalLIBSVM = function(...)  {   

	parameterList = list(..., 
		trainingParameterCallBack = LIBSVMTrainingParameterCallBack,
        testParameterCallBack = LIBSVMTestParameterCallBack,
        extractInformationCallBack  = LIBSVMExtractInformationCallBack)
    
    # we need to overwrite the template paths
    if (is.null(getOption("SVMBridge.LIBSVM.trainBinary")) == TRUE) {
		parameterList[["trainBinaryPath"]] = LIBSVMTrainBinary()
	} else {
		parameterList[["trainBinaryPath"]] = getOption("SVMBridge.LIBSVM.trainBinary")
	}

    if (is.null(getOption("SVMBridge.LIBSVM.testBinary")) == TRUE) {
		parameterList[["tesBinaryPath"]] = LIBSVMTestBinary()
	} else {
		parameterList[["testBinaryPath"]] = getOption("SVMBridge.LIBSVM.testBinary")
	}

	obj = do.call(universalWrapper, parameterList)
}



LIBSVMTrainingParameterCallBack = function (
	trainDataFile = "",
    modelFile = "",
    extraParameter = "",
    trainBinaryPath  = "",
    primalTime = 10, 
    wallTime = 8*60,
    kernelCacheSize = 1024,
    cost = 1, 
    gamma = 1, 
    epsilon = 0.001, 
    ...) {

    # --- take care of bias parameter

    args = c(
        "-s 0",                         # c classification
        "-t 2",
        sprintf("-m %d", kernelCacheSize), # in MB 
        sprintf("-c %.16f", cost),         # rbf kernel
        sprintf("-g %.16f", gamma),        # gamma
        sprintf("-e %.16f", epsilon),      # epsilon tolerance
        extraParameter,
        trainDataFile,
        modelFile
    )

    return (args)
}



LIBSVMTestParameterCallBack = function (
	testDataFile = "",
    modelFile = "", 
    ...) {
    args = c(
        testDataFile,
        modelFile,
        "/dev/null"                     # outfile, not needed
    )
    
    return (args)
}



LIBSVMExtractInformationCallBack = function (output) {

    # compute error
    pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100

    return(err)
}



LIBSVMTrainBinary <- function() {
    return ("./software/LIBSVM/bin/svm-train")
}


LIBSVMTestBinary <- function() {
    return ("./software/LIBSVM/bin/svm-predict")
}



