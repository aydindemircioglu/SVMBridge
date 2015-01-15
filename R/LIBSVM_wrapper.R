#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		SVMperf_wrapper.R
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
 

#' LIBSVMTrainBinary 
#'
#' return full path of training binary.
#

LIBSVMTrainBinary <- function() {
    return ("svm-train")
}



#' LIBSVMTestBinary 
#'
#' return full path of test binary.
#

LIBSVMTestBinary <- function() {
    return ("svm-predict")
}



#' LIBSVMTrainBinaryOutputPattern
#'
#' return pattern that identifies the training binary.
#

LIBSVMTrainBinaryOutputPattern <- function() {
    return ('Usage: svm-train .options. training_set_file .model_file.')
}



#' LIBSVMTestBinaryOutputPattern
#'
#' return pattern that identifies the test binary.
#

LIBSVMTestBinaryOutputPattern <- function() {
    return ('for one-class SVM only 0 is supported')
}


	
#' evalLIBSVM
#'
#' @param     ...		parameters that will be passed on training and/or test callbacks
#

evalLIBSVM = function(...)  {   

	parameterList = list(..., 
		trainingParameterCallBack = LIBSVMTrainingParameterCallBack,
        testParameterCallBack = LIBSVMTestParameterCallBack,
        extractInformationCallBack  = LIBSVMExtractInformationCallBack,
        readModelCallBack = LIBSVMReadModelCallBack,
        writeModelCallBack = LIBSVMWriteModelCallBack,
        predictionsCallBack = LIBSVMPredictionsCallBack)
    
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
    predictionsFilePath = "",
    ...) {
    args = c(
        testDataFile,
        modelFile,
        predictionsFilePath 
    )
    
    return (args)
}



LIBSVMExtractInformationCallBack = function (output) {

    # compute error
    pattern <- ".*Accuracy =\\s*(\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100

    return(err)
}



LIBSVMReadModelCallBack <- function (modelFilePath = "./model", verbose = FALSE)
{
    # open connection
    con  <- file(modelFilePath, open = "r")

    while ((oneLine <- readLines(con, n = 1, warn = FALSE)) != "SV") {
        # gamma value
        if (grepl("gamma", oneLine) == TRUE) {
            pattern <- "gamma (.*)"
            gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        }  
      
        # rho/bias
        if (grepl("rho", oneLine) == TRUE) {
            pattern <- "rho (.*)"
        bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        }
        
        # order of labels
        if (grepl("label", oneLine) == TRUE) {
            pattern <- "label (.*)"
            order = (sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        
            if ((order != "1 -1") && (order != "-1 1")) {
                stop ("Label ordering %s is unknown!", order)
            }
            # LABEL ORDERING IS NOT USED for libsvm!
        }  
    }
  
  
	# read and interprete data 
	# basically all data is sparse data format, but the data around this differs
	svmatrix = readSparseFormat(con)

  
	# add header information
	svmatrix$gamma = gamma
	svmatrix$bias = bias
	svmatrix$modelname = "LIBSVM"
	
	# close connection
	close(con)
	
	# return
	return (svmatrix)
}



# dummy for now
LIBSVMWriteModelCallBack <- function (model = NA, modelFilePath = "./model", verbose = FALSE) {
	if (verbose == TRUE) {
		messagef ("Writing SVM Model..")
	}
	
	# FIXME: label order
	# TODO: support multiclass
	model$nrclass = 2
	posSV = sum(model$a > 0)
	negSV = sum(model$a < 0)
    # open connection
    modelFileHandle <- file(modelFilePath, open = "w+")
	writeLines(paste ("svm_type c_svc", sep = ""), modelFileHandle )
	writeLines(paste ("kernel_type", "rbf", sep = " "), modelFileHandle )
	writeLines(paste ("gamma", model$gamma, sep = " "), modelFileHandle )
	writeLines(paste ("nr_class", model$nrclass, sep = " "), modelFileHandle )
	writeLines(paste ("total_sv", length(model$a), sep = " "), modelFileHandle )
	writeLines(paste ("rho", model$bias, sep = " "), modelFileHandle )
	writeLines(paste ("label 1 -1", sep = " "), modelFileHandle )
	writeLines(paste ("nr_sv", posSV, negSV, sep = " "), modelFileHandle )
	writeLines(paste ("SV", sep = ""), modelFileHandle )

	# basically all data is sparse data format, but the data around this differs
	svmatrix = dumpSparseFormat(model$a, model$X)
	writeLines(svmatrix, modelFileHandle, sep = "" )
	
	# close connection
	close(modelFileHandle)
}
 

#
# @param[in]	predictionsFile		file to read predictions from
# @return		array consisting of predictions
#

LIBSVMPredictionsCallBack <- function (predictionsFilePath = "", verbose = FALSE) {
    # open connection
    con  <- file(predictionsFilePath, open = "r")

    predictions = c()
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
		predictions = c(predictions, as.numeric(oneLine))
    }
    
	if (verbose == TRUE) {
		print(predictions)
	}
			
	close (con)
	
    return (predictions)
}
