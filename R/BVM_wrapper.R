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
  
#source ("./universalWrapper.R")

createTrainingArguments.BVM = function (trainfile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            kernelCacheSize = 1024,
                                            cost = 1, 
                                            gamma = 1, 
                                            epsilon = 0.001, ...) {

    args = c(
        "-s 6",                         # CVM = 6, BVM = 9
        "-t 2",
        sprintf("-c %.16f", cost), 
        sprintf("-m %d", kernelCacheSize), # in MB 
        sprintf("-g %.16f", gamma),
        sprintf("-e %.16f", epsilon),
        extraParameter,
        trainfile,
        modelFile
    )

    return (args)
}



createTestArguments.BVM = function (testfile = "",
                                        modelFile = "", ...) {
    args = c(
        testfile,
        modelFile,
        "/dev/null"                     # outfile, not needed
    )
    
    return (args)
}



extractTrainingInfo.BVM = function (output) {

    # maybe not the best way to grep the string
    pattern <- "Accuracy = (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}


#NEW
extractTestInfo.BVM = function (output) {

    # maybe not the best way to grep the string
    pattern <- "Accuracy = (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}



readModel.BVM <- function (modelFilePath = "./model", verbose = FALSE) {
	LIBSVMReadModelCallBack (modelFilePath = modelFilePath, verbose = verbose)
}



writeModel.BVM <- function (model = NA, modelFilePath = "./model", verbose = FALSE) {
	LIBSVMWriteModelCallBack (model = model, modelFilePath = modelFilePath, verbose = verbose)
}
 

 
#
# @param[in]	predictionsFile		file to read predictions from
# @return		array consisting of predictions
#

readPredictions.BVM <- function (predictionsFilePath = "", verbose = FALSE) {
	return (LIBSVMPredictionsCallBack (predictionsFilePath = predictionsFilePath, verbose = verbose))
}

#NEW
findSoftware.BVM = function(x, searchPath = "./", verbose = FALSE) {
		# short way without verbose messages
		x$trainBinaryPath  = findBinary (searchPath, "^svm-train$", "Usage: svm-train .options. training_set_file .model_file.", verbose = verbose)
	}
