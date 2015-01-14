#!/usr/bin/Rscript  --vanilla 

source ("./universalWrapper.R")

library(BBmisc)


# add functions to allow for searching the binaries
LIBCVMTrainBinary <- function() {
    return ("svm-train")
}


LIBCVMTestBinary <- function() {
    return ("svm-predict")
}


LIBCVMTrainBinaryOutputPattern <- function() {
    return ("outputPattern = '6 -- CVM \\(sqr. hinge-loss")
}


LIBCVMTestBinaryOutputPattern <- function() {
    return ("bvm-predict")
}


evalLibCVM = function(...)  {   
    universalWrapper (
        modelName = "libCVM",
        trainingParameterCallBack = LibCVMTrainingParameterCallBack,
        testParameterCallBack = LibCVMTestParameterCallBack,
        extractInformationCallBack  = LibCVMExtractInformationCallBack,
        trainBinary = LibCVMTrainBinary(),
        testBinary = LibCVMTestBinary (),
        bindir = LibCVMBinDir(),
        ...
    );
}



evalLibBVM = function(...)  {   
    universalWrapper (
        modelName = "libBVM",
        trainingParameterCallBack = LibBVMTrainingParameterCallBack,
        testParameterCallBack = LibCVMTestParameterCallBack,
        extractInformationCallBack  = LibCVMExtractInformationCallBack,
        trainBinary = LibCVMTrainBinary(),
        testBinary = LibCVMTestBinary (),
        bindir = LibCVMBinDir(),
        ...
    );
}



LibCVMTrainingParameterCallBack = function (trainfile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            kernelCacheSize = 1024,
                                            cost = 1, 
                                            gamma = 1, 
                                            epsilon = 0.001, ...) {

    # ---  take care of primal/wall time, will not be added if its turned off. 
    primalTimeParameter =  sprintf("-a %d", floor(primalTime))

    if (primalTime == -1)
        primalTimeParameter = ""

    wallTimeParameter =  sprintf("-l %d", floor(wallTime))

    if (wallTime == -1)
        wallTimeParameter = ""

    args = c(
        "-s 6",                         # CVM = 6, BVM = 9
        "-t 2",
        sprintf("-c %.16f", cost), 
        primalTimeParameter,
        wallTimeParameter,
        sprintf("-m %d", kernelCacheSize), # in MB 
        sprintf("-g %.16f", gamma),
        sprintf("-e %.16f", epsilon),
        extraParameter,
        trainfile,
        modelFile
    )

    return (args)
}



LibBVMTrainingParameterCallBack = function (trainfile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            cost = 1, 
                                            kernelCacheSize = 1024,
                                            gamma = 1, 
                                            epsilon = 0.001, ...) {

    # ---  take care of primal/wall time, will not be added if its turned off. 
    primalTimeParameter =  sprintf("-a %d", floor(primalTime))

    if (primalTime == -1)
        primalTimeParameter = ""

    wallTimeParameter =  sprintf("-l %d", floor(wallTime))

    if (wallTime == -1)
        wallTimeParameter = ""

    args = c(
        "-s 9",                         # CVM = 6, BVM = 9
        "-t 2",
        sprintf("-c %.16f", cost), 
        primalTimeParameter,
        wallTimeParameter,
        sprintf("-m %d", kernelCacheSize), # in MB 
        sprintf("-g %.16f", gamma),
        sprintf("-e %.16f", epsilon),
        extraParameter,
        trainfile,
        modelFile
    )

    return (args)
}



LibCVMTestParameterCallBack = function (testfile = "",
                                        modelFile = "", ...) {
    args = c(
        testfile,
        modelFile,
        "/dev/null"                     # outfile, not needed
    )
    
    return (args)
}



LibCVMExtractInformationCallBack = function (output) {

    # maybe not the best way to grep the string
    pattern <- "Accuracy = (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}



