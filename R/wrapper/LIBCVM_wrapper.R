#!/usr/bin/Rscript  --vanilla 

source ("./universalWrapper.R")

library(BBmisc)




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



LibCVMTrainBinary <- function() {
    return ("svm-train")
}


LibCVMTestBinary <- function() {
    return ("svm-predict")
}


LibCVMBinDir <- function() {
    return ("software/libCVM/bin/")
}



# stupid R check for pythons cool "name == __main__"
if (length(sys.frames()) == 0) 
{
  #  file = "software/libSVM/src/heart_scale"
#    file = "datasets/australian/australian.combined.scaled"
 #   file = "datasets/arthrosis/arthrosis.combined.scaled"
    file = "datasets/poker/poker.combined.scaled"

#    file = getMCData("dna")

    err = evalLibBVM(file, file, cost = 1, gamma = 1.0496346230004575, 
		epsilon = 0.00000001, 
        primalTime = -1,
#        subsamplingRate = 0.25,
        wallTime = 30,
        verbose = TRUE,
        modelFile = "~/libbvm.model",
        computePrimal = FALSE
    )  

    messagef("Error Rate: %s", err$err) 
    messagef("Dual: %s", err$dual) 
    messagef("Primal: %s", err$primal)

    
    err = evalLibCVM(file, file, cost = 1, gamma = 1.0496346230004575, 
		epsilon = 0.00000001, 
        primalTime = -1,
        wallTime = 30,
        verbose = TRUE,
        modelFile = "~/libcvm.model",
        computePrimal = FALSE
    )  

    messagef("Error Rate: %s", err$err) 
    messagef("Dual: %s", err$dual) 
    messagef("Primal: %s", err$primal)
    
}

