#!/usr/bin/Rscript  --vanilla 



evalLASVM = function(...)  {   
    universalWrapper (
        modelName = "LASVM",
        trainingParameterCallBack = LASVMTrainingParameterCallBack,
        testParameterCallBack = LASVMTestParameterCallBack,
        extractInformationCallBack  = LASVMExtractInformationCallBack,
        trainBinary = LASVMTrainBinary(),
        testBinary = LASVMTestBinary (),
        bindir = LASVMBinDir(),
        ...
    );
}



LASVMTrainingParameterCallBack = function (trainfile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            kernelCacheSize = 1024,
                                            cost = 1, 
                                            useBias = FALSE,
                                            gamma = 1,
                                            epochs = 1,
                                            epsilon = 0.001, ...) {

    # count training examples
    N = countLines(trainfile)

    biasParameter = "-b 0"
    if (useBias == TRUE)
        biasParameter = "-b 1"

    args = c(
        sprintf("-m %d", kernelCacheSize), # in MB 
        biasParameter,
        sprintf("-g %.16f", gamma),
        sprintf("-c %.16f", cost), 
        sprintf("-e %.16f", epsilon),
        sprintf("-p %.16f", epochs),
        extraParameter,
        trainfile,
        modelFile
    )

    return (args)
}



LASVMTestParameterCallBack = function (testfile = "",
                                        modelFile = "", ...) {
    args = c(
        testfile,
        modelFile,
        "/dev/null"                     # outfile, not needed
    )
    
    return (args)
}



LASVMExtractInformationCallBack = function (output) {

    # maybe not the best way to grep the string
    pattern <- "accuracy= (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
}
  


LASVMTrainBinary <- function() {
    return ("la_svm")
}


LASVMTestBinary <- function() {
    return ("la_test")
}


LASVMBinDir <- function() {
    return ("software/LASVM/bin/")
}



# stupid R check for pythons cool "name == __main__"
if (length(sys.frames()) == 0) 
{
    file = getData ("australian")
    testfile = file

    err = evalLASVM(trainfile = file, 
        testfile = testfile, cost = 1, gamma = 1, epsilon = 0.001, epochs = 1,
        primalTime = -1,
        wallTime = -1,
        verbose = TRUE,
        modelFile = "~/lasvm.model",
        computePrimal = TRUE
    )  

    messagef("Accuracy: %s", err$err) 
    messagef("Dual: %s", err$dual) 
    messagef("Primal: %s", err$primal)
}

