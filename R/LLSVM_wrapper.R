#!/usr/bin/Rscript  --vanilla 





# evalLLSVM
# @param[in]    trainfile       file to read training data from
# @param[in]    testfile        file to read test data from
# @param[in]    cost            cost parameter C
# @param[in]    gamma           gamma parameter, note: RBF kernel used by pegasos is exp(-0.5 ...)
# @param[in]    rank            number of landmarks
# @param[in]    bindir          relativ path to the binaries, defaults to default.
# @param[in]    modelFile       path to model, defaults to a temporary file (given by R)
evalLLSVM = function(...)  {   
    universalWrapper (
        modelName = "LLSVM",
        trainingParameterCallBack = LLSVMTrainingParameterCallBack,
        testParameterCallBack = BudgetedSVMTestParameterCallBack,
        extractInformationCallBack = BudgetedSVMExtractInformationCallBack,
        trainBinary = BudgetedSVMTrainBinary(),
        testBinary = BudgetedSVMTestBinary (),
        bindir = BudgetedSVMBinDir(),
        ...
    );
}



  
  

  

LLSVMTrainingParameterCallBack = function (trainfile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            cost = 1, 
                                            gamma = 1, 
                                            rank = 128, ...) {
    # ---- compute general things
    n = countLines(trainfile)

    # ---- sanity checks
    if(n < rank)
        stop("Rank must not be greater or equal to size of dataset")

    # ---- training
    args = c(
        "-A 3",
        "-r 0",
        sprintf("-B %.16f", rank),
        sprintf("-L %.16f", (1.0 / (n * cost))), 
        sprintf("-g %.16f", 2 * gamma),
        extraParameter,
        trainfile,
        modelFile
    )

    return (args)
}



BudgetedSVMTestParameterCallBack = function (testfile = "",
                                        modelFile = "", 
                                        predictionOutput = "/dev/null", ...) {
    args = c(
        "-v 1",
#        "-o 1", only works for BSGD for now
        testfile,
        modelFile,
        predictionOutput 
    )
  
    return (args)
}



BudgetedSVMExtractInformationCallBack = function (output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}


BudgetedSVMTrainBinary <- function() {
    return ("budgetedsvm-train")
}


BudgetedSVMTestBinary <- function() {
    return ("budgetedsvm-predict")
}


BudgetedSVMBinDir <- function() {
    return ("software/BudgetedSVM/bin/")
}


BudgetedSVMMODBinDir <- function() {
    return ("software/BudgetedSVM/bin.org/")
}
