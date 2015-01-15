#!/usr/bin/Rscript  --vanilla 


  
  

# evalPegasos
# @param[in]    trainfile       file to read training data from
# @param[in]    testfile        file to read test data from
# @param[in]    cost            cost parameter C
# @param[in]    gamma           gamma parameter, note: RBF kernel used by pegasos is exp(-0.5 ...)
# @param[in]    epochs          number of epochs to run pegasos
# @param[in]    bindir          relativ path to the binaries, defaults to default.
# @param[in]    modelFile       path to model, defaults to a temporary file (given by R)
evalPegasos = function(...)  {   
    universalWrapper (
        modelName = "Pegasos",
        trainingParameterCallBack = PegasosTrainingParameterCallBack,
        testParameterCallBack = PegasosTestParameterCallBack,
        extractInformationCallBack = PegasosExtractInformationCallBack,
        trainBinary = PegasosTrainBinary(),
        testBinary = PegasosTestBinary (),
        bindir = PegasosBinDir(),
        ...
    );
}



PegasosTrainingParameterCallBack = function (trainfile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            cost = 1, 
                                            gamma = 1, 
                                            epochs = 1, ...) {

    n = countLines(trainfile)

    # we make pegasos non linear by using budgeted SVM with an
    # excessively high budget-- i.e. n+1.
    
    # arguments for training
    args = c(
        "-A 4",
        "-r 0",
        "-m 2", # this makes sure that an error is thrown whenever the reduce-budget routine is called. makes it extra safe.
        sprintf("-a %d", floor(primalTime)),
        sprintf("-l %d", floor(wallTime)),         # rbf kernel
        sprintf("-B %.16f", n + 1 ),
        sprintf("-L %.16f", (1.0 / (n * cost))),
        sprintf("-e %.16f", epochs ),
        sprintf("-g %.16f", 2 * gamma),
        extraParameter,
        trainfile,
        modelFile
    )
  
    return (args)
}




PegasosTestParameterCallBack = function (testfile = "",
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



PegasosExtractInformationCallBack = function (output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}


PegasosTrainBinary <- function() {
    return ("budgetedsvm-train")
}


PegasosTestBinary <- function() {
    return ("budgetedsvm-predict")
}


PegasosBinDir <- function() {
    return ("software/BudgetedSVM/bin/")
}


