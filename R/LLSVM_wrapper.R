#!/usr/bin/Rscript  --vanilla 





# evalLLSVM
# @param[in]    trainfile       file to read training data from
# @param[in]    testfile        file to read test data from
# @param[in]    cost            cost parameter C
# @param[in]    gamma           gamma parameter, note: RBF kernel used by pegasos is exp(-0.5 ...)
# @param[in]    rank            number of landmarks
# @param[in]    bindir          relativ path to the binaries, defaults to default.
# @param[in]    modelFile       path to model, defaults to a temporary file (given by R)



createTrainingArguments.LLSVM = function (trainfile = "",
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



createTestArguments.LLSVM = function (testfile = "",
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



extractTrainingInfo.LLSVM = function (output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}

#NEW
extractTestInfo.LLSVM = function (output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}

#NEW
readModel.LLSVM= function (modelFile = './model', verbose = FALSE) {
		NextMethod (modelFile = modelFile, verbose = verbose)
	}
	
#NEW
writeModel.LLSVM = function (model = NA, modelFile = "./model", verbose = FALSE) {
		NextMethod (model = model, modelFile = modelFile, verbose = verbose)
	}
	
#NEW
readPredictions.LLSVM = function (predictionsFilePath = "", verbose = FALSE) {
		p = NextMethod (predictionsFilePath = predictionsFilePath, verbose = verbose)
		return (p)
	}
	
#NEW
findSoftware.LLSVM = function(x, searchPath = "./", verbose = FALSE) {
		# short way without verbose messages
		x$trainBinaryPath  = findBinary (searchPath, "^budgetedsvm-train$", "Usage: svm-train .options. training_set_file .model_file.", verbose = verbose)
	}
