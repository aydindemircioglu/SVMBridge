#!/usr/bin/Rscript  --vanilla 


  
  

# evalPegasos
# @param[in]    trainfile       file to read training data from
# @param[in]    testfile        file to read test data from
# @param[in]    cost            cost parameter C
# @param[in]    gamma           gamma parameter, note: RBF kernel used by pegasos is exp(-0.5 ...)
# @param[in]    epochs          number of epochs to run pegasos
# @param[in]    bindir          relativ path to the binaries, defaults to default.
# @param[in]    modelFile       path to model, defaults to a temporary file (given by R)


createTrainingArguments.Pegasos = function (trainfile = "",
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




createTestArguments.Pegasos = function (testfile = "",
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



extractTrainingInfo.Pegasos = function (output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}

#NEW
extractTestInfo.Pegasos = function (output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}

#NEW
readModel.Pegasos = function (modelFile = './model', verbose = FALSE) {
		NextMethod (modelFile = modelFile, verbose = verbose)
	}
	
#NEW
writeModel.Pegasos = function (model = NA, modelFile = "./model", verbose = FALSE) {
		NextMethod (model = model, modelFile = modelFile, verbose = verbose)
	}

#NEW
readPredictions.Pegasos = function (predictionsFilePath = "", verbose = FALSE) {
		p = NextMethod (predictionsFilePath = predictionsFilePath, verbose = verbose)
		return (p)
	}

	

findSoftware.Pegasos = function(x, searchPath = "./", verbose = FALSE) {
		# short way without verbose messages
		x$trainBinaryPath  = findBinary (searchPath, "^budgetedsvm-train$", "Usage: svm-train .options. training_set_file .model_file.", verbose = verbose)
	}
	

