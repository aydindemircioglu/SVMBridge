# stupid R
loadThings <- function ()
{
    source ("./includeWrapper.R")
}
suppressMessages(loadThings())



executeSVMSolver <- function (cost = 0.1, gamma = 1.0, examplePath, modelName, 
    stdOutput = TRUE, 
    modelBasename = "",
    testFile = "",
    ...)
{
    returnValue = list()

    # extract ...
    eP = list(...)
    epochs = 8
    if (!is.null (eP[["epochs"]])) {
        epochs = eP[["epochs"]]
    }

    # extract ...
    verbose = FALSE
    if (!is.null (eP[["verbose"]])) {
        verbose = eP[["verbose"]]
    }
    
    # TODO: fix me, use ... directly
    k = 500
    if (!is.null (eP[["k"]])) {
        k = eP[["k"]]
    }
    
    # TODO: fix me, use ... directly
    budget = 500
    if (!is.null (eP[["budget"]])) {
        budget = eP[["budget"]]
    }

    # for now: fake it.
    rank = 120
    if (!is.null (eP[["rank"]])) {
        rank = eP[["rank"]]
    }
    
    method = "Nystrom"
    if (!is.null (eP[["method"]])) {
        method = eP[["method"]]
    }

    subsamplingRate = -1
    if (!is.null (eP[["subsamplingRate"]])) {
        subsamplingRate = eP[["subsamplingRate"]]
    }

    
    if (modelBasename == "") {
        modelBasename = paste (modelName, "_", basename(examplePath), sep = "")
    }
    
    # compute model
    #examplePath = basename(examplePath)
    modelFile = paste("software/helpers/models/test/", modelBasename, ".model", sep = "")
    messagef ("     \t\tExecuting model %s with trainings data from %s and output %s", modelName, examplePath, modelFile)
      
    # find name for prediction output
    predictionOutput = paste ("software/helpers/models/test/", modelBasename, ".predictionOutput", sep ="")
    messagef ("     \t\tSaving prediction output as %s ", predictionOutput)
   
    # call the model, TODO: make this somehow more intuitive, especially for API changes..
    err = 140234234
    if (modelName == "BSGD") {
      returnValue = evalBSGD(examplePath, examplePath, cost = cost, gamma = gamma, budget = budget, epochs = epochs,
                        computePrimal = FALSE, 
                        extraParameter = "",
                        verbose = verbose,
                        modelFile = modelFile, 
                        predictionOutput = predictionOutput,
                        subsamplingRate = subsamplingRate)
    }
    
    if (modelName == "Pegasos") {
      returnValue = evalPegasos(examplePath, examplePath, cost = cost, gamma = gamma, epochs = epochs,
                        verbose = verbose,
                       extraParameter = "", #-v 1",
                       computePrimal = FALSE,
                       modelFile = modelFile, 
                       subsamplingRate = subsamplingRate,
                       predictionOutput = predictionOutput )
    }
    
    if (modelName == "LLSVM") {
      returnValue = evalLLSVM(examplePath, examplePath, cost = cost, gamma = gamma, rank = rank,
                        verbose = verbose,
                       subsamplingRate = subsamplingRate,
                       modelFile = modelFile, predictionOutput = predictionOutput )
    }
    
    if (modelName == "LASVM") {
      returnValue = evalLASVM(examplePath, examplePath, cost = cost, gamma = gamma, epsilon = 0.001, epochs = 128,
                       subsamplingRate = subsamplingRate,
                      modelFile = modelFile)
    }
    
    if (modelName == "libBVM") {
      returnValue = evalLibBVM(examplePath, examplePath, cost = cost, gamma = gamma, epsilon = 0.001, 
                       subsamplingRate = subsamplingRate,
                      modelFile = modelFile)
    }
    
    if (modelName == "libCVM") {
      returnValue = evalLibCVM(examplePath, examplePath, cost = cost, gamma = gamma, epsilon = 0.001, 
                       subsamplingRate = subsamplingRate,
                      modelFile = modelFile)
    }
    
    if (modelName == "libSVM") {
      returnValue= evalLibSVM(examplePath, examplePath, cost = cost, gamma = gamma, epsilon = 0.001, 
                        verbose = verbose,
                       subsamplingRate = subsamplingRate,
                       computePrimal = FALSE,
                       modelFile = modelFile)
    }
    
    if (modelName == "AESVM") {
      returnValue= evalAESVM(examplePath, examplePath, cost = cost, gamma = gamma, epsilon = 0.001, 
                        verbose = verbose,
                       subsamplingRate = subsamplingRate,
                       computePrimal = FALSE,
                       modelFile = modelFile)
    }

    if (modelName == "SharkSVMPegasos") {
      returnValue= evalSharkSVMPegasos(examplePath, examplePath, cost = cost, gamma = gamma, epochs = epochs,
                        useBias = FALSE,
                       subsamplingRate = subsamplingRate,
                        verbose = verbose,
                        computePrimal = FALSE,
                        modelFile = modelFile)
    }

    if (modelName == "SharkSVMSMO") {
      returnValue= evalSharkSVMSMO(examplePath, examplePath, cost = cost, gamma = gamma, epochs = epochs,
                       extraParameter = "",
                       verbose = verbose,
                       subsamplingRate = subsamplingRate,
                       computePrimal = FALSE,
                       modelFile = modelFile)
    }
    
    if (modelName == "SVMperf") {
      returnValue = evalSVMperf(examplePath, examplePath, cost = cost, gamma = gamma, epsilon = 0.1, 
                        k = k,
#                        extraParameter = "-w 3 --t 0 --i 2",
                       subsamplingRate = subsamplingRate,
                        method = method,
                        modelFile = modelFile, 
                        verbose = verbose)
    }

    err = returnValue$err
    
    if (is.null(err)) {
      err = 0
    }
    
    if (err == 140234234) {
      stop( paste("ERROR:\t\tUnknown model", modelName, "selected."))
    }
    
    returnValue$err = err
    returnValue$modelFile = modelFile
    returnValue$predictionOutput = predictionOutput
    return (returnValue)
}
