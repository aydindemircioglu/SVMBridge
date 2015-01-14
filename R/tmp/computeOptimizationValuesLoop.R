#!/usr/bin/Rscript  --vanilla 

# stupid R
loadThings <- function ()
{
  library(stringr)
  library(rbenchmark)
  library(BBmisc)
  library(plyr)
  library(e1071)
  library(SparseM) 

  source ("software/helpers/models/computeOptimizationValuesLibSVM.R")
  source ("software/helpers/models/computeOptimizationValuesSVMperf.R")
  source ("software/helpers/models/computeOptimizationValuesSharkSVM.R")
  source ("software/helpers/models/computeOptimizationValuesBSGD.R")
  source ("software/helpers/models/computeOptimizationValuesLLSVM.R")
}
suppressMessages(loadThings())



# compute sum of alpha coeefficients
computeSumOfAlphas <- function(modelData)
{
    s = list ()
    for (l in as.numeric(levels(factor(modelData$X)))) {
      s[l] = (sum (modelData$a * (modelData$X == as.numeric((levels(factor(modelData$X))))[l])))
    }

    return (s)
}



computeOptimizationValues <- function (modelData = "", 
    modelName = "", 
    trainingData = "", 
    data = -1,
    predictionOutput = "",
    verbose = FALSE)
{

# compute value with the specific model
    if ((modelName == "libSVM") | (modelName == "LASVM") | (modelName == "libBVM") |  (modelName == "libCVM") ) {
      values = computeOptimizationValuesLibSVM (modelData, 
        trainingData, 
        data = data, 
        predictionOutput = predictionOutput,
        verbose = verbose)
    }

    # SharkSVM has negativ bias. other than libSVM. wtf?...
    if ((modelName == "SharkSVM") ) {
      values = computeOptimizationValuesSharkSVM (modelData, 
        trainingData, 
        predictionOutput = predictionOutput,
        data = data, 
        verbose = verbose)
    }
    
    # NOTE: LIBSVM IS USED!
    if ((modelName == "SVMperf")  ) {
      values = computeOptimizationValuesLibSVM (modelData, 
        trainingData, 
        data = data, 
        predictionOutput = predictionOutput,
        verbose = verbose)
#      values = computeOptimizationValuesSVMperf (modelData, trainingData)
    }
    
    
    if ((modelName == "LLSVM") ) {
      values = computeOptimizationValuesLLSVM (modelData, 
            trainingData, 
            data = data, 
            predictionOutput = predictionOutput,
            verbose = verbose)
    }
    
    if ((modelName == "BSGD") | (modelName == "Pegasos" )) {
      values = computeOptimizationValuesBSGD (modelData, 
            trainingData, 
            data = data, 
            predictionOutput = predictionOutput,
            verbose = verbose)
    }
    
    return (values)
}


 

# stupid R check for pythons cool "name == __main__"
if (length(sys.frames()) == 0) 
{
    loadThings <- function ()
    {
    library(stringr)
    library(rbenchmark)
    library(BBmisc)
    library(plyr)
    library(e1071)
    library(SparseM) 

    source ("software/helpers/models/executeSVMSolver.R")
    source ("software/helpers/models/generateExampleHelpers.R")
    source ("software/helpers/models/computeOptimizationValuesLibSVM.R")
    source ("software/helpers/models/computeOptimizationValuesSVMperf.R")
    source ("software/helpers/models/computeOptimizationValuesBSGD.R")
    source ("software/helpers/models/computeOptimizationValuesLLSVM.R")

    # include all our wrappers, TODO: generate a "includeAllWrapper.R"
    source ("software/libSVM/wrapper.R")
    source ("software/SharkSVM/wrapper.R")
    source ("software/BudgetedSVM/wrapper.R")
    source ("software/LASVM/wrapper.R")
    source ("software/libCVM/wrapper.R")
    source ("software/libSVM/wrapper.R")
    source ("software/SVMperf/wrapper.R")
    }
suppressMessages(loadThings())


# set parameters
    cost = 50.0000000
    gamma = 1.0
   
    modelList = c ( "LLSVM",
                    "Pegasos",
                    "BSGD",
                    "libSVM",
                    "LASVM",
                    "libBVM", 
                    "libCVM", 
                    "SVMperf"
                    )

                    modelList = c ( "libSVM",
                    "SharkSVM",
#                    "BSGD",
                    "SVMperf",
                    "Pegasos")

#                    modelList = c("LLSVM")
#                    modelList = c("Pegasos")
#                    modelList = c("SVMperf")
#                    modelList = c("libSVM")
                 
                 verbose = FALSE
    epochs = 1
    for (e in seq(1,2))
    {
    
    pValueList = list()
    for (modelName in modelList) {
      messagef( "================================================ %s", modelName)
    messagef("Epoch: %d", epochs)
    
      # now convert the model
#      examplePath = "software/libSVM/src/heart_scale"
      examplePath = "datasets/australian/australian.combined.scaled"
#      examplePath = generateSimpleExample (filePath = "/tmp/1D.data", n = 1)
      #examplePath = generateSimpleExample1DChain (filePath = "./tmp/1Dchain.data", n = 50)
      #examplePath = generateXORExample2D(filePath = "./tmp/XOR2D.data", n = 12)
      
      rV = executeSVMSolver (cost, gamma, examplePath, modelName, epochs = epochs, verbose = verbose)
      predictionOutput = rV$predictionOutput
      modelFile = rV$modelFile
      modelData = readModel (filePath = modelFile, model = modelName, verbose = verbose)
      modelData$C = cost
      modelData$Lambda = 1
      
      #dumpModel (modelData, data = TRUE)
      
      # compute the weight/primary function value
      values = computeOptimizationValues (modelData = modelData, 
        modelName = modelName, 
        trainingData = examplePath, 
        verbose = verbose, 
        predictionOutput = predictionOutput)

      # compare with what we expect.
      
      
      
      # save
      pValueList[[modelName]][["maxA"]] = values[["maxAlpha"]]
      pValueList[[modelName]][["sumA"]] = values[["sumAlpha"]]
      pValueList[[modelName]][["model"]] = modelName
      pValueList[[modelName]][["primal"]] = values[["primal"]]
      pValueList[[modelName]][["dual"]] = values[["dual"]]
      pValueList[[modelName]][["weight"]] = values[["weight"]]
      pValueList[[modelName]][["error"]] = rV$err
      pValueList[[modelName]][["computedError"]] = values[["error"]]
    }
    
    messagef( "================================================ global ")
      messagef( "primal - dual - weight - computed error - stdout error - max alpha - model ")
    for (modelName in modelList) {
#      messagef( "================================================ %s", modelName)
      p = pValueList$modelName
#      print(p)
      messagef( "p:%f\t d:%f\t w:%f\t e:%f\t ce:%f\t ma:%f\t sa:%f\t %s", 
        as.double(pValueList[[modelName]][["primal"]]), 
        as.double(pValueList[[modelName]][["dual"]]), 
        as.double(pValueList[[modelName]][["weight"]]), 
        as.double(pValueList[[modelName]][["error"]]),
        as.double(pValueList[[modelName]][["computedError"]]),
        as.double(pValueList[[modelName]][["maxA"]]),
        as.double(pValueList[[modelName]][["sumA"]]),
        pValueList[[modelName]][["model"]])
#      print( pValueList$modelName[["primal"]])
    }
    epochs = epochs*2
    }
}

