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

  source ("./models/computeOptimizationValuesLibSVM.R")
  source ("./models/computeOptimizationValuesSVMperf.R")
  source ("./models/computeOptimizationValuesSharkSVM.R")
  source ("./models/computeOptimizationValuesBSGD.R")
  source ("./models/computeOptimizationValuesLLSVM.R")
}
suppressMessages(loadThings())


grepNumberAfter <- function (grepString = "", inputString = "")
{
    basicNumber = "\\s*([-]?\\d+\\.?\\d*)"
    pattern <- paste(".*", grepString, basicNumber, sep = "")
    return (as.numeric(sub(pattern, '\\1', inputString[grepl(pattern, inputString)])))
}



computeOptimizationValuesViaShark <- function (modelData = "", 
    modelPath = "",
    modelName = "", 
    testDataPath = "", 
    data = -1,
    extraParameter = "",
    predictionOutput = "",
    verbose = FALSE)
{
    # extract infos from Modeldata
    cost = modelData$C

    args = list()

    args = c(    
        sprintf("-c %f", cost), 
        "-s 1",
        "-v 0",
        extraParameter,
        testDataPath,
        modelPath
    )
    
    bindir = "../SharkSVM/bin/"
    evalBinary = "SharkSVM-predict"
    s = system3(file.path(bindir, evalBinary), args, verbose = verbose)

    values = list()
    
    # grep error
    output = s$output
    values[["error"]] = grepNumberAfter("Test error rate:", output)
    values[["primal"]] = grepNumberAfter("Primal:", output)
    values[["dual"]] = grepNumberAfter("Dual:", output)
    values[["weight"]] = grepNumberAfter("Weight vector length:", output)
    values[["weightsqr2"]] = grepNumberAfter("0.5 Weight vector length squared:", output)
    values[["maxAlpha"]] = grepNumberAfter("Max of Alphas:", output)
    values[["sumAlpha"]] = grepNumberAfter("Sum of Alphas:", output)
    values[["maxYAlpha"]] = grepNumberAfter("Max of labeled Alphas:", output)
    values[["sumYAlpha"]] = grepNumberAfter("Sum of labeled Alphas:", output)
    values[["loss"]] = grepNumberAfter("Loss:", output)

    return (values)
}



computeOptimizationValues <- function (modelData = "", 
    modelName = "", 
    trainingData = "", 
    data = -1,
    predictionOutput = "",
    verbose = FALSE)
{
# compute value with the specific model
    if ((modelName == "libSVM") | (modelName == "LASVM") | (modelName == "libBVM") |  (modelName == "libCVM") | (modelName == "AESVM")) {
      values = computeOptimizationValuesLibSVM (modelData, 
        trainingData, 
        data = data, 
        predictionOutput = predictionOutput,
        verbose = verbose)
    }

    # for now all sharks are the same.
    if ((modelName == "SharkSVMPegasos") | (modelName == "SharkSVMSMO")) {
        modelName <- "SharkSVM";
    }

    if ((modelName == "SharkSVM") ) {
      values = computeOptimizationValuesSharkSVM (modelData, 
        trainingData, 
        predictionOutput = predictionOutput,
        data = data, 
        verbose = verbose)
    }
    
    if ((modelName == "SVMperf")  ) {
      values = computeOptimizationValuesSVMperf (modelData, 
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

    source ("./models/executeSVMSolver.R")
    source ("./models/generateExampleHelpers.R")
    source ("./models/computeOptimizationValuesLibSVM.R")
    source ("./models/computeOptimizationValuesSVMperf.R")
    source ("./models/computeOptimizationValuesBSGD.R")
    source ("./models/computeOptimizationValuesLLSVM.R")

    # include all our wrappers, TODO: generate a "includeAllWrapper.R"
    source ("./includeWrapper.R")
    }
suppressMessages(loadThings())


# set parameters
    cost = 4
    gamma = 1
    
    # some optimal heart point
#    cost = 0.19
 #   gamma = 1.0496346230004575   
    
    modelList = c ( "LLSVM",
                    "Pegasos",
                    "BSGD",
                    "libSVM",
                    "LASVM",
                    "libBVM", 
                    "libCVM", 
                    "SVMperf",
                    "AESVM"
                    )

                    modelList = c ( "libSVM",
                    "SharkSVM",
                    "BSGD",
                    "LLSVM",
                    "SVMperf",
                    "Pegasos")

                    modelList = c ( 
                    "libSVM",
                    "AESVM",
                    #"BSGD",
                    "LASVM",
                    "SharkSVMPegasos",
                    "SharkSVMSMO",
               #     "LLSVM",
                    "SVMperf")


#                   modelList = c("libSVM", "LLSVM")
#                    modelList = c("Pegasos")
 #                   modelList = c("LLSVM")
#                    modelList = c("SVMperf")
#                    modelList = c("libCVM", "libSVM", "libBVM")
                   
    pValueList = list()

    # generate or load datasets
    n = 64
#        examplePath = getData ("heart")
        examplePath = getData ("a8a")
#        examplePath = getData ("australian")
        #examplePath = getData ("diabetes")
#        examplePath = generateSimpleExample2D ("tmp/2Dchain.data", n = n)
#    examplePath = generateLinear5 ("tmp/linear5.data", n =n)
    #examplePath = generateRadial5 ("tmp/radial5.data", n = n)
#    examplePath = generateXORClusters  ("tmp/xorcluster.data", n = floor(n/4))
#    examplePath = generateSimpleThreePoint ("tmp/easyexample.data")
#    examplePath = generateXORExample2D ("tmp/xorexample2D.data", n = 30)
#    examplePath = generate2621 ("tmp/2621.data")
#    examplePath = generateSimpleExample  ("tmp/simple.data", n = 7, scale = TRUE)
    
    for (modelName in modelList) {
        messagef( "================================================ %s", modelName)
    
        rV = executeSVMSolver (cost, gamma, examplePath, modelName, 
                epochs = 10, 
                budget = n, #floor(n*3/4),
                rank = n, #floor(n*3/4),
                k = n,  #floor(n*3/4),
                verbose = TRUE, 
                subsamplingRate = 0.05,
                method = "Nystrom")
#                k = 1000)

        predictionOutput = rV$predictionOutput
        modelFile = rV$modelFile
        modelData = readModel (filePath = modelFile, model = modelName, verbose = TRUE, cost = cost)
      
        dumpModel (modelData, data = FALSE)
      
        # compute the weight/primary function value
        model = "Direct"
        if (model == "Direct") {
            values = computeOptimizationValuesViaShark (modelData= modelData, 
                modelPath = modelFile,
                modelName = modelName, 
                testDataPath = examplePath, 
                verbose = TRUE, 
                predictionOutput = predictionOutput)
        } else {
            # compute the weight/primary function value
            values = computeOptimizationValues (modelData = modelData, 
                modelName = modelName, 
                trainingData = examplePath, 
                verbose = TRUE, 
                predictionOutput = predictionOutput)
        }
        print (modelFile)
      
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
}

