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

  source ("software/helpers/models/computeOptimizationValues.R")
  source ("software/helpers/models/computeOptimizationValuesLibSVM.R")
  source ("software/helpers/models/computeOptimizationValuesSVMperf.R")
  source ("software/helpers/models/computeOptimizationValuesSharkSVM.R")
  source ("software/helpers/models/computeOptimizationValuesBSGD.R")
  source ("software/helpers/models/computeOptimizationValuesLLSVM.R")
}
suppressMessages(loadThings())


subsampleData <- function ( filepath = "", subsamplingRate = -1)
{
    subsampledFile = filepath
    if (subsamplingRate > 0.0) 
    {
        # create subsampled file on /tmp
        subsampledFile = tempfile()
        originalFile = filepath

        # need to know how many data is there anyway
        subsamplingSize = subsamplingRate
        if (subsamplingRate < 1.001)
        {
            # this is a true fraction, so  compute true number and overwrite our assumption
            tmpOutput = system3('wc', c("-l", filepath))#, stdout = TRUE)
            fraction = as.numeric(strsplit(tmpOutput$output, split = " ")[[1]][1])
            subsamplingSize = floor(fraction*subsamplingRate)
        }

        # ask system to do subsampling for us
        system3('head', 
            c(sprintf("-n %d", subsamplingSize), 
              filepath,
                ">",
                subsampledFile)
        )
    }
    
    return (subsampledFile)
}

    
checkForParameterCorrectness <- function(table = list(), epsilon = 0.0001)
{
    # baseline
    baselineModel = "SharkSVMSMO"
    
    # first grep the results of libSVM.
    # this is the baseline we will compare all other values to
    libSVMPrimal = as.double(table[[baselineModel]][["primal"]])
    libSVMDual = as.double(table[[baselineModel]][["dual"]])

    # now check that every other model obeys what we expect 
    for (modelName in names(table))
    {
        primal = as.double(table[[modelName]][["primal"]]) 
        dual = as.double(table[[modelName]][["dual"]]) 
        
        message = "OK"
        if (primal + epsilon < dual)
           message = paste( message, sprintf("    -Wrong model p:%f - d:%f   ", primal, dual), sep = "\n")
        
        if (libSVMDual > primal + epsilon) 
            message = paste( message, sprintf("    -Wrong model p:%f - d:%f  vs true p:%f - d:%f ", primal, dual, libSVMPrimal, libSVMDual), sep = "\n")
        
        if (libSVMPrimal < dual + epsilon) 
            message = paste( message, sprintf("    -Wrong model p:%f - d:%f  vs true p:%f - d:%f ", primal, dual, libSVMPrimal, libSVMDual), sep = "\n")
        
        if (message == "OK")
            messagef("   %s passed the test.", modelName)
        else
            messagef("   %s failed the test: NOT %s", modelName, message)
    }

    return (".ok")
}


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


 

# stupid R check for pythons cool "name == __main__"
#if (length(sys.frames()) == 0) 
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

        # include all our wrappers, TODO: generate a "includeAllWrapper.R"
        source ("software/helpers/includeWrapper.R")
        source ("software/helpers/lsdir.R")
    }
    suppressMessages(loadThings())



# where is the path where we store our results
    globalPath = "./experiments/primalValue"
    globalModelPath = file.path(globalPath, "models/")
    globalParameterPath = file.path(globalPath, "parameters/")

    
# parse over all parameter files we have
    messagef ("Searching for parameters in %s", globalParameterPath)
    parameterfiles <- list.files (globalParameterPath, pattern='*\\.parameter')
    for (currentFile in parameterfiles) {
        
        # parse algorithm and dataset
            messagef("  Loading parameters of %s ", currentFile)
            pattern <- "(.*)_(.*).parameter"
            dataset = sub(pattern, '\\1', currentFile[grepl(pattern, currentFile)])
            algorithm = sub(pattern, '\\2', currentFile[grepl(pattern, currentFile)])
            messagef("  Dataset: %s  ", dataset)
            messagef("  Algorithm: %s  ", algorithm)
            
            paretoFront = read.table (file.path(globalParameterPath, currentFile))
        
        # now extract all relevant parameter from front-- these FIXME, there are more
            cost = 0
            epochs = 0
            gamma = 0
            epsilon = 0
            k = 0
            budget = 0
            rank = 0
            
            # this will assign the variables their respective values
            for (n in names(paretoFront))
            {
                expression = paste(n, " = ",  "paretoFront$", n, sep = "")
                eval(parse(text = expression))
            }
        
        # get dataset and subsample
            datasetPath = getData (dataset)
            subsampledDataPath = subsampleData (filepath = datasetPath, subsamplingRate = 8192)

        
        rV = executeSVMSolver (cost = cost, 
                gamma = gamma, 
                subsampledDataPath, 
                modelName = algorithm,
                epochs = epochs, 
                epsilon = epsilon,
                method = "CPSP",
                budget = budget, 
                rank = rank,  
                k = k, 
#                subsampleTest = TRUE,
                verbose = TRUE, 
            )
    }
    

    
    
    for (modelName in modelList) {
        messagef( "================================================ %s", modelName)
    
        rV = executeSVMSolver (cost, gamma, examplePath, modelName, 
                epochs = 5, 
                method = "CPSP",
                budget = n, #floor(n*3/4),
                rank = n, #floor(n*3/4),
                k = n, #floor(n*3/4),
#                subsampleTest = TRUE,
                verbose = TRUE, 
            )

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
    
    # check for parameter correctness now
    checkForParameterCorrectness (table = pValueList, epsilon = 0.0001)
}

