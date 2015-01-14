#!/usr/bin/Rscript  --vanilla 

# stupid R
loadThings <- function ()
{
  source ("./models/computeOptimizationValuesLibSVM.R")
  source ("./models/computeErrorsHelper.R")
}
suppressMessages(loadThings())




# compute primary optimization minimum for LibSVM
computeOptimizationValuesSVMperf <- function (modelData = "",
    trainingDataPath = "", 
    predictionOutput = "",
    verbose = FALSE, 
    data = NULL)
{
    # prepare model, the alphas have coefficients for 'both' classes, need simply to crop that
    newmodel = modelData

    # models cost is different, so we have to adapt
    N = countLines(trainingDataPath)
#    newmodel$C = modelData$C/N*100

    # alpha need to be rescaled by number of examples..
#    newmodel$a = newmodel$a*100/N

    values = computeOptimizationValuesLibSVM (newmodel, trainingDataPath, verbose = TRUE)
    
    
    return (values)
}

