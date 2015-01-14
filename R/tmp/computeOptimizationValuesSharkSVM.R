#!/usr/bin/Rscript  --vanilla 

# stupid R
loadThings <- function ()
{
  source ("./models/computeOptimizationValuesLibSVM.R")
  source ("./models/computeErrorsHelper.R")
}
suppressMessages(loadThings())



# compute primary optimization minimum for LibSVM
computeOptimizationValuesSharkSVM <- function (model, trainingDataPath, verbose = FALSE, 
        predictionOutput = predictionOutput,
        data = NULL)
{
    # prepare model, the alphas have coefficients for 'both' classes, need simply to crop that
    newmodel = model
    newmodel$bias = -newmodel$bias
    values = computeOptimizationValuesLibSVM(newmodel, trainingDataPath, verbose = verbose, data = NULL)

    return (values)
}


