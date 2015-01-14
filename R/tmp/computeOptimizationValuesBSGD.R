#!/usr/bin/Rscript  --vanilla 

# stupid R
loadThings <- function ()
{
  source ("./models/computeOptimizationValuesLibSVM.R")
  source ("./models/computeErrorsHelper.R")
}
suppressMessages(loadThings())



# compute primary optimization minimum for LibSVM
computeOptimizationValuesBSGD <- function (modelData = "", 
    trainingDataPath = "", 
    predictionOutput = "",
    verbose = FALSE, 
    data = NULL)
{
    # prepare model, the alphas have coefficients for 'both' classes, need simply to crop that
    newmodel = modelData
    newmodel$a = (t(modelData$a[,1]))
    newmodel$gamma = modelData$gamma/2
    
    # sanity check
    cl1 = modelData$a[,1]
    cl2 = -modelData$a[,2]
    if (!all(cl1 == cl2)) {
      stop ("Sanity check failed: alpha_1 == -alpha_2 is violated.")
    }
    
    # compute lambda and correction factor as BSGD model works with lambda-formulation
    N = countLines(trainingDataPath)
   # correctionFactor = modelData$C * N
  #  newmodel$L = 1/correctionFactor
 #   newmodel$C = 1/N
    
    values = computeOptimizationValuesLibSVM(newmodel, trainingDataPath, verbose = TRUE)

    # correct values 
#    values[["primal"]] = correctionFactor * values[["primal"]]
#    values[["dual"]] = correctionFactor * values[["dual"]]

    return (values)
}


