
# stupid R
loadThings <- function ()
{
  library(kernlab)
  source ("./tests/computeErrorsHelper.R")
}
suppressMessages(loadThings())


# compute primary optimization minimum for LibSVM
computeOptimizationValuesLibSVM <- function (model, trainingDataPath, 
        data = NULL, 
        predictionOutput = predictionOutput,
        verbose = FALSE)
{
    extraverbose = verbose

    # read trainings data
    if ((is.null(data) == TRUE ) | (class(data) == "numeric"))
    {
        data <- read.matrix.csr (trainingDataPath)
    }
    
    values = list()
    
    # get parameter
    gamma = model$gamma
    L = model$L
    C = model$C
    bias = model$bias
    if (verbose == TRUE)  messagef("  bias:  %f", model$bias)
    if (verbose)  messagef("  gamma: %f", model$gamma)
    if (verbose)  messagef("  lambda: %f", model$L)
    if (verbose)  messagef("  cost: %f", model$C)

    # get model
    ya = matrix(model$a, byrow = TRUE)
    a = sign(ya) * ya
    y = sign(ya)
#    if ( (max(a) > C) | (min(a) < 0.0) )
 #       stopf ("Sorry, the alphas are out of any borders and boundaries, we have max(a) = %f, min(a) = %f", max(a), min(a))
    SV = model$X

    # obtain labels
    testData = as.matrix(data$x)
    testLabel = as.numeric(as.matrix(data$y))
    if ( (max(testLabel) > 1) | (min(testLabel) > -0.5) )
        stop ("Sorry, the labels from the test dataset seem not to be in {-1, 1}.")
        
    
    # compute weight vector
    rbf <- rbfdot (sigma = gamma)
    k_sv = kernelMatrix(rbf, SV)
    if (extraverbose)  messagef( "ya %d x %d", dim(ya)[1], dim(ya)[2])
    if (extraverbose)  messagef( "k_sv %d x %d", dim(k_sv)[1], dim(k_sv)[2])
    half_wTw = 0.5 * t(ya) %*% k_sv %*% ya
    

    # compute hinge loss
    k_sv_test = kernelMatrix(rbf, testData, SV)
    if (extraverbose)  messagef( "k_sv_test %d x %d", dim(k_sv_test)[1], dim(k_sv_test)[2])
    b = as.matrix(rep (bias, length(testLabel)))
    predictionScore = k_sv_test %*% ya - b
    if (extraverbose)  messagef( "b %d x %d", dim(b)[1], dim(b)[2])
    if (extraverbose)  messagef( "predictionScore %d x %d", dim(predictionScore)[1], dim(predictionScore)[2])
    e = as.matrix(rep (1, length(testLabel)))
    loss = e - (testLabel) * predictionScore
    loss = loss - loss * (loss<0) 
    if (extraverbose)  messagef("  Max of hingeloss: %f", max(loss))
    if (extraverbose)  messagef("  Min of hingeloss: %f", min(loss))


    # and sum all these entries  and multiply by C
    if (verbose)  messagef( "sum slacks %f", sum(loss))
    weight = L * half_wTw
    hingeLoss = C * sum(loss)

    pValue = weight + hingeLoss
    dValue = -(half_wTw - sum(a))
    

    errors = computeErrors (prediction = predictionScore, labels = testLabel)
    values[["error"]] = errors$relError
    values[["primal"]] = pValue
    values[["dual"]] = dValue
    values[["weight"]] = sqrt(2*half_wTw)
    values[["weightsqr2"]] = half_wTw
    values[["loss"]] = sum(loss)
    values[["maxAlpha"]] = max(a)
    values[["maxYAlpha"]] = max(ya)
    values[["sumAlpha"]] = sum(a)
    values[["sumYAlpha"]] = sum(ya)

    if (verbose)  messagef("  test error: %f", errors$relError)
    if (verbose)  messagef("  1/2 w^T w: %f", half_wTw)
    if (verbose)  messagef("  ||w||: %f", sqrt(2*half_wTw))
    if (verbose)  messagef( "primalvalue %f", pValue)
    if (verbose)  messagef( "dualvalue %f", dValue)
    if (verbose)  messagef( "predictionScore %f x %f", dim(predictionScore)[1], dim(predictionScore)[2])
    if (verbose)  messagef( "loss  %f", hingeLoss)
    if (verbose)  messagef( "%f/2 ||w||^2 + %f loss", L, C)
    if (verbose)  messagef("  Sum of alpha coefficients with label: %f", sum(ya))
    if (verbose)  messagef("  Sum of alpha without label (0 < a < C): %f", sum(a))
    if (verbose)  messagef("  Max of alpha with label: %f", max(ya))
    if (verbose)  messagef("  Max of alpha: %f", max(a))
    

    return(values)
}

