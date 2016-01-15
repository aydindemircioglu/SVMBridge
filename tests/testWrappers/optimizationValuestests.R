#
# SVMBridge 
#
#		(C) 2015, by Aydin Demircioglu
# 
# SVMBridge is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SVMBridge is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# Please do not use this software to destroy or spy on people, environment or things.
# All negative use is prohibited.
#



## old routines (that should basically work)



computeErrors <- function (prediction = list(), labels = list(), verbose = FALSE )
{
    # sanity check
    if (length(prediction) != length(labels)) {
        # labels might be transposed
        labels = t(labels)
    }

    # retry
    if (length(prediction) != length(labels)) {
        stopf ("Cannot compute error, lengths are different.")
    }

    # we take the sign as a precaution, sign is idempotent
    prediction = sign (prediction)
    
    absError = sum(abs(labels - t(prediction))/2)
    relError = absError/length(prediction)

    for (i in 1:length(prediction)) {
        #messagef ("t:%f -- p:%f", labels[i], prediction[i]) 
    }
    
    pr = 2*as.numeric(prediction<= 0) - 1 

    errors = list ()
    errors$absError = absError
    errors$relError = relError
    if (verbose)  messagef( "Rel.Error  %f", relError)
    if (verbose)  messagef( "Abs.Error  %f", absError)
    
    return (errors)
}

 
 
computeErrorsLLSVM <- function (prediction = list(), labels = list(), verbose = FALSE )
{
    # we take the sign as a precaution, sign is idempotent
    PS = prediction
    prediction = 1.0 * (prediction > 0.0) + (-1.0) * ((prediction <= 0.0))
    
    absError = sum(abs(labels - t(prediction))/2)
    relError = absError/length(prediction)

    #print (prediction)
    tfile = tempfile()
    for (i in seq(1, length(prediction))) {
      write(paste (prediction[i], PS[i]), file = tfile, append = TRUE)
    }
    pr = 2*as.numeric(prediction<= 0) - 1 

    errors = list ()
    errors$absError = absError
    errors$relError = relError

    if (verbose)  messagef( "LLSVM Rel.Error  %f", relError)
    if (verbose)  messagef( "LLSVM Abs.Error  %f", absError)
    
    return (errors)
}



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
        data <- e1071::read.matrix.csr (trainingDataPath)
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
    rbf <- kernlab::rbfdot (sigma = gamma)
    k_sv = kernlab::kernelMatrix(rbf, SV)
    if (extraverbose)  messagef( "ya %d x %d", dim(ya)[1], dim(ya)[2])
    if (extraverbose)  messagef( "k_sv %d x %d", dim(k_sv)[1], dim(k_sv)[2])
    half_wTw = 0.5 * t(ya) %*% k_sv %*% ya

    # compute hinge loss
    k_sv_test = kernlab::kernelMatrix(rbf, testData, SV)
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

    #print (loss)

    # and sum all these entries  and multiply by C
    if (verbose)  messagef( "sum slacks %f", sum(loss))
    weight = L * half_wTw
    hingeLoss = C * sum(loss)
    if (extraverbose)  messagef("  Hingeloss: %f", hingeLoss)

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



## end old routines




optimizationValuestests  = function (solver, verbose) {
	
	# for now just set a fixed C
	C = 7.74
	
	if (verbose == TRUE) {
		cat ("Testing optimization values for ", solver, "\n")
	}
	
	
	# read data
	australian = readSparseData ("../data/australian.train")
	
	# create table of what we expecte
	mysolver = c("BSGD", "LIBSVM", "LASVM", "CVM", "BVM", "LLSVM", "SVMperf")
	trainingError = c(0.226087, 0.257971, 0.2, 0.742029, 0.7362319, 0.4927536, 0.2028986)
	halfwTw = c(8.501867, 15.66334, 15.8894, 0.5149239, 0.5202258, 0.4999954, 16.09485)
	primalValue  = c(1646.83, 1926.384, 1968.645, 2698.021, 2705.437, 2667.982, 1987.876)
	dualValue = c(16.50107, 16.98221, 17.41129, 1.463981, 1.465854, 13.11328, 17.81096)
	optTable = data.frame(cbind (mysolver, primalValue, dualValue, halfwTw, trainingError))
	
	# TODO: make this compact :(
	optTable$mysolver = as.character(mysolver)
	optTable$primalValue = as.numeric (primalValue)
	optTable$dualValue = as.numeric (dualValue)
	optTable$halfwTw = as.numeric (halfwTw)
	optTable$trainingError = as.numeric (trainingError)
	
	curModel = optTable [mysolver == solver,]

	if (TRUE == FALSE) {
		print (oV$trainingError)
		print (oV$weight)
		print (oV$primal)
		print (oV$dual)
	}
	
	# read model first
	modelFile = file.path ("..", "data", paste (solver, "australian", "model", sep = "."))
#			modelType = detectModelTypeFromFile (modelFile)
	modelType = solver
	
	# read model
	model = readModelFromFile (modelFile, modelType)
		
	# compute values
	X = as.matrix(australian$X)
	Y = as.matrix(australian$Y)[,1]
	oV = optimizationValues (X = X, Y = Y, model = model, C = C)

	# check values
	expect_equal (oV$primal, curModel$primalValue, tolerance = 0.01)
	expect_equal (oV$dual, curModel$dualValue, tolerance = 0.01)
	expect_equal (oV$weight, curModel$halfwTw, tolerance = 0.01)
	expect_equal (oV$trainingError, curModel$trainingError, tolerance = 0.01)
	
	# check also against old way of computing it (TODO: actually we could remove the table above..)
	
}	
	

	
	
# 		oV = optimizationValues (X = as.matrix(australian$X), Y = as.matrix(australian$Y), model = model, C = C)
# 
# 		# create regression model
# 		data = list()
# 		data$x = australian$X
# 		data$y = as.vector(australian$Y)
# 		model$L = 1
# 		model$C = C
# 		model$X = model$SV
# 
# 		# special care of BSGD
# 		if (s == "BSGD") {
# 			model$alpha = model$alpha[,1]
# 			model$gamma = model$gamma/2
# 			model$L = 1
# 			model$C = C
# 		}
# 		
# 		pV = computeOptimizationValuesLibSVM (model, NULL, data = data,  predictionOutput = NULL, verbose = FALSE)
# 
# 		expect_equal(oV$primal, pV$primal[1,1])
# 		expect_equal(oV$dual, pV$dual[1,1])
