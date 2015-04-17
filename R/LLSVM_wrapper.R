#!/usr/bin/Rscript  --vanilla 





# evalLLSVM
# @param[in]    trainDataFile       file to read training data from
# @param[in]    testDataFile        file to read test data from
# @param[in]    cost            cost parameter C
# @param[in]    gamma           gamma parameter, note: RBF kernel used by pegasos is exp(-0.5 ...)
# @param[in]    rank            number of landmarks
# @param[in]    bindir          relativ path to the binaries, defaults to default.
# @param[in]    modelFile       path to model, defaults to a temporary file (given by R)



createTrainingArguments.LLSVM = function (x,
								trainDataFile = "",
								modelFile = "",
								extraParameter = "",
								primalTime = 10, 
								wallTime = 8*60,
								cost = 1, 
								gamma = 1, 
								rank = 128, ...) {
    # ---- compute general things
    n = countLines(trainDataFile)

    # ---- sanity checks
    if(n < rank)
        stop("Rank must not be greater or equal to size of dataset")

    # ---- training
    args = c(
        "-A 3",
        "-r 0",
        sprintf("-B %.16f", rank),
        sprintf("-L %.16f", (1.0 / (n * cost))), 
        sprintf("-g %.16f", 2 * gamma),
        extraParameter,
        trainDataFile,
        modelFile
    )

    return (args)
}



createTestArguments.LLSVM = function (x,
									testDataFile = "",
									modelFile = "", 
									predictionOutput = "/dev/null", ...) {
    args = c(
        "-v 1",
#        "-o 1", only works for BSGD for now
        testDataFile,
        modelFile,
        predictionOutput 
    )
  
    return (args)
}



extractTrainingInfo.LLSVM = function (x, output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}


extractTestInfo.LLSVM = function (x, output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}

readModel.LLSVM = function (x, modelFile = "./model", verbose = FALSE)
{
	if (verbose == TRUE) {
		BBmisc::messagef("Reading LLSVM model from %s", modelFile)
	}

    # open connection
    con  <- file(modelFile, open = "r")

	# do we need to invert the labels?
	invertLabels = FALSE

	# grep needed information step by step, the bias is on the threshold line
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) 
    {
        if (grepl("MODEL", oneLine) == TRUE) break;
      
        # gamma value
        if (grepl("KERNEL_GAMMA_PARAM", oneLine) == TRUE) 
        {
            pattern <- "KERNEL_GAMMA_PARAM: (.*)"
            gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        }  
      
        # bias
        if (grepl("BIAS_TERM", oneLine) == TRUE) 
        {
            pattern <- "BIAS_TERM: (.*)"
            bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        }
      
        # order of labels
        if (grepl("LABELS", oneLine) == TRUE) 
        {
            pattern <- "LABELS: (.*)"
            order = (sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        
            if ((order != "1 -1") && (order != "-1 1")) {
                stop ("Label ordering %s is unknown!", order)
            }
        
            if (order == "1 -1") {
                invertLabels = FALSE
            }

            if (order == "-1 1") {
                invertLabels = TRUE
            }
        }  
    }
  
  
	# read and interprete data 
	# basically all data is sparse data format, but the data around this differs
	svmatrix = readSparseFormat(con)

	# add header information
	svmatrix$gamma = gamma
	svmatrix$bias = bias
	svmatrix$modelname = "LLSVM"
	
	
	# do we need to invert the labels? in this case we invert the coefficients
	if (invertLabels == TRUE) {
		if (verbose == TRUE)  
			messagef(" Inverting Labels.")

		# invert alphas
		svmatrix$a = -svmatrix$a

		# this is also needed.. 
		svmatrix$bias = -bias
	}

	# close connection
	close(con)
	
	# return
	return (svmatrix)
}
	
# dummy for now
writeModel.LLSVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		ret = writeModel.LIBSVM (model = model, modelFile = modelFile, verbose = verbose)
		return (ret)
	}
	
# dummy for now
readPredictions.LLSVM = function (x, predictionsFile = "", verbose = FALSE) {
		ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
	}
	
findSoftware.LLSVM = function (x, searchPath = "./", verbose = FALSE) {
		x = findSoftware.BSGD (x, searchPath, verbose)
		return(x)
	}
