#!/usr/bin/Rscript  --vanilla 





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
    n = R.utils::countLines(trainDataFile)

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
		cat("Reading LLSVM model from ", modelFile, "\n")
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
  
  
	# these will contain the coefficients and the  svs.
	supportvectors <- matrix()
	coefficients <- matrix()
	weights <- matrix()
	
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
	
		# remove comment if necesary
		oneLine = stringr::str_split_fixed(oneLine, pattern = '#', n = 2)[1]
		
		# split line by " "
		svec = vector(length = 1)
		parts = strsplit (oneLine, " ")
		
		# where the support vector data starts in the row
		fvpos = 1
		coeff = vector(length = 1)
		w = vector (length = 1)
		 
		# first entry is the weight vector
		value = as.numeric(parts[[1]][1])
		w[1] = value
		
		# read part for part until it is something positive
		for (i in seq(2, length(parts[[1]]))) {
			# if the entry has no colon, then it is a landmark weight
			if (grepl (":", parts[[1]][i]) == FALSE) {
				# just save it as a numerical in the coefficent matrix
				value = as.numeric(parts[[1]][i])
				# i-1 as we cropped the first value as a weight
				coeff[i-1] = value
			} else {
				# we have a feature vector, so go over to the data part
				fvpos = i
				break
			}
		}
		
		# grep feature vectors one by one
		for (i in fvpos:length(parts[[1]])) {
			# split by :
			fparts <- strsplit (parts[[1]][i], ":")
			
			# if we have anything, add it to our vector
			if (!is.na(fparts[[1]][1])) {
				ind = as.numeric(fparts[[1]][1])
				value = as.numeric(fparts[[1]][2])
				svec[ind] <- value
			}
		}
    
		# make sure our vector has no NAs
		#print (svec)
		svec[is.na(svec)] <- 0
		
		# stack matrices
		supportvectors <- plyr::rbind.fill.matrix(supportvectors, t(svec))
		coefficients <- plyr::rbind.fill.matrix(coefficients, t(coeff))
		weights <- plyr::rbind.fill.matrix(weights, t(w))
	}
	
	# crop first NA list (why does it even exist?..)
	supportvectors = supportvectors[-1, ]
	coefficients = coefficients[-1, ]
	weights = weights[-1, ]
	
	# remove possible NA values that occur if the very last
	# entry of a sparse vector is omitted because of sparsity
	supportvectors[is.na(supportvectors)] <- 0
	coefficients[is.na(coefficients)] <- 0 
	weights[is.na(weights)] <- 0 

	dimnames(supportvectors) = NULL
	dimnames(coefficients ) = NULL
	model = list("SV" = supportvectors, "alpha" = coefficients, "w" = weights)
	
	# add header information
	model$gamma = gamma
	model$bias = bias
	model$modelType = "LLSVM"
#	model$nSV	= c(sum(model$alpha[,1] > 0), sum(model$alpha[,1] < 0))
#	model$label = as.numeric(labels)

	print (model)
	# sanity check
	if (sum(model$nSV) != totalSV) {
		stop ("Counted number of SV and info given in header do not fit.")
	}
	
	# do we need to invert the labels? in this case we invert the coefficients
	if (invertLabels == TRUE) {
		if (verbose == TRUE)  
			cat(" Inverting Labels.")

		# invert alphas 
		model$alpha = -model$alpha
		
		# this is also needed.. 
		model$bias = -bias
	}

	# close connection
	close(con)
	
	# return
	return (model)
}



# dummy for now
writeModel.LLSVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		ret = writeModel.LIBSVM (model = model, modelFile = modelFile, verbose = verbose)
		return (ret)
	}

	

#' Detect whether a file is a model for LLSVM.
#'
#' @param	x		Object
#' @param	modelFile		File to check 
#' @param	verbose		Verbose output?
#'
#' @return	TRUE if the given modelFile exists and fits the LLSVM model, or FALSE if not.
#'
#' @note	This is a very basic check, enough to distinguish the wrappers provided within the SVMBridge

detectModel.LLSVM = function (x, modelFile = NULL, verbose = FALSE) {
	checkmate::checkFlag (verbose)
	if (is.null (modelFile) == TRUE) 
		return (FALSE)
	
	# read first lines and detect magic marker
	if (file.exists (modelFile) == FALSE) 
		return (FALSE)
		
	line = readLines(modelFile, n = 12)
	if (sum(grepl("total_sv", line)) > 0) {
		return (TRUE)
	}
	
	return (FALSE)
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
