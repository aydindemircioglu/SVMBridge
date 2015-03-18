#!/usr/bin/Rscript  --vanilla 



# evalBSGD
# @param[in]    trainfile       file to read training data from
# @param[in]    testfile        file to read test data from
# @param[in]    cost            cost parameter C
# @param[in]    gamma           gamma parameter, note: RBF kernel used by pegasos is exp(-0.5 ...)
# @param[in]    budget          budget parameter
# @param[in]    epochs          number of epochs to run 
# @param[in]    bindir          relativ path to the binaries, defaults to default.
# @param[in]    modelFile       path to model, defaults to a temporary file (given by R)


createTrainingArguments.BSGD = function (trainfile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            cost = 1, 
                                            gamma = 1, 
                                            budget = 128,
                                            epochs = 1, ...) {

    n = countLines(trainfile)


    # arguments for training
    args = c(
        "-A 4",
        "-r 0",
        sprintf("-B %.16f", budget ),
        sprintf("-L %.16f", (1.0 / (n * cost))),
        sprintf("-e %.16f", epochs ),
        sprintf("-g %.16f", 2 * gamma),
        extraParameter,
        trainfile,
        modelFile
    )

    return (args)
}



createTestArguments.BSGD = function (testfile = "",
                                        modelFile = "", 
                                        predictionOutput = "/dev/null", ...) {
    args = c(
        "-v 1",
#        "-o 1", only works for BSGD for now
        testfile,
        modelFile,
        predictionOutput 
    )
  
    return (args)
}



extractTrainingInfo.BSGD = function (output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}

#NEW
extractTestInfo.BSGD = function (output) {
    
    # ---- grep the error rate
    pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
    err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}


readModel.BSGD = function (modelFilePath = "./model", verbose = FALSE)
{
    # open connection
    con  <- file(modelFilePath, open = "r")

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
            
            # yes, exceptions.
            if (model == "LLSVM") {
                invertLabels = !invertLabels
            }
        }  
    }
  
  
	# read and interprete data 
	# basically all data is sparse data format, but the data around this differs
	svmatrix = readSparseFormat(con)

	# add header information
	svmatrix$gamma = gamma
	svmatrix$bias = bias
	svmatrix$modelname = "BSGD"
	
	
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
writeModel.BSGD = function (model = NA, modelFilePath = "./model", verbose = FALSE) {
	if (verbose == TRUE) {
		messagef ("Writing SVM Model..")
	}

	# BROKEN, FIXME
	
	# FIXME: label order
	# TODO: support multiclass
	model$nrclass = 2
	posSV = sum(model$a > 0)
	negSV = sum(model$a < 0)
    # open connection
    modelFileHandle <- file(modelFilePath, open = "w+")
	writeLines(paste ("svm_type c_svc", sep = ""), modelFileHandle )
	writeLines(paste ("kernel_type", "rbf", sep = " "), modelFileHandle )
	writeLines(paste ("gamma", model$gamma, sep = " "), modelFileHandle )
	writeLines(paste ("nr_class", model$nrclass, sep = " "), modelFileHandle )
	writeLines(paste ("total_sv", length(model$a), sep = " "), modelFileHandle )
	writeLines(paste ("rho", model$bias, sep = " "), modelFileHandle )
	writeLines(paste ("label 1 -1", sep = " "), modelFileHandle )
	writeLines(paste ("nr_sv", posSV, negSV, sep = " "), modelFileHandle )
	writeLines(paste ("SV", sep = ""), modelFileHandle )

	# basically all data is sparse data format, but the data around this differs
	svmatrix = dumpSparseFormat(model$a, model$X)
	writeLines(svmatrix, modelFileHandle, sep = "" )
	
	# close connection
	close(modelFileHandle)
}
 

#
# @param[in]	predictionsFile		file to read predictions from
# @return		array consisting of predictions
#

readPredictions.BSGD <- function (predictionsFilePath = "", verbose = FALSE) {
    # open connection
    con  <- file(predictionsFilePath, open = "r")

    predictions = c()
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
		predictions = c(predictions, as.numeric(oneLine))
    }
    
	if (verbose == TRUE) {
		print(predictions)
	}
			
	close (con)
	
    return (predictions)
}

#NEW
findSoftware.BSGD = function(x, searchPath = "./", verbose = FALSE) {
		# short way without verbose messages
		x$trainBinaryPath  = findBinary (searchPath, "^svm-train$", "Usage: svm-train .options. training_set_file .model_file.", verbose = verbose)
	}
