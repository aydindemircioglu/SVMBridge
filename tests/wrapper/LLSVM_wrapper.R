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


createTrainingArguments.LLSVM = function (x,
	trainDataFile = "",
	modelFile = "",
	extraParameter = "",
	cost = 1,
	gamma = 1,
	rank = 128, ...) {

	n = R.utils::countLines(trainDataFile)

	# ---- sanity checks
	if(n < rank)
		stop("Rank must not be greater or equal to size of dataset")

	args = c(
		"-A 3",
		"-r 0",
		sprintf("-B %.16f", rank),
		sprintf("-L %.16f", (1.0 / (n * cost))),
		sprintf("-g %.16f", 2 * gamma),
		extraParameter,
		bashEscape (trainDataFile),
		bashEscape (modelFile)
	)

	return (args)
}



createTestArguments.LLSVM = function (x, testDataFile = NULL, modelFile = NULL, predictionsFile = NULL, verbose = FALSE, ...) {
    args = c(
		"-v 1",
		bashEscape (testDataFile),
		bashEscape (modelFile),
		bashEscape (predictionsFile)
	)

	return (args)
}



extractTrainingInfo.LLSVM = function (x, output, verbose) {
	pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
	err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
	return (err)
}



extractTestInfo.LLSVM = function (x, output, verbose) {
	pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
	err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
	return (err)
}



readModel.LLSVM = function (x, modelFile = "./model", verbose = FALSE) {
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
			gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) / 2 # make sure it works like the 'normal' gamma for the user
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
	model$nSV	= c(sum(model$w > 0), sum(model$w < 0)) # dummy, with no reason at all
	model$totalSV = nrow(supportvectors)
	model$label = as.numeric(unlist(strsplit(order, " ")))

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



writeModel.LLSVM = function (x, model = NA, modelFile = "./model", verbose = FALSE)
{
	#FIXME: does this change globally?
	options(scipen=999)

	# TODO: check for weightvector, coefficients and SV

	# check for size
	if ((nrow(model$SV) != nrow(model$alpha)) || (nrow(model$alpha) != length(model$w))) {
		warning ("Cannot write model, as model is not consistent!")
		return (FALSE)
	}

	# get things
	dim = ncol (model$SV)
	nClasses = 2
	nWeights = nrow (model$SV)
	gamma = model$gamma * 2
	degree = 1
	coef = 1

	# write header
	modelFileHandle <- file(modelFile, open = "w+")
	writeLines(paste ("ALGORITHM: 3", sep = ""), modelFileHandle )
	writeLines(paste ("DIMENSION:", dim, sep = " "), modelFileHandle )
	writeLines(paste ("NUMBER_OF_CLASSES:", nClasses, sep = " "), modelFileHandle )
	writeLines(paste ("LABELS:", as.character (paste (model$label, sep = " ", collapse = " ")), sep = " "), modelFileHandle )
	writeLines(paste ("NUMBER_OF_WEIGHTS:", nWeights, sep = " "), modelFileHandle )
	writeLines(paste ("BIAS_TERM: ", model$bias, sep = " "), modelFileHandle )
	writeLines(paste ("KERNEL_FUNCTION: 0", sep = " "), modelFileHandle )
	writeLines(paste ("KERNEL_GAMMA_PARAM:", gamma, sep = " "), modelFileHandle )
	writeLines(paste ("KERNEL_DEGREE_PARAM:", degree, sep = " "), modelFileHandle )
	writeLines(paste ("KERNEL_COEF_PARAM:", coef, sep = " "), modelFileHandle )
	writeLines(paste ("MODEL:", sep = ""), modelFileHandle )

	for (r in 1:nrow(model$SV)) {
		wline = paste0 (model$w[r])
		aline = ""
		for (c in 1:ncol(model$a)) {
			aline = paste0 (aline, -model$a[r,c], " ")
		}
		sline = ""
		for (c in 1:ncol(model$SV)) {
			if (model$SV[r,c] != 0) {
				aline = paste0 (aline, c, ":", model$SV[r,c], " ")
			}
		}
		wholeLine = paste0 (wline, " ", aline, sline, "\n")
		writeLines(wholeLine, modelFileHandle, sep = "" )
	}
	close(modelFileHandle)

	return (TRUE)
}



detectModel.LLSVM = function (x, modelFile = NULL, verbose = FALSE) {
	checkmate::checkFlag (verbose)
	
	if (verbose == TRUE) {
		cat ("Checking for LLSVM model.\n")
	}

	if (is.null (modelFile) == TRUE)
		return (FALSE)

	if (file.exists (modelFile) == FALSE)
		return (FALSE)

	line = readLines(modelFile, n = 1)
	if (line == "ALGORITHM: 3") {
		return (TRUE)
	}

	return (FALSE)
}



readPredictions.LLSVM = function (x, predictionsFile = "", verbose = FALSE) {
	ret = readLIBSVMPredictions (predictionsFile = bashEscape (predictionsFile), verbose = verbose)
	return (ret)
}



findSoftware.LLSVM = function (x, searchPath = "./", verbose = FALSE) {
	if (verbose == TRUE) {
		cat ("    BSGD Object: Executing search for software for ", x$method)
	}

	# can do now OS specific stuff here
	if(.Platform$OS.type == "unix") {
		if (verbose == TRUE) {
			cat ("    Unix binaries.\n")
		}
		trainBinaryPattern = "budgetedsvm-train"
		testBinaryPattern = "budgetedsvm-predict"
	} else {
		if (verbose == TRUE) {
			cat ("    Windows binaries.\n")
		}
		trainBinaryPattern = "budgetedsvm-train.exe"
		testBinaryPattern = "budgetedsvm-predict.exe"
	}

	# can do now OS specific stuff here
	x$trainBinaryPath = findBinaryInDirectory (trainBinaryPattern, dir = searchPath, patterns = list ('budgetedsvm-train .options. train_file .model_file.'), verbose = verbose)
	x$testBinaryPath = findBinaryInDirectory (testBinaryPattern, dir = searchPath, patterns = list ('budgetedsvm-predict .options. test_file model_file output_file'), verbose = verbose)

	return(x)
}



print.LLSVM = function(x) {
	cat("Solver: ", x$method)
	cat("    Training Binary at ", x$trainBinaryPath)
	cat("    Test Binary at ", x$testBinaryPath)
}

