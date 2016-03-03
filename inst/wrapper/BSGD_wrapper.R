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



createTrainingArguments.BSGD = function (x,
	trainDataFile = "",
	modelFile = "",
	extraParameter = "",
	cost = 1,
	gamma = 1,
	budget = 128,
	epochs = 1, ...) {

	n = R.utils::countLines(trainDataFile)

	# arguments for training
	args = c(
		"-A 4",
		"-r 0",
		sprintf("-B %.16f", budget ),
		sprintf("-L %.16f", (1.0 / (n * cost))),
		sprintf("-e %.16f", epochs ),
		sprintf("-g %.16f", 2 * gamma),
		extraParameter,
		shQuote (trainDataFile),
		shQuote (modelFile)
	)
	return (args)
}



createTestArguments.BSGD = function (x, testDataFile = NULL, modelFile = NULL, predictionsFile = NULL, verbose = FALSE, ...) {
    args = c(
		"-v 1",
		shQuote (testDataFile),
		shQuote (modelFile),
		predictionsFile
	)

	return (args)
}



extractTrainingInfo.BSGD = function (x, output, verbose) {
	pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
	err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
	return (err)
}



extractTestInfo.BSGD = function (x, output, verbose) {
	pattern <- ".*Testing error rate: (\\d+\\.?\\d*).*"
	err = as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
	return (err)
}



readModel.BSGD = function (x, modelFile = "./model", verbose = FALSE, singleBinaryColumn = FALSE) {
	if (verbose == TRUE) {
		cat("Reading BSGD model from ", modelFile, "\n")
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

		if (grepl("NUMBER_OF_WEIGHTS", oneLine) == TRUE) {
			pattern <- "NUMBER_OF_WEIGHTS: (.*)"
			totalSV = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)]))
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

			if (order == "1 -1") {
				invertLabels = FALSE
			}

			if (order == "-1 1") {
				invertLabels = TRUE
			}

			labels = unlist(strsplit(order, split = "\\s"))
		}
	}


	# read and interprete data
	# basically all data is sparse data format, but the data around this differs
	# 	model = readSparseFormat(con)

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

		# read part for part until it is something positive
		for (i in seq(1, length(parts[[1]]))) {
			fparts <- strsplit (parts[[1]][i], ":")
			if (!is.na(fparts[[1]][1])) {
				ind = as.numeric(fparts[[1]][1])
				value = as.numeric(fparts[[1]][2])

				# check if we have part of some feature vector
				if (ind > 0) {
					# yes, so quit the whole loop
					fvpos = i
					break
				}
				# if not, we can save it in the coeff container, which contains alpha values with negative index
				coeff[-ind] = value
			}
			else {
				stop ("Should never happen. Really.")
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
	model$modelType = "BSGD"
	model$label = as.numeric(labels)

	if (length(model$label) == 2) {
		# this is stupid. the model can contain a NON-SUPPORTVECTOR with all alpha=0. probably because
		# using double and saving single. :/
		model$nSV	= c(sum(model$alpha[,1] >= 0), sum(model$alpha[,1] < 0))
	} else {
		# count total number of svs 
		model$nSV = model$label * 0
		for (i in 1:nrow(model$alpha)) {
			for (c in 1:length(model$nSV)) {
				if (model$alpha[i, c] == 0) {
					model$nSV[c] = model$nSV[c] + 1
				}
			}
		}
	}

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

	# remove extra row if necessary
	if (singleBinaryColumn == TRUE) {
		if (length (model$label) == 2) {
			model$alpha = model$alpha[,1]
			if (verbose == TRUE)
				cat ("  Removing Column of extra alpha values.\n")
		}
	}

	# return
	return (model)
}



writeModel.BSGD = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
	if (verbose == TRUE) {
		cat ("Writing BSGD Model..\n")
	}

	dim = ncol (model$SV)
	nClasses = length (model$nSV)
	nWeights = nrow (model$SV)
	gamma = model$gamma * 2
	degree = 1
	coef = 1


	# open connection
	modelFileHandle <- file(modelFile, open = "w+")
	writeLines(paste ("ALGORITHM: 4", sep = ""), modelFileHandle )
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

	# basically all data is sparse data format, but the data around this differs
	dumpSparseLine = function (row, invertIndex = FALSE) {
		sparseLine = ''
		for (x in seq(1, length(row))) {
			if (row[x] != 0) {
				if (invertIndex == TRUE)
					idx = -x
				else idx = x
				sparseLine = paste(sparseLine, paste(idx, row[x], sep =":"), sep = " ")
			}
		}
		return(sparseLine)
	}

	for (r in seq(1, nrow(model$SV))) {
		sparseLine = paste(dumpSparseLine (model$alpha[r,], invertIndex = TRUE), " ", dumpSparseLine (model$SV[r,]), "\n", sep = '')
		writeLines(sparseLine, modelFileHandle, sep = "" )
	}

	# close connection
	close(modelFileHandle)
}



detectModel.BSGD = function (x, modelFile = NULL, verbose = FALSE) {
	checkmate::checkFlag (verbose)
	
	if (verbose == TRUE) {
		cat ("Checking for BSGD model.\n")
	}

	if (is.null (modelFile) == TRUE)
		return (FALSE)

	if (file.exists (modelFile) == FALSE)
		return (FALSE)

	line = readLines(modelFile, n = 1)
	if (line == "ALGORITHM: 4") {
		return (TRUE)
	}

	return (FALSE)
}



readPredictions.BSGD <- function (x, predictionsFilePath = "", verbose = FALSE) {
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



findSoftware.BSGD = function (x, searchPath = "./", execute = FALSE, verbose = FALSE) {
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



print.BSGD = function(x) {
	cat("Solver: ", x$method)
	cat("    Training Binary at ", x$trainBinaryPath)
	cat("    Test Binary at ", x$testBinaryPath)
}
