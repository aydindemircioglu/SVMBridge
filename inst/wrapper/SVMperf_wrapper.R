#
# SVMBridge
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



createTrainingArguments.SVMperf = function (x,
	trainDataFile = "",
	modelFile = "",
	kernelCacheSize = 1024,
	cost = 1,
	gamma = 1,
	k = 1000,
	epsilon = 0.001,
	extraParameter,
	verbose = FALSE,
	...)  {


	# count training examples
	N = R.utils::countLines(trainDataFile)

	# modify COST to C*N/100, as the formulation is C/n instead of C,
	# svmperf command tells us:
	# NOTE: The '-c' parameters in SVM-light and SVM-perf are related as
	#    c_light = c_perf*100/n for the 'Errorrate' loss function, where n is the
	#    number of training examples.
	cost = cost*N/100

	# even more stuff from the webpage:
	# -----
	# For training non-linear SVMs with kernels, SVMperf employs the Cutting-Plane Subspace Pursuit (CPSP) algorithm [Joachims & Yu, 2009].
	# The algorithm is enabled with the '--i' option. It allows you to limit the number of  basis functions (i.e. support vectors) via the '--k'
	# option. The larger '--k', the better the approximation quality but the longer training and testing times. Typically, you enable CPSP via
	# '--i 2 -w 9 --b 0 --k 500' and set the kernel parameters via '-t' just like in SVMlight. Currently, the full CPSP algorithm is implemented
	# only for RBF kernels.
	#
	# Another option for training non-linear SVMs with kernels is the Nystrom method ('--i 0 --t 1 -w 3') and the incomplete Cholesky
	# factorization ('--i 0 --t 2 -w 3'). Again, you can limit the number of basis functions via the --k option. The larger '--k', the better
	# the approximation quality but the longer training and testing times. Typically, you enable Nystrom (and incomplete Cholesky respectively)
	# via '--i 0 --t 1 -w 3 --b 0 --k 500' and set the kernel parameters via '-t' just like in SVMlight. Normally, the basis functions are sampled
	# from the training set, but you can give a file with an explicit set of basis function using the '--f' option. The file has the same format
	# as a training/test file.
	#
	# You can in principle also train non-linear kernels in SVMperf  exactly using '--t 0 --i 0 -w 3', and setting the kernel options just like
	# in SVMlight. However, this is painfully slow.
	# ------
	# we will adopt the first paragraph, our default settings (-l 0, --i 0 default, -w 4) were.. painfully slow. here -w 3 seems faster than -w 4!
	#

	# need to unpack the cost from the ... parameters
	eP = list(...)
	if (is.null (eP[["submethod"]])) {
		method = "CPSP"    }
	else {
		method = eP[["submethod"]]
	}


	args = list()
	if (method == "Nystrom")    {
		# DEFAULT values are commented out!
		args = c(
			sprintf("-c %.16f", cost),
			sprintf("-m %d", kernelCacheSize), # in MB
			#"-p 1",                            # use 1-norm for xi
			#"-o 2",                            # in 0/1 case, which we are interested in, both formulations are the same. o 2=margin rescaling is default.
			"-l 2",                             # 0/1 loss
			"-w 3",                             # 1-slack algorithm is superior, one of the main points of the paper. use cache to make it faster
			sprintf("-e %.16f", epsilon),          # epsilon
			#"-k 100",                          # default cache size before recomputing QP problem
			#"-b 100",                          # unsure of its meaning, default value.
			#"-f 10",                           # default constraint cache size for every example is 5, but paper states: 10 in section 5.1

			# not changing any svm light parameters
			"-t 2",                             # RBF kernel
			sprintf("-g %.16f", gamma),
			"--b 0.0",                          # for non-linear kernels bias must be zero here
			"--t 1",
			"--i 0",
			sprintf("--k %d", k),               # number of kernel functions, 500 by default
			# --r
			# --s
			extraParameter,
		    shQuote(trainDataFile),
		    shQuote(modelFile)
		)
	}

	if (method == "Cholesky")    {
		# DEFAULT values are commented out!
		args = c(
			sprintf("-c %.16f", cost),
			sprintf("-m %d", kernelCacheSize), # in MB
			#"-p 1",                            # use 1-norm for xi
			#"-o 2",                            # in 0/1 case, which we are interested in, both formulations are the same. o 2=margin rescaling is default.
			"-l 2",                             # 0/1 loss
			"-w 3",                             # 1-slack algorithm is superior, one of the main points of the paper. use cache to make it faster
			sprintf("-e %.16f", epsilon),          # epsilon
			#"-k 100",                          # default cache size before recomputing QP problem
			#"-b 100",                          # unsure of its meaning, default value.
			#"-f 10",                           # default constraint cache size for every example is 5, but paper states: 10 in section 5.1

			# not changing any svm light parameters
			"-t 2",                             # RBF kernel
			sprintf("-g %.16f", gamma),
			"--b 0.0",                          # for non-linear kernels bias must be zero here
			"--t 2",
			"--i 0",
			sprintf("--k %d", k),               # number of kernel functions, 500 by default
			# --r
			# --s
			extraParameter,
		    shQuote(trainDataFile),
		    shQuote(modelFile)
		)
	}

	if (method == "vanilla")    {
		# DEFAULT values are commented out!
		args = c(
			sprintf("-c %.16f", cost),
			sprintf("-m %d", kernelCacheSize), # in MB
			#"-p 1",                            # use 1-norm for xi
			#"-o 2",                            # in 0/1 case, which we are interested in, both formulations are the same. o 2=margin rescaling is default.
			sprintf("-e %.16f", epsilon),          # epsilon
			#"-k 100",                          # default cache size before recomputing QP problem
			#"-b 100",                          # unsure of its meaning, default value.
			#"-f 10",                           # default constraint cache size for every example is 5, but paper states: 10 in section 5.1

			# not changing any svm light parameters

			# FAST, like SVMperf 2.5: --t 1 --i 0
			# VERY  erratic: --t 0 --i 0

			"-t 2",                             # RBF kernel
			sprintf("-g %.16f", gamma),
			"--b 0.0",                          # for non-linear kernels bias must be zero here
#            sprintf("--k %d", k),               # number of kernel functions, 500 by default
			# --r
			# --s
			extraParameter,
		    shQuote(trainDataFile),
		    shQuote(modelFile)
		)
	}

	if (method == "CPSP_w3")    {
		# DEFAULT values are commented out!
		args = c(
			sprintf("-c %.16f", cost),
			sprintf("-m %d", kernelCacheSize), # in MB
			#"-p 1",                            # use 1-norm for xi
			#"-o 2",                            # in 0/1 case, which we are interested in, both formulations are the same. o 2=margin rescaling is default.
			"-l 0",                             # 0/1 loss
			"-w 3",                             # 1-slack algorithm is superior, one of the main points of the paper. use cache to make it faster
			sprintf("-e %.16f", epsilon),          # epsilon
			#"-k 100",                          # default cache size before recomputing QP problem
			#"-b 100",                          # unsure of its meaning, default value.
			#"-f 10",                           # default constraint cache size for every example is 5, but paper states: 10 in section 5.1

			# not changing any svm light parameters

			# FAST, like SVMperf 2.5: --t 1 --i 0
			# VERY  erratic: --t 0 --i 0

			"-t 2",                             # RBF kernel
			sprintf("-g %.16f", gamma),
			"--b 0.0",                          # for non-linear kernels bias must be zero here
			"--t 0",
			"--i 2",
			sprintf("--k %d", k),               # number of kernel functions, 500 by default
			# --r
			# --s
			extraParameter,
		    shQuote(trainDataFile),
		    shQuote(modelFile)
		)
	}

	if (method == "CPSP")    {
		# DEFAULT values are commented out!
		args = c(
			sprintf("-c %.16f", cost),
			sprintf("-m %d", kernelCacheSize), # in MB
			#"-p 1",                            # use 1-norm for xi
			#"-o 2",                            # in 0/1 case, which we are interested in, both formulations are the same. o 2=margin rescaling is default.
			"-l 2",                             # 0/1 loss
			"-w 9",                             # 1-slack algorithm is superior, one of the main points of the paper. use cache to make it faster
			sprintf("-e %.16f", epsilon),          # epsilon
			#"-k 100",                          # default cache size before recomputing QP problem
			#"-b 100",                          # unsure of its meaning, default value.
			#"-f 10",                           # default constraint cache size for every example is 5, but paper states: 10 in section 5.1

			# not changing any svm light parameters

			"-t 2",                             # RBF kernel
			sprintf("-g %.16f", gamma),
			"--b 0.0",                          # for non-linear kernels bias must be zero here
			"--i 2",
			sprintf("--k %d", k),               # number of kernel functions, 500 by default
			# --r
			# --s
			extraParameter,
		    shQuote(trainDataFile),
		    shQuote(modelFile)
		)
	}

	if (length(args) < 1) {
		stop ("Unknown method selected for SVMperf. Please use something like CPSP, CPSP_w3, etc.")
	}

	if (verbose == TRUE) {
		cat("SVMperf will apply method ", method, "\n")
	}

	return (args)
}



createTestArguments.SVMperf = function (x, testDataFile = NULL, modelFile = NULL, predictionsFile = NULL, verbose = FALSE, ...) {
	args = c(
		shQuote (testDataFile),
	    shQuote (modelFile),
	    shQuote (predictionsFile)
	)

	return (args)
}



extractTrainingInfo.SVMperf = function (x, output, verbose) {
	pattern <- "Accuracy :\\s*(\\d+\\.?\\d*)"
	err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
}



extractTestInfo.SVMperf = function (x, output, verbose) {
	pattern <- "Accuracy :\\s*(\\d+\\.?\\d*)"
	err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
}



readModel.SVMperf <- function (x, modelFile = "./model", verbose = FALSE)
{
	if (verbose == TRUE) {
		cat ("Reading SVMperf model from ", modelFile, "\n")
	}
	# open connection
	con  <- file(modelFile, open = "r")

	# grep needed information step by step, the bias is on the threshold line
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
		if (grepl("threshold", oneLine) == TRUE)
			break;

		# gamma value
		if (grepl("parameter -g", oneLine) == TRUE) {
			pattern <- "(.*) # kernel.*"
			gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)]))
		}
	}

	# the last line read should then contain the bias/threshold
	if (grepl("threshold", oneLine) == TRUE) {
	pattern <- "(.*) # threshold.*"
	bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)]))
	}


	# read and interprete data
	# basically all data is sparse data format, but the data around this differs
	model = readSparseDataFromConnection(con)

	# rename Y to alpha and X to SVs
	names(model) = replace(names(model), names(model) == "Y", "alpha")
	names(model) = replace(names(model), names(model) == "X", "SV")
	model$nSV = nrow(model$SV)

	# add header information
	model$gamma = gamma
	model$bias = bias
	model$modelType = "SVMperf"

	model$nSV	= c(sum(model$alpha[,1] > 0), sum(model$alpha[,1] < 0))
	model$label = as.numeric(c(1, -1)) # SVMperf is always binary?

	# close connection
	close(con)

	# this is only true for multiclass, but SVMperf does not support multiclass--
	# we must sort the alphas, else computation of optimize values will not work
	# as LIBSVMs alphas are sorted and prediction depend on this (because of multiclass
	# and the special format LIBSVM saves the alphas)
	sortSVs = FALSE
	if (sortSVs == TRUE) {
		s = sort (model$alpha, index.return = TRUE, decreasing = TRUE)
		model$alpha[,1] = s$x
		model$SV = model$SV[s$ix,]
	}

	# invert the alphas...
	#model$alpha[1:model$nSV[1],] = - model$alpha[1:model$nSV[1],]

	#model$alpha[(1+model$nSV[1]):nrow(model$alpha),] = - model$alpha[(1+model$nSV[1]):nrow(model$alpha),]


	# return
	return (model)
}



writeModel.SVMperf <- function (x, model = NA, modelFile = "./model", verbose = FALSE) {

	if (verbose == TRUE) {
		cat ("Writing SVMperf Model to ", modelFile, "\n")
	}

	# check of S3 object here.

	model$nrclass = length(model$label)
	posSV = sum(model$alpha > 0)
	negSV = sum(model$alpha < 0)

	# open connection
	if (verbose == TRUE)
		cat ("  Writing Header\n")
	modelFileHandle <- file(modelFile, open = "w+")
	writeLines ("SVM-light Version V6.20", modelFileHandle)
	writeLines("2 # kernel type", modelFileHandle) #kernel type
	writeLines("3 # kernel parameter -d ", modelFileHandle) # degree, not used here
	writeLines(paste(as.character(model$gamma), "# kernel parameter -g ", sep = " "), modelFileHandle )
	writeLines("1 # kernel parameter -s ", modelFileHandle ) #s?
	writeLines("1 # kernel parameter -r ", modelFileHandle ) #r?
	writeLines("empty # kernel parameter -u ", modelFileHandle ) #u?

	# highest feature index
	maxFeatIndex = ncol(model$SV)
	writeLines(paste(as.character(maxFeatIndex), "# highest feature index "), modelFileHandle ) #r?

	# FIXME: numer of training data-- we do not have this information..
	nTraining = ncol(model$SV)
	writeLines(paste(as.character(nTraining), " # number of training documents "), modelFileHandle ) #r?

	nr_svValues = nrow(model$SV)+1
	writeLines(paste(as.character(nr_svValues), " # number of support vectors plus 1 "), modelFileHandle )

	biasvalues = paste(model$bias, collapse = " ")
	writeLines(paste(as.character(biasvalues), "# threshold b, each following line is a SV (starting with alpha*y)"), modelFileHandle )

	# basically all data is sparse data format, but the data around this differs
	#svmatrix = dumpSparseFormat(model$alpha, model$X)
	#writeLines(svmatrix, modelFileHandle, sep = "" )

	# close connection
	close(modelFileHandle)

	if (verbose == TRUE)
		cat ("  Writing SV\n")
	appendSparseDataToFile (modelFile, model$SV, model$alpha)

}



detectModel.SVMperf = function (x, modelFile = NULL, verbose = FALSE) {
	checkmate::checkFlag (verbose)
	
	if (verbose == TRUE) {
		cat ("Checking for SVMperf model.\n")
	}
	
	if (is.null (modelFile) == TRUE)
		return (FALSE)

	# read first lines and detect magic marker
	if (file.exists (modelFile) == FALSE)
		return (FALSE)

	line = readLines(modelFile, n = 12)
	if (sum(grepl("SVM-light", line)) > 0) {
		return (TRUE)
	}

	return (FALSE)
}



readPredictions.SVMperf <- function (x, predictionsFile = "", verbose = FALSE) {
		ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
}



findSoftware.SVMperf = function (x, searchPath = "./", execute = FALSE, verbose = FALSE) {

	if (verbose == TRUE) {
		cat ("    SVMperf Object: Executing search for software for ", x$method)
	}

	# can do now OS specific stuff here
	if(.Platform$OS.type == "unix") {
		if (verbose == TRUE) {
			cat ("    Unix binaries.\n")
		}
		trainBinaryPattern = "svm_perf_learn"
		testBinaryPattern = "svm_perf_classify"
	} else {
		if (verbose == TRUE) {
			cat ("    Windows binaries.\n")
		}
		trainBinaryPattern = "svm_perf_learn.exe"
		testBinaryPattern = "svm_perf_classify.exe"
	}
	
	# can do now OS specific stuff here
	x$trainBinaryPath = findBinaryInDirectory (trainBinaryPattern, 
		dir = searchPath, patterns = list ('ROCArea: Percentage of swapped pos/neg pairs', 'usage: svm_struct_learn .options. example_file model_file'), verbose = verbose)
	x$testBinaryPath = findBinaryInDirectory (testBinaryPattern, dir = searchPath, patterns = list ('usage: svm_struct_classify .options. example_file model_file output_file'), verbose = verbose)

	return(x)
}



print.SVMperf  = function(x) {
	cat("--- Object: ", x$method, "\n")
	cat("       Training Binary at ", x$trainBinaryPath, "\n")
	cat("       Test Binary at ", x$testBinaryPath, "\n")
}

