#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		SVMperf_wrapper.R
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
 

#' evalSVMperf
#'
#' @param     ...		parameters that will be passed on training and/or test callbacks

#' SVMperfTrainingParameterCallBack
#'
#' @param     ...		parameters that will be passed on training and/or test callbacks


createTrainingArguments.SVMperf = function (
		x,
		trainDataFile = "",
        modelFile = "",
        kernelCacheSize = 1024,
		cost = 1, 
        gamma = 1,
		k = 1000,
        epsilon = 0.001, 

		saveFactor = -1,
		saveExponential = -1,
		modelPath = -1,
		wallTime = -1,
        primalTime = -1,
        extraParameter = "",
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
        
    # ---  take care of primal/wall time, will not be added if its turned off. 
    primalTimeParameter =  sprintf("-a %d", floor(primalTime))

    if (primalTime == -1)
        primalTimeParameter = ""

    wallTimeParameter =  sprintf("-L %d", floor(wallTime))

    if (wallTime == -1)
        wallTimeParameter = ""


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
			primalTimeParameter,
			wallTimeParameter,
            extraParameter,
            trainDataFile,
            modelFile
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
			primalTimeParameter,
			wallTimeParameter,
			extraParameter,
            trainDataFile,
            modelFile
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
			primalTimeParameter,
			wallTimeParameter,
            extraParameter,
            trainDataFile,
            modelFile
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
			primalTimeParameter,
			wallTimeParameter,
            extraParameter,
            trainDataFile,
            modelFile
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
			primalTimeParameter,
			wallTimeParameter,
            extraParameter,
            trainDataFile,
            modelFile
        )
    }

    if (length(args) < 1) {
        BBmisc::stopf ("Unknown method selected for SVMperf. Please use something like CPSP, CPSP_w3, etc.")
    }
    
    BBmisc::messagef ("SVMperf will apply method %s.", method)

    return (args)
}



createTestArguments.SVMperf = function (x,
										testDataFile = "",
                                        modelFile = "",
                                        predictionsFile = "", 
                                        ...) {
    args = c(
        testDataFile,
        modelFile,
        predictionsFile,
        "/dev/null"                     # outfile, not needed
    )
    
    return (args)
}
  


extractTrainingInfo.SVMperf = function (x, output) {

    # compute error
    pattern <- "Accuracy :\\s*(\\d+\\.?\\d*)"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
}

extractTestInfo.SVMperf = function (x, output) {

    # compute error
    pattern <- "Accuracy :\\s*(\\d+\\.?\\d*)"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
}



readModel.SVMperf <- function (x, modelFile = "./model", verbose = FALSE)
{
	if (verbose == TRUE) {
			BBmisc::messagef ("Reading SVMperf model from %s.", modelFile)
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
	
	# return
	return (model)
}



# DUMMY
writeModel.SVMperf <- function (x, model = NA, modelFile = "./model", verbose = FALSE) {
	ret = writeModel.LIBSVM (model = model, modelFile = modelFile, verbose = verbose)
		return (ret)
}
 

#
# @param[in]	predictionsFile		file to read predictions from
# @return		array consisting of predictions
#

# DUMMY
readPredictions.SVMperf <- function (x, predictionsFile = "", verbose = FALSE) {
		ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
    
}


findSoftware.SVMperf = function(x, searchPath = "./", verbose = FALSE) {
		if (verbose == TRUE) {
			BBmisc::messagef("    SVMperf_wrapper Object: Executing search for software for %s", x$method)
		}
		
		trainBinaryPattern = "^svm_perf_learn$"
		trainBinaryOutputPattern = c(
			'saveExponential : set exponential of time interval in seconds',
			'usage: svm_struct_learn .options. example_file model_file')
			
		binaryPath = findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)
		
		


		if (verbose == TRUE) {
			BBmisc::messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^svm_perf_classify$"
		testBinaryOutputPattern = 'usage: svm_struct_classify .options. example_file model_file output_file'

		binaryPath = findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		if (verbose == TRUE) {
			BBmisc::messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
}



#' print.SVMperf
#'
#' @param	x			svm object
#'	
print.SVMperf  = function(x) {
	cat("--- Object: ", x$method, "\n")
	cat("       Training Binary at ", x$trainBinaryPath, "\n")
	cat("       Test Binary at ", x$testBinaryPath, "\n")
}
	
