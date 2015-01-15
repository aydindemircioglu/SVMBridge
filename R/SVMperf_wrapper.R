#!/usr/bin/Rscript  --vanilla 

source ("software/helpers/universalWrapper.R")

library(BBmisc)




evalSVMperf = function(...)  {   
    universalWrapper (
        modelName = "SVMperf",
        trainingParameterCallBack = SVMperfTrainingParameterCallBack,
        testParameterCallBack = SVMperfTestParameterCallBack,
        extractInformationCallBack  = SVMperfExtractInformationCallBack,
        trainBinary = SVMperfTrainBinary(),
        testBinary = SVMperfTestBinary (),
        bindir = SVMperfBinDir(),
        ...
    );
}



SVMperfTrainingParameterCallBack = function (trainfile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            cost = 1, 
                                            gamma = 1, 
                                            kernelCacheSize = 1024,
                                            k = 1000,
                                            epsilon = 0.001, ...) {

                       
    # count training examples
    N = countLines(trainfile)

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
    if (is.null (eP[["method"]])) {
        method = "CPSP"    }
    else {
        method = eP[["method"]]
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
            trainfile,
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
            trainfile,
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
            trainfile,
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
            trainfile,
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
            trainfile,
            modelFile
        )
    }

    if (length(args) < 1) {
        stopf ("Unknown method selected for SVMperf. Please use something like CPSP, CPSP_w3, etc.")
    }
    
    messagef ("SVMperf will apply method %s.", method)

    return (args)
}



SVMperfTestParameterCallBack = function (testfile = "",
                                        modelFile = "", ...) {
    args = c(
        testfile,
        modelFile,
        "/dev/null"                     # outfile, not needed
    )
    
    return (args)
}
  


SVMperfExtractInformationCallBack = function (output) {

    # compute error
    pattern <- "Accuracy :\\s*(\\d+\\.?\\d*)"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
}



SVMperfTrainBinary <- function() {
    return ("svm_perf_learn")
}


SVMperfTestBinary <- function() {
    return ("svm_perf_classify")
}


SVMperfBinDir <- function() {
    return ("software/SVMperf/bin/")
}



# stupid R check for pythons cool "name == __main__"
if (length(sys.frames()) == 0) 
{
#    file = "software/libSVM/src/heart_scale"
#    file = "datasets/australian/australian.combined.scaled"
    file = "datasets/arthrosis/arthrosis.combined.scaled"
#    file = "datasets/protein/protein.combined.scaled"

    err = evalSVMperf(file, file, cost = 19, gamma = 0.0496346230004575, epsilon = 0.1, k = 1000,
        primalTime = 1,
        wallTime = 66,
        verbose = TRUE,
        modelFile = "~/svmperf.model",
        method = "CPSP",
        computePrimal = FALSE
    )  

    messagef("Error rate: %s", err$err) 
    messagef("Dual: %s", err$dual) 
    messagef("Primal: %s", err$primal)
}


  



