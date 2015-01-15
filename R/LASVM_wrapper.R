#!/usr/bin/Rscript  --vanilla 



evalLASVM = function(...)  {   
    universalWrapper (
        modelName = "LASVM",
        trainingParameterCallBack = LASVMTrainingParameterCallBack,
        testParameterCallBack = LASVMTestParameterCallBack,
        extractInformationCallBack  = LASVMExtractInformationCallBack,
        trainBinary = LASVMTrainBinary(),
        testBinary = LASVMTestBinary (),
        bindir = LASVMBinDir(),
        ...
    );
}



LASVMTrainingParameterCallBack = function (trainfile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            kernelCacheSize = 1024,
                                            cost = 1, 
                                            useBias = FALSE,
                                            gamma = 1,
                                            epochs = 1,
                                            epsilon = 0.001, ...) {

    # count training examples
    N = countLines(trainfile)

    biasParameter = "-b 0"
    if (useBias == TRUE)
        biasParameter = "-b 1"

    args = c(
        sprintf("-m %d", kernelCacheSize), # in MB 
        biasParameter,
        sprintf("-g %.16f", gamma),
        sprintf("-c %.16f", cost), 
        sprintf("-e %.16f", epsilon),
        sprintf("-p %.16f", epochs),
        extraParameter,
        trainfile,
        modelFile
    )

    return (args)
}



LASVMTestParameterCallBack = function (testfile = "",
                                        modelFile = "", ...) {
    args = c(
        testfile,
        modelFile,
        "/dev/null"                     # outfile, not needed
    )
    
    return (args)
}



LASVMExtractInformationCallBack = function (output) {

    # maybe not the best way to grep the string
    pattern <- "accuracy= (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
}
  


LASVMTrainBinary <- function() {
    return ("la_svm")
}


LASVMTestBinary <- function() {
    return ("la_test")
}


LASVMBinDir <- function() {
    return ("software/LASVM/bin/")
}



LASVMReadModelCallBack <- function (modelFilePath = "./model", verbose = FALSE) {
	LIBSVMReadModelCallBack (modelFilePath = modelFilePath, verbose = verbose)
}



LASVMWriteModelCallBack <- function (model = NA, modelFilePath = "./model", verbose = FALSE) {
	LIBSVMWriteModelCallBack (model = model, modelFilePath = modelFilePath, verbose = verbose)
}
 

 
#
# @param[in]	predictionsFile		file to read predictions from
# @return		array consisting of predictions
#

LASVMPredictionsCallBack <- function (predictionsFilePath = "", verbose = FALSE) {
	return (LIBSVMPredictionsCallBack (predictionsFilePath = predictionsFilePath, verbose = verbose))
}
