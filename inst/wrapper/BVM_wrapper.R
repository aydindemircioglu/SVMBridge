#!/usr/bin/Rscript  --vanilla 

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
  
#source ("./universalWrapper.R")
createTrainingArguments.BVM = function (x, 
					trainDataFile = "",
                                            modelFile = "",
                                            extraParameter = "",
                                            primalTime = 10, 
                                            wallTime = 8*60,
                                            kernelCacheSize = 1024,
                                            cost = 1, 
                                            gamma = 1, 
                                            epsilon = 0.001, ...) {

    args = c(
        "-s 6",                         # CVM = 6, BVM = 9
        "-t 2",
        sprintf("-c %.16f", cost), 
        sprintf("-m %d", kernelCacheSize), # in MB 
        sprintf("-g %.16f", gamma),
        sprintf("-e %.16f", epsilon),
        extraParameter,
        trainDataFile,
        modelFile
    )

    return (args)
}



createTestArguments.BVM = function (x, testDataFile = "", modelFile = "", ...) {
    args = c(
        testDataFile,
        modelFile,
        "/dev/null"                     # outfile, not needed
    )
    
    return (args)
}



extractTrainingInfo.BVM = function (x, output) {

    # maybe not the best way to grep the string
    pattern <- "Accuracy = (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}


#NEW
extractTestInfo.BVM = function (x, output) {

    # maybe not the best way to grep the string
    pattern <- "Accuracy = (\\d+\\.?\\d*).*"
    err = 1 - as.numeric(sub(pattern, '\\1', output[grepl(pattern, output)])) / 100
    
    return (err)
}

#DUMMY
	readModel.BVM = function (x, modelFile = './model', verbose = FALSE) {
		ret = readModel.LIBSVM (modelFile = modelFile, verbose = verbose)
		return (ret)
	}
	
	
#DUMMY
	writeModel.BVM = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
		ret = writeModel.LIBSVM (model = model, modelFile = modelFile, verbose = verbose)
		return (ret)
	}
 
#
# @param[in]	predictionsFile		file to read predictions from
# @return		array consisting of predictions
#

#DUMMY
readPredictions.BVM = function (x, predictionsFile = "", verbose = FALSE) {
		ret = readPredictions.LIBSVM (predictionsFile = predictionsFile, verbose = verbose)
		return (ret)
	}

findSoftware.BVM = function (x, searchPath = "./", verbose = FALSE) {

		if (verbose == TRUE) {
			BBmisc::messagef("    BVM Object: Executing search for software for %s", x$method)
		}
		
		trainBinaryPattern = "^svm-train$"
		trainBinaryOutputPattern = 'bvm-train .options. training_set_file .model_file.'
		binaryPath = findBinary (searchPath, trainBinaryPattern, trainBinaryOutputPattern, verbose = verbose)

		# TODO: check for empty path+handling
		
		if (verbose == TRUE) {
			BBmisc::messagef("--> Found train binary at %s", binaryPath) 
		}
		x$trainBinaryPath = binaryPath


		testBinaryPattern = "^svm-predict$"
		testBinaryOutputPattern = "bvm-predict .options. test_file model_file output_file"

		binaryPath = findBinary (searchPath, testBinaryPattern, testBinaryOutputPattern, verbose = verbose)
		
		# TODO: check for empty path+handling

		if (verbose == TRUE) {
			BBmisc::messagef("--> Found test binary at %s", binaryPath) 
		}
		x$testBinaryPath = binaryPath

		return(x)
	}



