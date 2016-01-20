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
 

	
#' Write LIBSVM model.
#'
#' Given a model in memory, write it out in LIBSVM model format, so that e.g. LIBSVM can predict on it directly.
#' 
#' @param	model		model object to write
#' @param	modelFile		path where to write the model
#' @param	verbose		be verbose?
#'
#' @note 		As this is a basic for all other model readers, we export it.
#'
#' @export	writeLIBSVMModel
writeLIBSVMModel = function (model = NA, modelFile = "./model", verbose = FALSE) {

	checkmate::assertFlag (verbose)
	checkmate::assertString (modelFile)
	checkmate::assertClass (model, "SVMModel")

	# check the model if everything is as we expect it
	# here: alpha, label... gamma,..
	
	if (verbose == TRUE) {
		cat("Writing SVM Model to ", modelFile, "\n")
	}
	
	
	model$nrclass = length(model$label)
	posSV = sum(model$alpha > 0)
	negSV = sum(model$alpha < 0)

	# open connection
	if (verbose == TRUE)
		cat ("    Writing Header.\n")
	modelFileHandle = file(modelFile, open = "w+")

	# TODO: allow for other kernels probably?
	writeLines(paste ("svm_type c_svc", sep = ""), modelFileHandle )
	writeLines(paste ("kernel_type", "rbf", sep = " "), modelFileHandle )
	
	gammaValue = model$gamma
	if(is.numeric(gammaValue)) {
		writeLines(paste ("gamma", model$gamma, sep = " "), modelFileHandle )
	} else { 
		stop ("Gamma is not numeric?")
	}
	
	writeLines(paste ("nr_class", model$nrclass, sep = " "), modelFileHandle )
	writeLines(paste ("total_sv", sum(model$nSV), sep = " "), modelFileHandle )
	biasvalues = paste(model$bias, collapse = " ")
	writeLines(paste ("rho", biasvalues, sep = " "), modelFileHandle )
	labelvalues = paste(model$label, collapse = " ")
	writeLines(paste ("label",labelvalues,  sep = " "), modelFileHandle )
	nr_svValues = paste(model$nSV,  collapse =  " ")
	writeLines(paste ("nr_sv", nr_svValues, sep = " "), modelFileHandle )
	writeLines(paste ("SV", sep = ""), modelFileHandle )

	# close connection
	close(modelFileHandle)

	# basically all data is sparse data format, but the data around this differs
	if (verbose == TRUE)
		cat ("    Writing Support Vectors.\n")
	appendSparseDataToFile (modelFile, model$SV, model$alpha, verbose = verbose)

	if (verbose == TRUE)
		cat ("    Done.\n")
	
}
 
