#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#	.LASVM_walltime_wrapper.R
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
 

	
	#' Write LIBSVM model
	#'
	#' As this is a basic for all other model readers, we export it.
	#' 
	#' @param	model		model object to write
	#' @param	modelFile		path where to write the model
	#' @param	verbose		be verbose?
	#'
	#' @export
	writeLIBSVMModel = function (model = NA, modelFile = "./model", verbose = FALSE) {
		if (verbose == TRUE) {
			BBmisc::messagef ("Writing SVM Model..")
		}
		
		#model$alpha = model$Y
		# FIXME: label order
		# TODO: support multiclass
		model$nrclass = length(model$label)
		posSV = sum(model$alpha > 0)
		negSV = sum(model$alpha < 0)
		# open connection
		modelFileHandle <- file(modelFile, open = "w+")
		writeLines(paste ("svm_type c_svc", sep = ""), modelFileHandle )
		writeLines(paste ("kernel_type", "rbf", sep = " "), modelFileHandle )
		gammaValue = model$gamma
		if(is.numeric(gammaValue))
			writeLines(paste ("gamma", model$gamma, sep = " "), modelFileHandle )
		writeLines(paste ("nr_class", model$nrclass, sep = " "), modelFileHandle )
		writeLines(paste ("total_sv", length(model$nSV), sep = " "), modelFileHandle )
		biasvalues = paste(model$bias, collapse = " ")
		writeLines(paste ("rho", biasvalues, sep = " "), modelFileHandle )
		labelvalues = paste(model$label, collapse = " ")
		writeLines(paste ("label",labelvalues,  sep = " "), modelFileHandle )
		nr_svValues = paste(model$nSV,  collapse =  " ")
		writeLines(paste ("nr_sv", nr_svValues, sep = " "), modelFileHandle )
		writeLines(paste ("SV", sep = ""), modelFileHandle )

		# basically all data is sparse data format, but the data around this differs
		#svmatrix = dumpSparseFormat(model$alpha, model$X)
		#writeLines(svmatrix, modelFileHandle, sep = "" )
		writeSparseDataToConnection(modelFileHandle, model$SVs, model$alpha)
		
		# close connection
		close(modelFileHandle)
	}
 

