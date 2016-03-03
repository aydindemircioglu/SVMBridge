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


#' Read a model in LIBSVM format
#'
#' This will simply read a model in LIBSVM format. As this is the base of many
#' other solvers, this is part of the library (instead of putting it into a wrapper)
#' and it uses Rcpp to read the support vectors for speeding up.
#'
#' @param	modelFile		model file to read
#' @param	verbose		be verbose?
#'
#' @return	model object

readLIBSVMModel = function (modelFile = './model', verbose = FALSE) {
	if (verbose == TRUE) {
		BBmisc::messagef ("Reading LIBSVM model from %s.", modelFile)
	}

	tmpModel = list()

	# open connection
	con  <- file(modelFile, open = "r")
	oneLine = readLines(con, n = 1, warn = FALSE)
	while ( !is.na(oneLine) && oneLine != "SV") {
		# gamma value
		if (grepl("gamma", oneLine) == TRUE) {
			pattern <- "gamma (.*)"
			tmpModel$gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)]))
		}

		# rho/bias
		if (grepl("rho", oneLine) == TRUE) {
			bias = numeric()
			rhoLine = unlist(strsplit(oneLine, split = "\\s"))
			for(value in rhoLine){
				if(value != "rho"){
					tmpModel$bias = as.numeric (c(tmpModel$bias, value))
				}
			}
		}

		# order of labels
		if (grepl("label", oneLine) == TRUE) {
			label = numeric()
			labelLine = unlist(strsplit(oneLine, split = "\\s"))
			for(value in labelLine){
				if(value != "label")
					tmpModel$label = as.numeric (c(tmpModel$label, value))
			}
		}

		# number of svs, needed for multiclass (i think)
		if (grepl("nr_sv", oneLine) == TRUE) {
			tmpModel$nSV = numeric()
			nSVline = unlist(strsplit(oneLine, split = "\\s"))
			for (value in nSVline ){
				if(value != "nr_sv")
					tmpModel$nSV = as.numeric(c(tmpModel$nSV, value))
			}
		}

		if (grepl("svm_type", oneLine) == TRUE) {
			pattern <- "rho (.*)"
			svm_type = sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])
		}
		oneLine = readLines(con, n = 1, warn = FALSE)
	}


	# read and interprete data
	# basically all data is sparse data format, but the data around this differs
	svmModel = readSparseDataFromConnection(con)

	# rename Y to alpha and X to SVs
	names(svmModel) = replace(names(svmModel), names(svmModel) == "Y", "alpha")
	names(svmModel) = replace(names(svmModel), names(svmModel) == "X", "SV")
	svmModel$nSV = nrow(svmModel$SV)


	# add header information
	svmModel = c(tmpModel, svmModel)
	svmModel$modelType = "LIBSVM"

	# close connection
	close(con)

	# return
	class (svmModel) = append(class(svmModel),"SVMModel")
	return (svmModel)
}
