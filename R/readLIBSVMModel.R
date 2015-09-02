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
 

#' readLIBSVMModel
#' 		Read LIBSVM model
#'
#' 
#' @param	modelFile		model file to read
#' @param	verbose		be verbose?
#' @note	As this is a basic for all other model readers, we export it.
#'
#' @return	model object
#'
#' @export
readLIBSVMModel = function (modelFile = './model', verbose = FALSE) {
	if (verbose == TRUE) {
		BBmisc::messagef ("Reading LIBSVM model from %s.", modelFile)
	}
	
	# open connection
	con  <- file(modelFile, open = "r")
	oneLine = readLines(con, n = 1, warn = FALSE)
	while ( !is.na(oneLine) && oneLine != "SV") {
		# gamma value
		if (grepl("gamma", oneLine) == TRUE) {
			pattern <- "gamma (.*)"
			gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
		}  
	
		# rho/bias
		if (grepl("rho", oneLine) == TRUE) {
			#print(oneLine)
			bias = numeric()
			rhoLine = unlist(strsplit(oneLine, split = "\\s"))
			for(value in rhoLine){
				if(value != "rho"){
					bias = c(bias, value)
				}
			}
		}
		
		# order of labels
		if (grepl("label", oneLine) == TRUE) {
			label = numeric()
			labelLine = unlist(strsplit(oneLine, split = "\\s"))
			for(value in labelLine){
				if(value != "label")
					label = c(label, value)
			}
		}  

		# number of svs, needed for multiclass (i think)
		if (grepl("nr_sv", oneLine) == TRUE) {
			nSV = numeric()
			nSVline = unlist(strsplit(oneLine, split = "\\s"))
			for(value in nSVline ){
				if(value != "nr_sv")
					nSV = c(nSV, value)
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
	svmatrix = readSparseDataFromConnection(con)
	
	# rename Y to alpha and X to SVs
	names(svmatrix) = replace(names(svmatrix), names(svmatrix) == "Y", "alpha")
	names(svmatrix) = replace(names(svmatrix), names(svmatrix) == "X", "SVs")
	svmatrix$nSV = nrow(svmatrix$SVs)
	
	# FIXME: i think this exists ansatz is broken if gamma exists beforehand. replace with a list or something
	
	# add header information
	if(exists("gamma"))
		svmatrix$gamma = gamma
	if(exists("label"))
		svmatrix$label = as.numeric(label)
	if(exists("bias"))
		svmatrix$bias = as.numeric(c(bias))
	if(exists("nSV"))
		svmatrix$nSV = as.numeric(c(nSV))
	svmatrix$modeltype = "LIBSVM"
	
	# close connection
	close(con)

	# return
#	retObj = BBmisc::makeS3Obj("SVMModel", svmatrix)
	return (svmatrix)
}


