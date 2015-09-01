#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		detectModelTypeFromFile.R
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


#' Detect model type from file.
#' 
#' This will auto detect the type of SVM model from a given file.
#' 
#' @param	modelFile		path to model file
#' @param	verbose		be verbose?
#'
#' @return	method name or NULL if no method could be detected.
#'
#' @export
#'
detectModelTypeFromFile <- function(
	modelFile = NULL,
	verbose = FALSE
)
{
	checkmate::checkFile (modelFile)
	checkmate::checkFlag (verbose)
	line = readLines(modelFile, n = 12)

	# BSGD
	method = NULL
	if (sum(grepl("KERNEL_GAMMA_PARAM:", line)) > 0) {
		method = "BSGD"
	} else if (sum(grepl("SVM-light", line)) > 0) {
		method = "SVMperf"
	} else if (sum(grepl("total_sv", line)) > 0) {
		method = "LIBSVM"
	}

	if (verbose == TRUE)
		cat("Found model Type: ", method, "\n")
	return (method)
}
