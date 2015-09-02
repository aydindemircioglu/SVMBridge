#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		detectMethodFromFile.R
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


#' detectModelTypeFromFile
#'		auto detect type of SVM model
#' 
#' @param	modelFile		path to model file
#' @param	verbose		be verbose?
#' @return	method name or NULL if no method could be detected.
#'
detectModelTypeFromFile <- function(
	modelFile = NULL,
	verbose = FALSE
)
{
	line = readLines(modelFile, 64)

	# BSGD
	pattern = "KERNEL_GAMMA_PARAM:"
	if (sum(grepl(pattern, line)) > 0) {
		method = "BSGD/LLSVM"
		cat(method, "\n")
		return (method)
	}

	# SVMperf/light
	pattern = "SVM-light"
	if (sum(grepl(pattern, line)) > 0) {
		method = "SVMLight"
		cat(method, "\n")
		return (method)
	}

	# LibSVM
	pattern = "total_sv"
	if (sum(grepl(pattern, line)) > 0) {
		method = "LIBSVM"
		cat(method, "\n")
		return (method)
	}

	return (NULL)
}
