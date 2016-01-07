#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		sparseFormat.R
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
 

#' Read Sparse File via E1071.
#'
#'	Reads sparse data from a file via e1071 library and return it as a dense matrix/vector.
#' 
#' @param	fileName		path to the file
#' @param	verbose		report helpful messages?
#' 
#' @importMethodsFrom		SparseM		as.matrix.csr
#'
#' @export
readSparseFileE1071 <- function (fileName = '', verbose = FALSE) {

	if (verbose == TRUE) {
		cat("Loading sparse data from file ", fileName, "\n")
	}
	
	# load matrix
    dataset <- e1071::read.matrix.csr (fileName)

	# convert the label 
	y = as.numeric(as.character(dataset$y))
	X = as.matrix.csr(dataset$x)

	return (list("X" = X, "y" = y))
}
 