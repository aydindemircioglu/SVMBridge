#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		readLIBSVMPredictions
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


# TODO:
# Read predictions from a file 
#


#' Read predictions produced by LIBSVM
#'
#' Read predictions produced by LIBSVM, i.e. each line one label.
#'
#' @param	predictionsFile		file to read predictions from
#' @param	verbose		be verbose?
#' 
#' @return		array consisting of predictions
#
#' @export
readLIBSVMPredictions = function (predictionsFile = "", verbose = FALSE) {
	if (verbose == TRUE) {
		cat ("Reading LIBSVM predictions from ", predictionsFile, "\n")
	}
	
	con  = file (predictionsFile, open = "r")

	predictions = c()
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
		predictions = c(predictions, as.numeric(oneLine))
	}
	
	if (verbose == TRUE) {
		print(predictions)
	}
			
	close (con)
	
	return (predictions)
}

