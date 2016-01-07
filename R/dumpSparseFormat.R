#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		dumpSparseFormat.R
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
 


#' dumpSparseLine
#' 
#' @param	row		array to read data from
#'
#' @return	sparseLine		string with row data 
#'
dumpSparseLine = function (row) {
	sparseLine = ''

	for (x in seq(1, length(row))) {
		if (row[x] != 0) {
			sparseLine = paste(sparseLine, paste(x, row[x], sep =":"), sep = " ")
		}
	}
	return(sparseLine)
}


#' Dump SparseFormat.
#'
#' Simple (debug) dump of a sparse format given by labels and data into a string.
#' 
#' @param	labels		container for labels
#' @param	data		container for data
#'
#' @return	sparseLine		string with all collected data
#'
#' @export
dumpSparseFormat <- function (labels, data)
{
	# TODO: sanity check for length of labels and data

	# TODO: speedup??
	sparseString = ''
	for (r in seq(1, nrow(data))) {
		sparseLine = dumpSparseLine (data[r,])
		sparseLine = paste(labels[r], sparseLine, sep = '')
		sparseLine = paste(sparseLine, "\n", sep = '')
		sparseString = paste(sparseString, sparseLine, sep = "")
	}

	return(sparseString)
}
