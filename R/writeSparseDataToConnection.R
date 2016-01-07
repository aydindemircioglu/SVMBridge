#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		writeSparseDataToConnection.R
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
 
 
#' writeSparseDataToConnection
#'
#' @param	con		connection
#' @param 	X		input data
#' @param	Y		labels
#' @param	verbose		be verbose?
#' @param	zeroBased		do the indices in the file start with 0, e.g. -1 0:2 1:4 ...?
#'
#' @export
writeSparseDataToConnection = function (con, X, Y, verbose = FALSE, zeroBased = FALSE) {
	currentPosition = seek(con)
	filename = summary(con)$description
	writeSparseData(filename, X, Y, skipBytes = currentPosition, verbose = verbose, zeroBased = zeroBased)
}

