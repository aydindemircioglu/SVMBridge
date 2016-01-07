#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		readSparseDataFromConnection .R
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
 

#' Read spase data from an open connection.
#'
#' @param	con		connection
#' @param 	verbose		be verbose?
#' @param	zeroBased		do the indices in the file start with 0, e.g. -1 0:2 1:4 ...?
#'
#' @return	L		list containing input data and labels
#'
#' @export
readSparseDataFromConnection = function (con, verbose = FALSE, zeroBased = FALSE) {
    # where are we?
    currentPosition = seek(con)

    # get filename
    filename = summary(con)$description

    # call readSparseData skipping to the current Position
    L = readSparseData (filename, skipBytes = currentPosition, verbose = verbose, zeroBased = zeroBased)

    return (L)
}
