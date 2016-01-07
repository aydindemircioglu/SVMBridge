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
 

#' readSparseFile
#'		reads sparse data from a file
#' @param	fileName		path to the file
#' @param	verbose		report helpful messages?
#' @importMethodsFrom		SparseM		as.matrix.csr
#'
#' @export	readSparseFile
readSparseFileE1071 <- function (fileName = '', verbose = FALSE) {

	if (verbose == TRUE) {
		BBmisc::messagef("Loading sparse data from file %s.", fileName)
	}
	
	# load matrix
    dataset <- e1071::read.matrix.csr (fileName)

	# convert the label 
	y = as.numeric(as.character(dataset$y))
	X = as.matrix.csr(dataset$x)

	return (list("X" = X, "y" = y))
}
 



# FIXME: RENAME
#' readSparseFormat
#'		
#' @param	con		connection to read from
#'
#' @return	list of supportvectors, coefficients and weights
readSparseFormat <- function (con)
{
  # these will contain the coefficients and the  svs.
  supportvectors <- matrix()
  coefficients <- matrix()
  weights <- matrix()
  
  # read file one by one
  currentIndex = 0
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    
    # if we are at libcvm or libbvm we have to skip the very last line!
    # unluckily in an while loop like this there is no way to know that beforehand
    # so we grep for the "CPUTime" line explicitly
    
    # remove comment if necesary
    oneLine = stringr::str_split_fixed(oneLine, pattern = '#', n = 2)[1]
    
    # split line by " "
    svec = vector(length = 1)
    parts = strsplit (oneLine, " ")
    
    # where the support vector data starts in the row
    fvpos = 1
    coeff = vector(length = 1)
    w = vector (length = 1)

    # grep coefficient
      coeff[1] = as.numeric(parts[[1]][1])
      fvpos = 2
    
    # grep feature vectors one by one
    for (i in fvpos:length(parts[[1]])) {
      # split by :
      fparts <- strsplit (parts[[1]][i], ":")
      
      # if we have anything, add it to our vector
      if (!is.na(fparts[[1]][1])) {
        ind = as.numeric(fparts[[1]][1])
        value = as.numeric(fparts[[1]][2])
        svec[ind] <- value
      }
    }
    
    # make sure our vector has no NAs
    #print (svec)
    svec[is.na(svec)] <- 0
    
    # stack matrices
    supportvectors <- plyr::rbind.fill.matrix(supportvectors, t(svec))
    coefficients <- plyr::rbind.fill.matrix(coefficients, t(coeff))
    weights <- plyr::rbind.fill.matrix(weights, t(w))
  } 
 
  # crop first NA list (why does it even exist?..)
  supportvectors = supportvectors[-1, ]
  coefficients = coefficients[-1, ]
  weights = weights[-1, ]
  
  # remove possible NA values that occur if the very last
  # entry of a sparse vector is omitted because of sparsity
  supportvectors[is.na(supportvectors)] <- 0
  coefficients[is.na(coefficients)] <- 0 
  weights[is.na(weights)] <- 0 

  return (list("X" = supportvectors, "a" = coefficients, "w" = weights))
}


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


#' dumpSparseFormat
#' 
#' @param	labels		container for labels
#' @param	data		container for data
#' @return	sparseLine		string with all collected data
#'
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



#' read spase data from an open connection 
#'
#' @param	con		connection
#' @param 	verbose		be verbose?
#' @param	zeroBased		do the indices in the file start with 0, e.g. -1 0:2 1:4 ...?
#' @return	L		list containing input data and labels
#'
#' @export	readSparseDataFromConnection
readSparseDataFromConnection = function (con, verbose = FALSE, zeroBased = FALSE) {
    # where are we?
    currentPosition = seek(con)

    # get filename
    filename = summary(con)$description

    # call readSparseData skipping to the current Position
    L = readSparseData (filename, skipBytes = currentPosition, verbose = verbose, zeroBased = zeroBased)

    return (L)
}

#' writeSparseDataToConnection
#'
#' @param	con		connection
#' @param 	X		input data
#' @param	Y		labels
#' @param	verbose		be verbose?
#' @param	zeroBased		do the indices in the file start with 0, e.g. -1 0:2 1:4 ...?
#'
writeSparseDataToConnection = function (con, X, Y, verbose = FALSE, zeroBased = FALSE) {
	currentPosition = seek(con)
	filename = summary(con)$description
	writeSparseData(filename, X, Y, skipBytes = currentPosition, verbose = verbose, zeroBased = zeroBased)
}


# stupid R check for pythons cool "name == __main__"
if (length(sys.frames()) == 0) 
{

}
