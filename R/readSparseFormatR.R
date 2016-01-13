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
 


#' Read SparseFormat via R.
#'
#' Will read a sparse format file via R routines. Will nearly always be slower than the C++ method.
#'		
#' @param	con		connection to read from
#'
#' @return	list of supportvectors, coefficients and weights
#'
#' @export
readSparseFormatR = function (con)
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
