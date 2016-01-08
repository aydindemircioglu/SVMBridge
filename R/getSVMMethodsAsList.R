#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		getSVMMethodsAsList.R
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



#' getSVMMethodsAsList
#' 
#' Retrieve the names all known packages as a list of strings.
#'
#' @param	verbose		be verbose in output?
#' 
#' @return	A vector of all known SVM solvers as string.
#'
#' @export

getSVMMethodsAsList = function (verbose = FALSE) {

	knownPackages = c()
	if (verbose == TRUE) {
		cat("Currently known solver:\n")
	}
	
	for (package in SVMBridgeEnv$packages) {
		if (verbose == TRUE) {
			cat ("    -", package$method, "\n")
		}

		knownPackages = c(knownPackages, package$method)
	}
	
	return (knownPackages)
}

