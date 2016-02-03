#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
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



#' Find All SVM Software 
#'
#' Given a search path, it will try to find the software packages
#' for all registered SVM packages by calling the 'findSoftware' routine of each known wrapper.
#'
#' @param 	searchPath	 	Search path for the SVM binaries of all known SVM packages.
#' @param	verbose			Print messages while searching?
#'
#' @note	To make sure that the binary is correct, it will be executed! (see findBinary for more infos)
#' @note	If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.
#'
#' @export

findAllSVMSoftware <- function (searchPath = NA, verbose = FALSE) {
	
	checkmate::checkString (searchPath)
	
	if (verbose == TRUE) {
		cat("Finding all SVM software packages:\n")
	}
	
	for (method in getSVMMethodsAsList()) {
		findSVMSoftware (method = method, searchPath = searchPath, verbose = verbose)
	}
}

