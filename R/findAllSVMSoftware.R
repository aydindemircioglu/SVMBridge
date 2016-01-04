#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		findAllSVMSoftware.R
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



#' findAllSVMSoftware 
#'		Given a search path, it will try to find the software packages
#'		for all registered SVM package by calling the 'findSoftware' routine of each known wrapper.
#'
#' @param 	searchPath	 	search the given path for the SVM binaries of all known SVM packages.
#' @param	verbose			print messages while searching?
#'
#' @note	To make sure that the binary is correct, it will be executed! (see findBinary for more infos)
#' @note	If multiple binaries are found, the last one will be taken. Overwrite by hand, if necessary.
#'
#' @export
findAllSVMSoftware <- function (searchPath = NA, verbose = FALSE) {
	if (verbose == TRUE) {
		cat("-Finding all SVM software packages:")
	}
	
	for (i in seq(1, length(SVMBridgeEnv$packages))) {
		method = SVMBridgeEnv$packages[[i]]$method
		findSVMSoftware (method = method, searchPath = searchPath, verbose = verbose)
	}
}

