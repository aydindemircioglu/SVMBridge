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



#' Set an SVM object into the environment.
#'
#' @param	method		Name of method
#' @param	object	Object to set.
#' @param 	verbose		Be verbose?
#'

setSVMObject = function (method = method, object = NULL, verbose = FALSE) {
	if (checkmate::testNull (object) == TRUE) {
		if (verbose == TRUE) {
			cat ("Setting a NULL object, i.e. object for method ", method, " will be erased.\n")
		}
	}
	SVMBridgeEnv$packages[[method]] = object
}
