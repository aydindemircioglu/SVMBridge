#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		getSVMObject.R
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



#' getSVMObject
#'
#' Retrieve the object for a given method.
#'
#' @param	method		name of method
#' @return	SVM object for the given method (or NULL if it does not exist)
#'
#' @export
setSVMObject = function (method = method, object = NULL) {
	if (checkmate::testNull (object) == TRUE) {
		warn ("Setting a NULL object, i.e. object for method ", method, " will be erased.")
	SVMBridgeEnv$packages[[method]] = object
}

