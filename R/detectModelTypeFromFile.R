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


#' Detect model type from file.
#' 
#' This will auto detect the type of SVM model from a given file.
#' 
#' @param	modelFile		Path to model file.
#' @param	defaultModel		Model to use as default in case multiple models match.
#' @param	verbose		Be verbose?
#'
#' @return	Method name or NULL if no method could be detected.
#'
#' @note		As it is common for many derivates of LIBSVM to use the very same model structure
#' as LIBSVM, the model cannot be detected reliably. Nonetheless, to minimize confusion, a default
#  model can be provided. If this solver/modeltype is amongst the ones matching, this will be returened.
#' E.g. a LIBSVM model will be detected as LASVM, BVM and CVM, By providing defaultModel, say
#' to be BVM, the modelType returned will be BVM. It has no effect, if only one or none models are matched.
#'
#' @export
#'

detectModelTypeFromFile = function (modelFile = NULL, defaultModel = "LIBSVM", verbose = FALSE) {
	checkmate::checkFile (modelFile)
	checkmate::checkFlag (verbose)
	checkmate::checkString (defaultModel)
	
	# iterate over all known wrapper
	detectedModels = c()
	for (package in getSVMMethodsAsList(verbose = verbose)) {
		if (detectModel (getSVMObject(package), modelFile = modelFile, verbose = verbose) == TRUE) {
			if (verbose == TRUE)
				cat("Detected model ", package, ".\n")
			detectedModels = c(detectedModels, package)
		}
	}

	if (length(detectedModels) == 0) 
		return (NULL)
	
	if (defaultModel %in% detectedModels)
		return (defaultModel)
	
	return (detectedModels[1])
}
