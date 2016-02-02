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
 

	
#' Check model.
#'
#' Given a model, check it by calling the asking the corresponding wrapper.
#' 
#' @param	model		Model (in memory) to check. This cannot be used simultanously with modelFile.
#' @param	modelFile		Path to model (on disk) to check. . This cannot be used simultanously with model.
#' @param	verbose		Be verbose?
#'
#' @note 		As this is a basic for all other model readers, we export it. 
#'
#' @export

checkModel = function (model = NA, modelFile = NA, verbose = FALSE) {

	if (verbose == TRUE)
		cat ("Checking model for completeness.\n")

	if (is.null (model) && is.null (modelFile)) {
		stop ("You must specify either a model or a model file!")
	}
		
	if (is.null (model) == FALSE && is.null (modelFile) == FALSE) {
		stop ("You cannot specify a model and a model file at the same time!")
	}

	# try to load the model in case that it is not in memory
	if (is.null (model) == TRUE) {
		model = readModelFromFile (modelFile = modelFile, verbose = verbose) 
	}
	
	# check first, if the model has is the correct class 
	if (checkmate::testNull (model) == TRUE) {
		warning ("No model was given.")
		return (-1)
	}
	
	checkmate::assertString (model$modelType)
	SVMObject = getSVMObject (model$modelType)
	if (checkmate::testClass (SVMObject, "SVMWrapper") == FALSE) {
		warning ("The wrapper for this model seems not to be loaded.")
		return (-1)
	}
	
	retValue = checkModel (SVMObject, model = model, verbose = verbose)
	return (retValue)
}
 
