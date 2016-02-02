#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
# SVMBridge is free software: you can redistribtrainSVMute it and/or modify
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
 

#' Read a model from a given file
#'
#' @param	modelFile		Path to the file to read the model from
#' @param	modelType		Name of the SVM method/solver. If Null, detectModel will be called.
#' @param	verbose		Be verbose?
#'
#' @return		An SVM model
#'
#' @note		If no modelType is given, the model will be inferred from detectModelTypeFromFile.
#' If this fails, an error will be issued.
#'
#' @export
#'

readModelFromFile = function(modelFile = NULL, modelType = NULL, verbose = FALSE) {
	# file must be there.
	checkmate::checkFile (modelFile)

	# automatic detection, if no method is given
	if (is.null (modelType) == TRUE) {
		if (verbose == TRUE) 
			cat ("No model type given, checking automatically. ")
		modelType = detectModelTypeFromFile (modelFile = modelFile)
		if (verbose == TRUE) 
			cat ("Found model:", modelType, "\n")
	}
	
	# check that we have a model now.
	checkmate::checkString (modelType)
	
	# get the correct object
	SVMObject = getSVMObject (modelType)
	if (checkmate::testClass (SVMObject, "SVMWrapper") == FALSE) {
		stop ("Could not find the SVM Wrapper corresponding to model ", modelType, ". Please make sure it is loaded first.\n")
	}

	if (verbose == TRUE) {
		cat ("Handling over reading model to object\n")
	}
	
	# read model
	model = readModel (SVMObject, modelFile = modelFile, verbose = verbose)
	return (model)
}	
	
	