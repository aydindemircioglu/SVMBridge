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
 

#' Write a given moel to a file.
#'
#' @param	model		Model to write
#' @param	modelFile		Path to file to write model to
#' @param	verbose		Be verbose?
#'
#' @return	nothing
#'
#' @export
#'
writeModelToFile = function (model = NULL, modelFile = NULL, verbose = FALSE) {

	# make sure the model exists and has some member variables
	if (is.null (model) == TRUE) {
		stop ("No model was given to write. \n")
	}

	# check modelType exists
	checkmate::checkString (model$modelType)

	# get the correct object
	SVMObject = getSVMObject (model$modelType)
	if (checkmate::testClass (SVMObject, "SVMWrapper") == FALSE) {
		stop ("Could not find the SVM Wrapper corresponding to model ", model$modelType, ". Please make sure it is loaded first.\n")
	}	

	# write model
	model = writeModel (SVMObject, model = model, modelFile = modelFile, verbose = verbose)
	return (TRUE)
}	
	
