#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		readModel.R
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
 

#' readModel
#'
#' @param	method		name of the SVM method/solver
#'
#' @param	modelFile		path to file to read model from 
#' @param	verbose		be verbose
#'
#' @return		SVM model 
#'
#' @export
#'
# 
#'

readModelFromFile = function(
	modelFile = NULL, 
	modelType = NULL,
	verbose = FALSE,
	...) {

	# file must be there.
	checkmate::checkFile (modelFile)

	if (file.exists(modelFile) == FALSE) 
		stop ("Sorry, specified file ", modelFile, " does not exist!")

	# automatic detection, if no method is given
	if (is.null (modelType) == TRUE) {
		if (verbose == TRUE) 
			cat ("No model type given, checking automatically. ")
		modelType = detectModelTypeFromFile (modelFile = modelFile)
		if (verbose == TRUE) 
			cat ("Found model:", modelType, "\n")
		if (is.null (modelType) == TRUE) 
			stop ("Sorry, unable to detect model type. ")
	}
	
	# check
	checkmate::checkString (modelType)
	
	# get the correct object
	SVMObject = getSVMObject (modelType)
	if (checkmate::testClass (SVMObject, "SVMWrapper") == FALSE) {
		stop ("Could not find the SVM Wrapper corresponding to model ", modelType, ". Please make sure it is loaded first.\n")
	}

	if (verbose == TRUE) {
		cat ("Handling over reading model to object\n")
	}
	
	# read model (TODO; make call better)
	model = readModel (SVMObject, modelFile = modelFile, verbose = verbose)
	return (model)
}	
	
	