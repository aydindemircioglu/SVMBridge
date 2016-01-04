#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		writeLIBSVMModel.R
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
 

	
#' Write LIBSVM model.
#'
#' Given a model in memory, write it out in LIBSVM model format, so that e.g. LIBSVM can predict on it directly.
#' 
#' @param	model		model object to write
#' @param	modelFile		path where to write the model
#' @param	verbose		be verbose?
#'
#' @note 		As this is a basic for all other model readers, we export it.
#'
#' @export	writeLIBSVMModel
checkModel = function (model = NA, verbose = FALSE) {

	if (verbose == TRUE)
		cat ("Checking model for completeness.\n")
	
	checkmate::assertNumeric (model$gamma)
	checkmate::assertMatrix (model$alpha)

	if (verbose == TRUE)
		cat ("    Done.\n")
	
	# close connection
	close(modelFileHandle)
}
 
