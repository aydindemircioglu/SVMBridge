#
# SVMBridge
#		(C) 2015, by Aydin Demircioglu
#
#		optimizationValues.R
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



#' Compute optimization values on given data for a model in LIBSVM 'format'.
#'
#' Compute different optimization values like primal and dual for a given model.
#'
#' @param 	X  Data
#' @param	Y  Labels
#' @param	model  LIBSVM model to work with.
#' @param	C		Regularization parameter.
#' @param	verbose		Report certain messages?
#' @return	A list of values with primal, dual, weight (0.5 w^T w) and training error.
#'
#' @export

computeOptimizationValues = function (X, Y, model, C = 0.0, verbose = FALSE) {

	# checkmate checks
	checkmate::assertFlag (verbose)
	checkmate::assertString (model$modelType)

	if (verbose == TRUE) {
		cat("Computing optimization values for given model ", model$modelType, "\n")
	}

	if (verbose == TRUE) {
		cat ("Model parameters:\n")
		cat ("    gamma", model$gamma, "\n")
		cat ("    nSV", model$nSV, "\n")
		cat ("    bias", model$bias, "\n")
		cat ("    label", model$label, "\n")
		cat ("    C", model$C, "\n")
	}

	checkmate::assertMatrix(X, min.rows = 1)
	checkmate::assertVector(Y)
	checkmate::assertNumber (C, lower = 0)
	checkmate::assertNumber (model$gamma, lower = 0)
	checkmate::assertMatrix (model$SV)
	checkmate::assertVector (model$nSV)
	checkmate::assertMatrix (model$alpha)
	checkmate::assertVector (model$bias)
	checkmate::assertVector (model$label)

	# try to get the object
	svmObject = getSVMObject (model$modelType)
	
	if (is.null(svmObject) == TRUE) {
		warning ("No object found for given model of type ", model$modelType)
		return (NULL)
	}
	
	computedValues = optimizationValues (svmObject, X, Y, model = model, C = C, verbose = verbose)

	if (verbose == TRUE)
		print (computedValues)

	return (computedValues)
}
 
