#!/usr/bin/Rscript  --vanilla 
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
 

#' optimizationValues
#'		compute different optimization values for a given model
#'
#' @param 		X
#' @param		Y
#' @param		model
#' @param		C		regularization parameter (if needed)
#' @param		values		which values to evaluate
#' @param		verbose		report certain messages?
#' @return		a list of values and corresponding objects
#'
#' @export
optimizationValuesLIBSVM = function (X, Y, model, C = 0.0, verbose = FALSE) {
	# checkmate checks
	checkmate::assertFlag (verbose)
	checkmate::assertString (model$modelType)

	if (verbose == TRUE) {
		cat("Computing optimization values for given model.")
	}

	if (verbose == TRUE) {
		cat ("gamma", model$gamma, "\n")
		cat ("nSV", model$nSV, "\n")
		cat ("bias", model$bias, "\n")
		cat ("label", model$label, "\n")
		cat ("C", model$C, "\n")
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

	computedValues = computeOptimizationValuesCpp (X, Y, C, model$gamma, 
		model$SV, model$nSV, model$alpha, model$bias, model$label, verbose)

	if (verbose == TRUE)
		print (computedValues)
	
	return (computedValues)
}
 