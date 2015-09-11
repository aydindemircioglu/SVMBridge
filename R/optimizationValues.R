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
optimizationValues <- function (X, Y, model, C = 0.0, values = c(), verbose = FALSE) {

	# checkmate checks
	checkmate::assertFlag (verbose)
	checkmate::assertString (model$modelType)

	if (verbose == TRUE) {
		cat("Computing optimization values for given model.")
	}

	# apply our fixes-- if we do not know the model, we apply LIBSVM
	if (model$modelType == "LIBSVM")	{
		# nothing to fix
	}
	
	if (model$modelType == "SVMperf")	{
		
		# compute lambda and correction factor as BSGD model works with lambda-formulation
		N = nrow(X)
	}
	
	if (model$modelType == "BSGD")	{
	# sanity check, if we really have BSGD data
		cl1 = model$alpha[,1]
		cl2 = -model$alpha[,2]
		if (!all(cl1 == cl2)) {
			stop ("Sanity check failed: alpha_1 == -alpha_2 is violated.")
		}

		# prepare model, the alphas have coefficients for 'both' classes, need simply to crop that
		model$alpha = t(model$alpha[,1])

		# adapt gamma, as BSGD has different kernel constant
		model$gamma = model$gamma/2
		
		
		# compute lambda and correction factor as BSGD model works with lambda-formulation
		N = nrow(X)
	}
	
	if (model$modelType == "LASVM")	{
		# nothing to be done, as LASVM and LIBSVM have the same model format.
	}
		

	# does model fit to data?

	
	# FIXME: for now ignore values vector
	
	# compute values
	if (verbose == TRUE) {
		cat ("gamma", model$gamma, "\n")
		cat ("nSV", model$nSV, "\n")
		cat ("bias", model$bias, "\n")
		cat ("label", model$label, "\n")
		cat ("C", model$C, "\n")
		cat ("SV", model$SV, "\n")
		cat ("alpha", model$alpha, "\n")
	}

	checkmate::assertMatrix(X, min.rows = 1)
	checkmate::assertVector(Y)
	checkmate::assertNumber (C, lower = 0)
	checkmate::assertNumber (model$gamma, lower = 0)
	checkmate::assertMatrix (model$SV)
	checkmate::assertVector (model$nSV)
	checkmate::assertMatrix(model$alpha)
	checkmate::assertVector (model$bias)
	checkmate::assertVector (model$label)

	computedValues = computeOptimizationValues (X, Y, C, model$gamma, 
		model$SV, model$nSV, model$alpha, model$bias, model$label, verbose)

	if (verbose == TRUE)
		print (computedValues)
	
	return (computedValues)
}
 