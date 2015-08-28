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

	if (verbose == TRUE) {
		BBmisc::messagef("Computing optimization values for given model.")
	}

	# check data
	
	# check model
	
	
	# does model fit to data?
	
	# extract values
	gamma = model$gamma
	SV = model$SVs
	nSV = model$nSV  #NEEDED?
	sv_coef = model$alpha
	bias = model$bias
	label = model$label

	# FIXME: for now ignore values vector
	
	# compute values
	if (verbose == TRUE) {
		print (gamma )
		print (SV )
		print (nSV )
		print (sv_coef )
		print (bias )
		print (label )
	}
	computedValues = computeOptimizationValues (X, Y, C, gamma, SV, nSV, sv_coef, bias, label, verbose)

	return (computedValues)
}
 

