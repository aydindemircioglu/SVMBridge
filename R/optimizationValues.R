
#' compute different optimization values for a given model
#'
#' @param 		X
#' @param		Y
#' @param		model
#' @param		C		regularization parameter (if needed)
#' @param		values		which values to evaluate
#' @return		a list of values and corresponding objects 
#' @export		
#'

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
 

