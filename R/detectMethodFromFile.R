
#' Auto detect type of SVM model
#' 
#' @param ..
#' @return	method name or NULL if no method could be detected.
#'
detectModelTypeFromFile <- function(
	filePath = NULL,
	verbose = FALSE
)
{
	line = readLines(filePath, 64)

	# BSGD
	pattern = "KERNEL_GAMMA_PARAM:"
	if (sum(grepl(pattern, line)) > 0) {
		method = "BSGD/LLSVM"
		cat(method, "\n")
		return (method)
	}

	# SVMperf/light
	pattern = "SVM-light"
	if (sum(grepl(pattern, line)) > 0) {
		method = "SVMLight"
		cat(method, "\n")
		return (method)
	}

	# LibSVM
	pattern = "total_sv"
	if (sum(grepl(pattern, line)) > 0) {
		method = "LIBSVM"
		cat(method, "\n")
		return (method)
	}

	return (NULL)
}
