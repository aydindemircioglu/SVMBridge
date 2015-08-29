#' list files wrapper to prevent list.files on cluster
#'
#' 
#' @export
listFiles = function (...) {
	if (is.null(grep("lidong", (Sys.info()["nodename"]))) == TRUE) {
		stop ("Sorry, on LIDO we do not use this list.files function.")
	}
	list.files (...)
}