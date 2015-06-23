
#' Subsample Data by cutting off (a copy of) the file
#'
#' This is a simple routine that will do subsampling by copying the first
#' lines to a temporary file and return this.
#'
#' @param 	filepath		path of the file to subsample
#'	@param	subsamplingRate		rate to subsample. in case this is a value
#'		between 0..1.0000000001, it will be interpreted as a fraction, e.g.
#'		0.5 means subsampling half of the file. If the rate is above 1.0000000001,
#'		the value is taken as the absolut number of data points, e.g. 50 will
#'		subsample exactly 50 points (the first 50 of the file). If anything above 1.00..1
#'		is given, this number will be floored first, so 50.9 lines will yield 50 lines.
#'	
#' @note		if the subsamplingRate == 1 then no copy is done, but the
#' 	file itself is returned. 
#'	@note		a temporary file will be created (in case any subsampling has to be
#'		done)-- this will stay until the R session finishes.
#'	@note		there is no way to subsample just ONE line from the file.
#' @export
#'
subsampleDataByCutoff <- function ( filepath = "", subsamplingRate = -1)
{
	# check
	if (filepath == "")
		stop ("No filepath is given.")
		
	if (subsamplingRate < 0.0) 
		stop ("Subsampling rate cannot be negative")

	subsampledFile = filepath
	if (subsamplingRate > 0.0) 
	{
		originalFile = filepath

		# in case of subsamplingRate == 1.0 we do not do anything
		if (subsamplingRate == 1) {
			return (originalFile)
		}
		
		# need to know how many data is there anyway
		subsamplingSize = subsamplingRate
		if (subsamplingRate < 1.0000000001)
		{
			# this is a true fraction, so  compute true number and overwrite our assumption
			nLines = R.utils::countLines (filepath)[1] 
			subsamplingSize = floor (nLines*subsamplingRate)
		}

		# make sure whatever we have is integer
		subsamplingSize = floor (subsamplingSize)
		
		# create subsampled file on /tmp
		subsampledFile = tempfile(pattern = paste("sub", subsamplingSize, "", sep = "_"))

		# subsample by read-writing, unsure if this is performant enough
		unlink (subsampledFile)
		srcConn = file (originalFile)
		targetConn = file (subsampledFile, "w")
		
		# read in batches of 1024
		batchSize = 1024
		nBatches = subsamplingSize %/% batchSize
		for (i in seq_len(nBatches)) {
			data = readLines (con = srcConn, batchSize)
			writeLines (data, con = targetConn)
		}
		
		# write rest of data
		data = readLines (con = srcConn, subsamplingSize %%  batchSize)
		writeLines (data, con = targetConn)
		
		close (srcConn)
		close (targetConn)
	}
	
	return (subsampledFile)
}
