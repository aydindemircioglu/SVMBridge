

subsampleDataByCutoff <- function ( filepath = "", subsamplingRate = -1)
{
    subsampledFile = filepath
    if (subsamplingRate > 0.0) 
    {
        # create subsampled file on /tmp
        subsampledFile = tempfile()
        originalFile = filepath

        # need to know how many data is there anyway
        subsamplingSize = subsamplingRate
        if (subsamplingRate < 1.001)
        {
            # this is a true fraction, so  compute true number and overwrite our assumption
            nLines = R.utils::countLines (filepath)[1] 
            subsamplingSize = floor (nLines*subsamplingRate)
        }

        # subsample by read-writing, unsure if this is performant enough
		srcConn = file (originalFile)
		targetConn = file (subsampledFile)
		
		# read in batches of 1024
		batchSize = 1024
		nBatches = subsamplingSize %/% batchSize
		for (i in 1:nBatches) {
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
