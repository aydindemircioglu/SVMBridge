
source ("./system3.R")


subsampleData <- function ( filepath = "", subsamplingRate = -1)
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
            tmpOutput = system3('wc', c("-l", filepath))#, stdout = TRUE)
            fraction = as.numeric(strsplit(tmpOutput$output, split = " ")[[1]][1])
            subsamplingSize = floor(fraction*subsamplingRate)
        }

        # ask system to do subsampling for us
        system3('head', 
            c(sprintf("-n %d", subsamplingSize), 
              filepath,
                ">",
                subsampledFile)
        )
    }
    
    return (subsampledFile)
}

    