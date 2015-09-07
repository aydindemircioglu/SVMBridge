#!/usr/bin/Rscript  --vanilla 

# stupid R
loadThings <- function ()
{
  library(BBmisc)
}
suppressMessages(loadThings())


computeErrors <- function (prediction = list(), labels = list(), verbose = FALSE )
{
    # sanity check
    if (length(prediction) != length(labels)) {
        # labels might be transposed
        labels = t(labels)
    }

    # retry
    if (length(prediction) != length(labels)) {
        stopf ("Cannot compute error, lengths are different.")
    }

    # we take the sign as a precaution, sign is idempotent
    prediction = sign (prediction)
    
    absError = sum(abs(labels - t(prediction))/2)
    relError = absError/length(prediction)

    for (i in 1:length(prediction)) {
        #messagef ("t:%f -- p:%f", labels[i], prediction[i]) 
    }
    
    pr = 2*as.numeric(prediction<= 0) - 1 

    errors = list ()
    errors$absError = absError
    errors$relError = relError
    if (verbose)  messagef( "Rel.Error  %f", relError)
    if (verbose)  messagef( "Abs.Error  %f", absError)
    
    return (errors)
}

 
 
computeErrorsLLSVM <- function (prediction = list(), labels = list(), verbose = FALSE )
{
    # we take the sign as a precaution, sign is idempotent
    PS = prediction
    prediction = 1.0 * (prediction > 0.0) + (-1.0) * ((prediction <= 0.0))
    
    absError = sum(abs(labels - t(prediction))/2)
    relError = absError/length(prediction)

    #print (prediction)
    unlink ("./PLLSV")
    for (i in seq(1, length(prediction))) {
      write(paste (prediction[i], PS[i]), file = "./PLLSV", append = TRUE)
    }
    pr = 2*as.numeric(prediction<= 0) - 1 
    unlink ("./PLLSV")

    errors = list ()
    errors$absError = absError
    errors$relError = relError

    if (verbose)  messagef( "LLSVM Rel.Error  %f", relError)
    if (verbose)  messagef( "LLSVM Abs.Error  %f", absError)
    
    return (errors)
}

 

# stupid R check for pythons cool "name == __main__"
if (length(sys.frames()) == 0) 
{
    errors = computeErrors (prediction = c(0.2, 0.4, 0.6, 1.4, 1.2, -1.0, -1.2), labels = c(-1, -1, -1, -1, -1, 1, 1))
    messagef ("Absolute Error: %f", errors$absError)
    messagef ("Relative Error: %f", errors$relError)
    
#        if (verbose)  print (pr)
 #   if (verbose)  messagef( "y %f %f", dim(labels)[1], dim(labels)[2])
  #  if (verbose)  messagef( "prediction %f %f", dim(pr)[1], dim(pr)[2])
   # if (verbose)  print (labels)
    #if (verbose)  print (prediction <= 0)

}