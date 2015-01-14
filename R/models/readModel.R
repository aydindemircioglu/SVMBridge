#!/usr/bin/Rscript  --vanilla 


# stupid R
loadThings <- function ()
{
  library(stringr)
  library(stringr)
  library(BBmisc)
  library(plyr)
}
suppressMessages(loadThings())



# -readModel
#       convert given model (=file) into a model=some list with entries
# -readSparseFormat (internal)
#       reads the data after the header and gives them back as matricies
# -dumpModel
#       dump some basic information about a given model (=list in memory)


# FIXME: put this into the libsvm wrapper!
readModel <- function (modelFilePath = "./model", method = "LIBSVM", verbose = FALSE)
{
    # open connection
    con  <- file(modelFilePath, open = "r")

    if (method == "SVMperf") {
        method<- "SVMlight"
    }
  
  
    if ((method == "SharkSVMPegasos") | (method == "SharkSVMSMO")) {
        method<- "SharkSVM";
    }
  
  # depending on model, process the header

  # do we need to invert the labels?
  invertLabels = FALSE
  
  # libsvm 
  if ((method == "LIBSVM") | (method == "SharkSVM") | (method == "AESVM") |
        (method == "LASVM") | (method == "libCVM") | (method == "libBVM"))  {
    # grep needed information step by step
    while ((oneLine <- readLines(con, n = 1, warn = FALSE)) != "SV") {
        # gamma value
        if (grepl("gamma", oneLine) == TRUE) {
            pattern <- "gamma (.*)"
            gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        }  
      
        # rho/bias
        if (grepl("rho", oneLine) == TRUE) {
            pattern <- "rho (.*)"
        bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        }
        
        # order of labels
        if (grepl("label", oneLine) == TRUE) {
            pattern <- "label (.*)"
            order = (sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        
            if ((order != "1 -1") && (order != "-1 1")) {
                stop ("Label ordering %s is unknown!", order)
            }
            # LABEL ORDERING IS NOT USED for libsvm!
        }  
    }
  } 
  
  
  # lasvm or svmlight
  if (method == "SVMlight") {
    # grep needed information step by step, the bias is on the threshold line
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
      if (grepl("threshold", oneLine) == TRUE) break;
      # gamma value
      if (grepl("parameter -g", oneLine) == TRUE) {
	pattern <- "(.*) # kernel.*"
	gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
      }  
    }
    
    # the last line read should then contain the bias/threshold
    if (grepl("threshold", oneLine) == TRUE) {
      pattern <- "(.*) # threshold.*"
      bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
    }
  }

  
  # bsgd or pegasos (=bsgd with unlimited budget)
  if ((method == "BSGD") | (method == "Pegasos") | (method == "LLSVM")) 
  {
    # grep needed information step by step, the bias is on the threshold line
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) 
    {
        if (grepl("MODEL", oneLine) == TRUE) break;
      
        # gamma value
        if (grepl("KERNEL_GAMMA_PARAM", oneLine) == TRUE) 
        {
            pattern <- "KERNEL_GAMMA_PARAM: (.*)"
            gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        }  
      
        # bias
        if (grepl("BIAS_TERM", oneLine) == TRUE) 
        {
            pattern <- "BIAS_TERM: (.*)"
            bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        }
      
        # order of labels
        if (grepl("LABELS", oneLine) == TRUE) 
        {
            pattern <- "LABELS: (.*)"
            order = (sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
        
            if ((order != "1 -1") && (order != "-1 1")) {
                stop ("Label ordering %s is unknown!", order)
            }
        
            if (order == "1 -1") {
                invertLabels = FALSE
            }

            if (order == "-1 1") {
                invertLabels = TRUE
            }
            
            # yes, exceptions.
            if (method == "LLSVM") {
                invertLabels = !invertLabels
            }
        }  

    }
  }
  
  
  # read and interprete data 
  # basically all data is sparse data format, but the data around this differs
  svmatrix = readSparseFormat(con, submodel = method)

  
  # add header information
  svmatrix$gamma = gamma
  svmatrix$bias = bias
  svmatrix$modelname = method

  # modify the relative weights, will be corrected when the optimization value is being computed.... maybe FIXME.
#  svmatrix$C = cost
#  svmatrix$L = 1
    
  # do we need to invert the labels? in this case we invert the coefficients
  if (invertLabels == TRUE) {
    
    if (verbose == TRUE)  messagef(" Inverting Labels.")
    
    # invert alphas
    svmatrix$a = -svmatrix$a
    
    # this is also needed.. 
    svmatrix$bias = -bias
  }
  
  # close connection
  close(con)
  
  # return
  return (svmatrix)
}
 




readSparseFormat <- function (con, submodel)
{
  # these will contain the coefficients and the  svs.
  supportvectors <- matrix()
  coefficients <- matrix()
  weights <- matrix()
  
  # read file one by one
  currentIndex = 0
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    
    # if we are at libcvm or libbvm we have to skip the very last line!
    # unluckily in an while loop like this there is no way to know that beforehand
    # so we grep for the "CPUTime" line explicitly
    if ((submodel == "libCVM") | (submodel == "libBVM")) {
      if (grepl("CPU Time =", oneLine) == TRUE) {
        next
      }  
    }
    
    # remove comment if necesary
    oneLine = str_split_fixed(oneLine, pattern = '#', n = 2)[1]
    
    # split line by " "
    svec = vector(length = 1)
    parts = strsplit (oneLine, " ")
    
    # where the support vector data starts in the row
    fvpos = 1
    coeff = vector(length = 1)
    w = vector (length = 1)
    if ((submodel == "LASVM") | (submodel == "SharkSVM") | (submodel == "AESVM") |
            (submodel == "SVMlight") | (submodel == "LIBSVM") | 
            (submodel == "libCVM") | (submodel == "libBVM")) {
      # grep coefficient
      coeff[1] = as.numeric(parts[[1]][1])
      fvpos = 2
    }

    if ((submodel == "LLSVM")) {
      # first entry is the weight vector
      value = as.numeric(parts[[1]][1])
      w[1] = value
    
      # read part for part until it is something positive
      for (i in seq(2, length(parts[[1]]))) {
        # if the entry has no colon, then it is a landmark weight
        if (grepl (":", parts[[1]][i]) == FALSE) {
          # just save it as a numerical in the coefficent matrix
          value = as.numeric(parts[[1]][i])
          # i-1 as we cropped the first value as a weight
          coeff[i-1] = value
        } 
        else {
          # we have a feature vector, so go over to the data part
          fvpos = i
          break
        }
      }
    }
    
    if ((submodel == "BSGD") | (submodel == "Pegasos")) {
      # read part for part until it is something positive
      for (i in seq(1, length(parts[[1]]))) {
        fparts <- strsplit (parts[[1]][i], ":")
        if (!is.na(fparts[[1]][1])) {
            ind = as.numeric(fparts[[1]][1])
            value = as.numeric(fparts[[1]][2])
            
            # check if we have part of some feature vector
            if (ind > 0) {
              # yes, so quit the whole loop
              fvpos = i
              break
            }
            
            # if not, we can save it in the coeff
            coeff[-ind] = value
        }
        else {
          stop ("Should never happen. Really.")
        }
      }
    }
    
    # grep feature vectors one by one
    for (i in fvpos:length(parts[[1]])) {
      # split by :
      fparts <- strsplit (parts[[1]][i], ":")
      
      # if we have anything, add it to our vector
      if (!is.na(fparts[[1]][1])) {
        ind = as.numeric(fparts[[1]][1])
        value = as.numeric(fparts[[1]][2])
        svec[ind] <- value
      }
    }
    
    # make sure our vector has no NAs
    #print (svec)
    svec[is.na(svec)] <- 0
    
    # stack matrices
    supportvectors <- rbind.fill.matrix(supportvectors, t(svec))
    coefficients <- rbind.fill.matrix(coefficients, t(coeff))
    weights <- rbind.fill.matrix(weights, t(w))
  } 
 
  # crop first NA list (why does it even exist?..)
  supportvectors = supportvectors[-1, ]
  coefficients = coefficients[-1, ]
  weights = weights[-1, ]
  
  # remove possible NA values that occur if the very last
  # entry of a sparse vector is omitted because of sparsity
  supportvectors[is.na(supportvectors)] <- 0
  coefficients[is.na(coefficients)] <- 0 
  weights[is.na(weights)] <- 0 


  return (list("X" = supportvectors, "a" = coefficients, "w" = weights))
}



dumpModel <- function (model, data = FALSE)
{
    messagef ("model: %s", model$modelname)
    messagef ("gamma: %f", model$gamma)
    messagef ("bias: %f", model$bias)
    messagef ("C: %f", model$C)
    
    if (data == TRUE)
    {
        messagef ("----------- X: ")
        print (model$X, digits = 13)
        messagef ("----------- a: ")
        print (model$a, digits = 13)
        messagef ("----------- w: ")
        print (model$w)
    }
}




# stupid R check for pythons cool "name == __main__"
if (length(sys.frames()) == 0) 
{

  source ("software/helpers/models/executeSVMSolver.R")

  # for testing we first generate a model for a given dataset
    # set parameters
    cost = 1.0
    gamma = 1.0
   
    modelList = c ( #"LLSVM",
                    #"LIBSVM"
                    #"BSGD"
                    #"Pegasos",
                    #"LASVM", 
                    #"libBVM", 
                    #"libCVM", 
                    "SharkSVM"
                    #"SVMperf"
                    )

    # dataset to consider
    examplePath = "datasets/australian/australian.combined.scaled"

    # generate models
    for (modelName in modelList) {
      messagef( "  Generating model for %s", modelName)
    
      rV = executeSVMSolver (cost, gamma, examplePath, modelName)
      modelFile = rV$modelFile
      modelData = readModel (modelFilePath = modelFile, model = method, verbose = TRUE)
      modelData$C = cost
#      dumpModel (modelData, data = TRUE)
    }
}

# TODO: find out which row corresponds to which label.


