#!/usr/bin/Rscript  --vanilla 

# This test will try to download each of the SVM software packages
# that the SVMBridge provides a pre-fabricated wrapper for. 
# This is done via SVN, so SVN needs to be accessible from the command line.
# Then the package will be compiled via make, so some compile tool must be available.
# Finally the software will undergo simple train/testcalls.
# The results are checked by testthat.

library (SVMBridge)

source ("wrappertests.R")
source ("downloadSoftware.R")

# VERY VERY IMPORTANT!
# else SVM does not work 100p the same, as the data is shuffled.
set.seed(42)

verbose = TRUE

modelFile = tempfile()
predictionsFile = tempfile()


## prepare the data

	# create binary iris 
	shufflediris = iris[sample(nrow(iris)),]
	trainDataX = data.matrix(shufflediris [1:100,1:4])
	trainDataY = data.matrix(as.numeric(shufflediris [1:100, 5]))
	testDataX = data.matrix(shufflediris [-(1:100),1:4])
	testDataY = data.matrix(as.numeric(shufflediris [-(1:100), 5]))
	trainDataY [trainDataY==3] = 1
	testDataY [testDataY==3] = 1
	trainDataY [trainDataY==2] = -1
	testDataY [testDataY==2] = -1
	
	# write data for later use
	trainDataFile = tempfile()
	writeSparseData(filename = trainDataFile, X = trainDataX, Y = trainDataY)
	testDataFile = tempfile()
	writeSparseData(filename = testDataFile, X = testDataX, Y = testDataY)

	
## 1. test finding all software first
	
	# stupid, stupid: need to clear up old wrappers, else we find LIBSVM and then test may fail.
	detach("package:SVMBridge", unload = TRUE)
	library(SVMBridge)

	solvers = c("LIBSVM", "LASVM", "BSGD", "SVMperf", "BVM", "CVM", "LLSVM")
	solvers = c("LASVM")
	for (solver in solvers) {
		cat ("Downloading and building software ", solver, "\n")
		softwareDir = downloadSoftware (solver)
		addSVMPackage (solver, wrapperPath = "../../inst/wrapper", verbose = verbose)
	}

	print (getSVMMethodsAsList())
	findAllSVMSoftware (file.path(softwareDir, ".."), verbose = verbose)

	
##  now do all the thirty different ways of calling trainSVM 
	
	for (solver in solvers) {
		context(solver)
		wrappertests (solver, trainDataX, trainDataY, testDataX, testDataY, verbose)
	}
	
	if (verbose == TRUE) {
		for (solver in solvers) {
			s = getSVMObject (solver)
		}
	}
	