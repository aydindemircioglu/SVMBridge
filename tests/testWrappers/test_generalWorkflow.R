#!/usr/bin/Rscript  --vanilla 

# make it more general so that all solver can be tested.....

# as LIBSVM has to be compiled, this script expects a c++-compiler and make.


library(SVMBridge)

source("wrappertests.R")


# VERY VERY IMPORTANT!
# else SVM does not work 100p the same, as the data is shuffled.
set.seed(42)

solver = "LIBSVM"
verbose = FALSE

modelFile = tempfile()
predictionsFile = tempfile()



## 1. first we need LIBSVM , so we download and compile it.

downloadSoftware = function (solver) {
	
	if ((solver == "BSGD") || (solver == "LLSVM")) {
		solver = "BudgetedSVM"
	}
	
	if ((solver == "CVM") || (solver == "BVM")) {
		solver = "libCVM"
	}

	tmpDir = tempdir()
	print (tmpDir)
	softwareDir = file.path (tmpDir, solver)
	system2 ("svn", args = c("checkout", paste0 ("https://github.com/aydindemircioglu/SVMBridge/trunk/software/", solver), softwareDir ))
	system2 ("make", args = c("-C", softwareDir))

	return (softwareDir )
}

downloadSoftware ("BVM")

stop ("B")

## 2. prepare the data

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

	
## 3. now do all the thirty different ways of calling trainSVM 
	
	solvers = c("LIBSVM", "LASVM", "BSGD", "SVMperf", "BVM", "CVM", "LLSVM")
	for (solver in solvers) {
		softwareDir = downloadSoftware (solver)
		addSVMPackage (solver, wrapperPath = "../../inst/wrapper", softwarePath = softwareDir)
		wrappertests ("LIBSVM", trainDataX, trainDataY, testDataX, testDataY, verbose)
	}
	