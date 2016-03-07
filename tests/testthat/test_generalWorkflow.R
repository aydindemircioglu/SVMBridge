#
# SVMBridge 
#
#		(C) 2015, by Aydin Demircioglu
# 
# SVMBridge is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SVMBridge is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# Please do not use this software to destroy or spy on people, environment or things.
# All negative use is prohibited.
#

context("General Workflow")



# This test will try to download each of the SVM software packages
# that the SVMBridge provides a pre-fabricated wrapper for. 
# This is done via SVN, so SVN needs to be accessible from the command line.
# Then the package will be compiled via make, so some compile tool must be available.
# Finally the software will undergo simple train/testcalls.
# The results are checked by testthat.


test_that("general workflow works with binary packages.", {
	
	testthat::skip_on_cran()
	
	source ("bashEscapetests.R")
	source ("cycletests.R")
	source ("detectModeltests.R")
	source ("downloadSoftware.R")
	source ("multiClasstests.R")
	source ("predicttests.R")
	source ("wrappertests.R")



	# VERY VERY IMPORTANT!
	# else SVM does not work 100p the same, as the data is shuffled.
	set.seed(42)

	verbose = FALSE
	cat ("\nVerbose is", verbose, "\n")

	modelFile = tempfile()
	predictionsFile = tempfile()


	## prepare the data

	# create binary iris 
	shufflediris = iris[sample(nrow(iris)),]
	trainDataX = data.matrix(shufflediris [1:100,1:4])
	trainDataY = data.matrix(as.numeric(shufflediris [1:100, 5]))
	testDataX = data.matrix(shufflediris [-(1:100),1:4])
	testDataY = data.matrix(as.numeric(shufflediris [-(1:100), 5]))
	
	# multiclass
	mcTestDataY = testDataY
	mcTrainDataY = trainDataY
	
	# binarize labels
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
		
	# stupid, stupid: need to clear up old wrappers, else we find some strange test wrappers and then our tests may fail.
	cat ("Unloading and Reloading SVMBridge!\n")
	clearSVMObjects ()

	solvers = c("LIBSVM", "LASVM", "BSGD", "SVMperf", "BVM", "CVM", "LLSVM")
	softwareBaseDir = tempdir()

	# debugging: only use a subset of solvers
#	solvers = c("LIBSVM")
	#solvers = c("LIBSVM", "LASVM", "BSGD", "SVMperf", "BVM", "CVM", "LLSVM")
	
	# this stuff is for debugging locally without reloading the packages each time
	softwareBaseDir = "/tmp/software"
	if (file.exists(softwareBaseDir) == FALSE) {
#	if (TRUE == TRUE) {
		for (solver in solvers) {
			cat ("Downloading and building software ", solver, "\n")
			softwareDir = downloadSoftware (solver, softwareDir = softwareBaseDir, verbose = verbose)
			cat ("Unlinking ", file.path(softwareDir, ".svn"), "\n")
			unlink (file.path(softwareDir, ".svn"), recursive = TRUE)
			addSVMPackage (solver, wrapperPath = file.path("..", "R"),  verbose = verbose) 
		}
		findAllSVMSoftware (softwareBaseDir, verbose = verbose)
	} else {
		cat ("Found existing software directory (", softwareBaseDir, ") using it\n")
		for (solver in solvers) {
			addSVMPackage (solver, wrapperPath = file.path("..", "R"),  verbose = verbose) 
		}
		findAllSVMSoftware (softwareBaseDir, verbose = verbose)
	}

		
	##  now do all the thirty different ways of calling trainSVM 
	
	for (solver in solvers) {
		cat("\nWrapper test for solver", solver, ": ")
		wrappertests (solver, trainDataX, trainDataY, testDataX, testDataY, verbose)
	}
	
	
	for (solver in solvers) {
		cat ("\nBash escape for solver", solver, ": ")
		bashEscapetests (solver, trainDataX, trainDataY, testDataX, testDataY, verbose)
	}
	

	for (solver in solvers) {
		cat ("\nTest predictions for solver", solver, ": ")
		predicttests (solver, trainDataX, trainDataY, testDataX, testDataY, verbose)
	}
	
	mcsolvers = c("LIBSVM", "BSGD", "BVM", "CVM")
	for (solver in mcsolvers) {
		cat ("\nMulticlass test for solver", solver, ": ")
#		testthat::context (paste0(solver, "wrapper"))
		multiclasstests (solver, trainDataX, mcTrainDataY, testDataX, mcTestDataY, verbose)
	}

	for (solver in solvers) {
		cat ("\nCycle test for solver", solver, ": ")
#		testthat::context (paste0(solver, "cycle"))
		cycletests (solver, verbose)
	}
	
	for (solver in solvers) {
		cat ("\nModel test for solver", solver, ": ")
#		testthat::context (paste0(solver, " detect models."))
		detectModeltests (solver, verbose)
	}
	
	
	
		
	## do a train/test cycle
	
	if (verbose == TRUE) {
		for (solver in solvers) {
			s = getSVMObject (solver)
		}
	}
})
	
