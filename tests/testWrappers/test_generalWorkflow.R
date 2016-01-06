#!/usr/bin/Rscript  --vanilla 

# make it more general so that all solver can be tested.....

# as LIBSVM has to be compiled, this script expects a c++-compiler and make.


library(SVMBridge)

# VERY VERY IMPORTANT!
# else SVM does not work 100p the same, as the data is shuffled.
set.seed(42)

solver = "LIBSVM"
verbose = FALSE

modelFile = tempfile()
predictionsFile = tempfile()


## 1. first we need LIBSVM , so we download and compile it.

	tmpDir = tempdir()
	LIBSVMzip = file.path (tmpDir, "libsvm.zip")
	download.file ("https://github.com/cjlin1/libsvm/archive/master.zip", destfile = LIBSVMzip, "auto")
	unzip (LIBSVMzip, exdir = tmpDir)
	LIBSVMDir = file.path (tmpDir, "libsvm-master")
	system2 ("make", args = c("-C", LIBSVMDir))

	# now we have build libsvm, hopefully successfully.


## 2. we now source our wrapper and prepare the data

	addSVMPackage ("LIBSVM", wrapperPath = "../../inst/wrapper", softwarePath = LIBSVMDir)

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

## 3.1 memory to memory

	cat("\n\n\n======= Train ", solver, ", Traindata from Memory, Model to Memory\n")
	print (getSVMObject(solver))
	trainObj =  trainSVM(
		method = solver,
		trainDataX = trainDataX, 
		trainDataY = trainDataY, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		readModelFile = TRUE,
		verbose = verbose
	)  
	expect_equal (trainObj$model$nSV, c(20, 12))
	
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX,
		testDataY = testDataY,
		model = trainObj$model,
		verbose = verbose
	)  
	expect_equal (testObj$testError, 0.06)

f = function () {	
		
	stop ("A")
	
	cat("\n\n\n======= Train ", solver, ", Traindata from File, Model to Memory")
	trainObj =  trainSVM(
		method = solver,
		trainDataFile = trainDataFile,
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		readModelFile = TRUE,
		verbose = verbose
	)  

	testObj =  testSVM(
		method = solver,
		testDataFile = trainDataFile,
		model = trainObj$model,
		verbose = verbose
	)  


	
	cat ("\n\n\n======= Test ", solver, ", Testdata from Memory, Model from Memory, Predictions to Memory")
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX, 
		testDataY = testDataY, 
		verbose = verbose,
		model = trainObj$model,
		readPredictions = TRUE
	)  


	
	cat("\n\n\n======= Test ", solver, ", Testdata from Disk, Model from Memory, Predictions to Disk")
	testObj =  testSVM(
		method = solver,
		testDataFile = testDataFile,
		model = trainObj$model,
		predictionsFile = predictionsFile,
		verbose = verbose
	)  

	
	cat("\n\n\n======= Test ", solver, ", Testdata from Memory, Model from Memory, Predictions to Disk")
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX, 
		testDataY = testDataY, 
		model = trainObj$model,
		predictionsFile = predictionsFile,
		verbose = verbose,
	)  

	
	
	cat("\n\n\n======= Train ", solver, ", Traindata from Disk, Model to Disk")
	trainObj =  trainSVM(
		method = solver,
		trainDataFile = testDataFile,
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		verbose = verbose,
		modelFile = modelFile,
		readModelFile = FALSE
	)  


	cat("\n\n\n======= Train ", solver, ", Traindata from Memory, Model to Disk")
	trainObj =  trainSVM(
		method = solver,
		trainDataX = trainDataX, 
		trainDataY = trainDataY, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		verbose = verbose,
		readModelFile = FALSE,
		modelFile = modelFile
	)  
			

	cat("\n\n\n======= Test ", solver, ", Testdata from Memory, Model from Disk, Predictions to Memory")
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX, 
		testDataY = testDataY, 
		modelFile = modelFile,
		verbose = verbose
	)  

	

	cat("\n\n\n======= Test ", solver, ", Testdata from Disk, Model from Disk, Predictions to Memory")
	testObj =  testSVM(
		method = solver,
		testDataFile = testDataFile,
		modelFile = modelFile,
		verbose = verbose
	)  
	

	
	cat("\n\n\n======= Test ", solver, ", Testdata from Memory, Model from Disk, Predictions to Disk")
	testObj =  testSVM(
		method = solver,
		testDataX = testDataX, 
		testDataY = testDataY, 
		modelFile = modelFile,
		predictionsFile = predictionsFile,
		verbose = verbose
	)  

	

	cat("\n\n\n======= Test ", solver, ", Testdata from Disk, Model from Disk, Predictions to Disk")
	testObj =  testSVM(
		method = solver,
		testDataFile = testDataFile,
		modelFile = modelFile,
		predictionsFile = predictionsFile,
		verbose = verbose
	)  
	
	cat("\n\n\n======= Finished all demo calls. Please check for any visible errors.\n\n")


}