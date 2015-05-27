#!/usr/bin/Rscript  --vanilla 

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

#library (SVMBridge)
	library(methods)
	library(devtools)
	load_all (".")
	build_vignettes(".")
	document(".")
	# devtools::build_win()	
	# is done by document/loadall anyway?
	library(Rcpp)
	compileAttributes()

	# run tests
	#devtools::test()
#	devtools::check()
# R CMD check ./SVMBridge_1.0.tar.gz

char_vec = c("Pegasos") #"LASVM", "LIBSVM", "SVMperf" ,"BSGD", "BVM", "CVM", "LLSVM", "Pegasos"

OldreadModel.LIBSVM = function (x, modelFile = '~/svmmodel', verbose = FALSE) {
		if (verbose == TRUE) {
			BBmisc::messagef ("Reading LIBSVM model from %s.", modelFile)
		}
		
		# open connection
		con  <- file(modelFile, open = "r")

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
	
	
		# read and interprete data 
		# basically all data is sparse data format, but the data around this differs
		svmatrix = readSparseDataFromConnection(con)

	
		# add header information
		svmatrix$gamma = gamma
		svmatrix$bias = bias
		svmatrix$modelname = "LIBSVM"
		
		# close connection
		close(con)
		
		# return
		return (svmatrix)
	}
	
	die()




	solver = "LIBSVM"
	messagef("\n\n\n======= Train %s, Traindata from Memory, Model to Memory", solver)
	trainObj =  trainSVM(
		method = solver,
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		readModelFile = TRUE,
		verbose = verbose
	)  


die()
#Test Function
#ReadDummyModel(filename)
z = file("/home/hanna/svmmodel")
open(z)
e = readLines(z, 9)
print(e)
L = readSparseDataFromConnection(z)
print (L)
close(z)
die()




readSparseData("tests/data/a0", zeroBased = FALSE, verbose = TRUE)

#In the following we are going to do a few test runs with SparseData.cpp to simulate certain scenarios

# --- perform a simple reading task
cat("Test #1 Start --- perform a simple reading task\nReading tests/data/australian.train\n")
S = readSparseData ("tests/data/australian.train")
print (paste("Data has", nrow(S$X), "points."))
print (paste("Labels are", unique(S$Y), "."))
cat("Test #1 End\n\n")

# --- peform a simple writing task
cat("Test #2 Start --- peform a simple writing task\nGetting Iris Matrix\n")
X = as.matrix(iris[,1:4])
Y = as.matrix(as.numeric(iris[,5]))
cat("Write the matrix to file ./australian.data\n")
writeSparseData ("./australian.data", X, Y)
cat("Test #2 End\n\n")

# --- perform a reading/writing task with faulty parameter zeroBased = FALSE
#		The code should work without any error even with faulty zeroBased parameter, but it should give out a certain warning message with 			 		informations about the underlying problem. 
cat("Test #3 Start --- perform a reading/writing task with faulty parameter zeroBased = FALSE\n")
S = readSparseData ("tests/data/a0", zeroBased = FALSE, verbose = TRUE) #zeroBased should be TRUE here
writeSparseData ( "./tmp/test.sparse.data", S$X,  S$Y, verbose = TRUE, zeroBased = FALSE)
cat("test #3 End\n\n")

# --- perform a reading/writing task with faulty parameter zeroBased = TRUE
#		The code should work without any error even with faulty zeroBased parameter, but it should give out a certain warning message with 			 		informations about the underlying problem.                                                                                                     		In this scenario, the created Matrix should have one additional index value since the data starts with one while the user intended to start 		with zero.
#		hint: if you enable zeroBased flag on data which starts with 1: you get an extra column at the beginning
cat("Test #4 Start --- perform a reading/writing task with faulty parameter zeroBased = TRUE\n")
S = readSparseData (filename = "tests/data/a1", zeroBased = TRUE, verbose = TRUE) #zeroBased should be FALSE here
writeSparseData ( "./tmp/test.sparse.data", S$X,  S$Y, verbose = TRUE, zeroBased = TRUE)
cat("test #4 End\n\n")

# --- Cycle Test
cat("Test #5 --- Cycle\n")
S = readSparseData (filename = "tests/data/australian.train", verbose = TRUE, zeroBased = FALSE) #dataset starts with one
writeSparseData ( "./tmp/test.sparse.data", S$X,  S$Y, verbose = TRUE, zeroBased = FALSE)
S = readSparseData (filename = "./tmp/test.sparse.data", verbose = TRUE, zeroBased = FALSE) 
cat("Cycle 1 Done...\n\n")

S = readSparseData (filename = "tests/data/australian.train", verbose = TRUE, zeroBased = TRUE) #dataset starts with one
writeSparseData ( "./tmp/test.sparse.data", S$X,  S$Y, verbose = TRUE, zeroBased = TRUE)
S = readSparseData (filename = "./tmp/test.sparse.data", verbose = TRUE, zeroBased = TRUE) 
cat("Cycle 2 Done...\n\n")

S = readSparseData (filename = "tests/data/australian.train", verbose = TRUE, zeroBased = FALSE) #dataset starts with one
writeSparseData ( "./tmp/test.sparse.data", S$X,  S$Y, verbose = TRUE, zeroBased = TRUE)
S = readSparseData (filename = "./tmp/test.sparse.data", verbose = TRUE, zeroBased = FALSE) 
cat("Cycle 3 Done...\n\n")

S = readSparseData (filename = "tests/data/australian.train", verbose = TRUE, zeroBased = TRUE) #dataset starts with one
writeSparseData ( "./tmp/test.sparse.data", S$X,  S$Y, verbose = TRUE, zeroBased = FALSE)
S = readSparseData (filename = "./tmp/test.sparse.data", verbose = TRUE, zeroBased = TRUE) 
cat("Cycle 4 Done...\n\n")




die()


// unittest
// cycle: iris/lese daten -> schreibe daten -> lese sie erneut, zerobased = FALSE
-> klappt
// schreibe erneut mit zerobased = TRUE -> laden mit zerobased=TRUE -> klappt
// schreibe erneut mit zerobased = FALSE -> laden mit zerobased=FALSE -> klappt




# first check read data function
S = readSparseData ("tests/data/australian.train")

S = readSparseData ("tests/data/australian.train", verbose = TRUE)

S = readSparseData (filename = "tests/data/australian.train", verbose = TRUE)
print(S)

S = readSparseData (filename = "tests/data/australian.train", verbose = TRUE, zeroBased = FALSE) #Error: dataset != zeroBased
print(S)
print ("CYCLE")

writeSparseData ( "./tmp/test.sparse.data", S$X,  S$Y, verbose = TRUE, zeroBased = TRUE)

S = readSparseData ("./tmp/test.sparse.data", zeroBased = TRUE)
print(S)

writeSparseData (X = S$X, Y = S$Y, filename = "./tmp/test.sparse2.data", verbose = TRUE)
print ("TEST")

S = readSparseData ("tests/data/a0", zeroBased = TRUE, verbose = TRUE)
print(S)

S = readSparseData ("tests/data/a0", zeroBased = FALSE, verbose = TRUE)
print(S)

S = readSparseData (filename = "tests/data/a1", zeroBased = TRUE, verbose = TRUE)
print(S)

S = readSparseData (filename = "tests/data/a1", zeroBased = TRUE, verbose = TRUE)
print(S)

writeSparseData (X = S$X, Y =  S$Y, filename = "./tmp/test.sparse.data", zeroBased = FALSE)

S = readSparseData (filename = "./tmp/test.sparse.data", verbose = FALSE, zeroBase = FALSE)

writeSparseData (X = S$X, Y = S$Y, filename =  "./tmp/test.sparse2.data")

die()

for(solver in char_vec)
{
	# as the libary already loads default wrappers this works
		addSVMPackage (method = solver, verbose = FALSE)
		findSVMSoftware (solver, searchPath = "../svm_large_data/software/", verbose = TRUE)
print("TESH")

	# wird alle bekannte software-pakete suchen also SVMperf, libSVM, ...
	#FIXME: allow path like ~/
		outputAllSVMSoftwarePackages ()
	# 

		verbose = TRUE
		
	# load iris  for now
		shufflediris = iris[sample(nrow(iris)),]

		trainDataX = data.matrix(shufflediris [1:100,1:4])
		trainDatay = data.matrix(as.numeric(shufflediris [1:100, 5]))
		testDataX = data.matrix(shufflediris [-(1:100),1:4])
		testDatay = data.matrix(as.numeric(shufflediris [-(1:100), 5]))

		# FIXME: for now we only accept binary labels
		trainDatay[trainDatay==3] = 1
		testDatay[testDatay==3] = 1
		trainDatay[trainDatay==2] = -1
		testDatay[testDatay==2] = -1

		
		
		messagef("\n\n\n======= Train %s, Traindata from Memory, Model to Memory", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataX = trainDataX, 
			trainDatay = trainDatay, 
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			readModelFile = TRUE,
			verbose = verbose
		)  
		

		messagef("\n\n\n======= Train %s, Traindata from File, Model to Memory", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataFile = './tests/data/australian.train',
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			readModelFile = TRUE,
			verbose = verbose
		)  

		
		messagef("\n\n\n======= Test %s, Testdata from Disk, Model from Memory, Predictions to Memory", solver)
		testObj =  testSVM(
			method = solver,
			testDataFile = './tests/data/australian.train',
			model = trainObj$model,
			verbose = verbose
		)  

		
		messagef("\n\n\n======= Test %s, Testdata from Memory, Model from Memory, Predictions to Memory", solver)
		testObj =  testSVM(
			method = solver,
			testDataX = testDataX, 
			testDatay = testDatay, 
			verbose = verbose,
			model = trainObj$model
		)  

		
		messagef("\n\n\n======= Test %s, Testdata from Disk, Model from Memory, Predictions to Disk", solver)
		testObj =  testSVM(
			method = solver,
			testDataFile = './tests/data/australian.train',
			model = trainObj$model,
			predictionsFile = "./tmp/predictions.txt",
			verbose = verbose
		)  

		
		messagef("\n\n\n======= Test %s, Testdata from Memory, Model from Memory, Predictions to Disk", solver)
		testObj =  testSVM(
			method = solver,
			testDataX = testDataX, 
			testDatay = testDatay, 
			model = trainObj$model,
			predictionsFile = "./tmp/predictions.txt",
			verbose = verbose,
		)  

		
		
		
		
		messagef("\n\n\n======= Train %s, Traindata from Disk, Model to Disk", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataFile = './tests/data/australian.train',
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			verbose = verbose,
			modelFile = "/tmp/libsvm_model.txt",
			readModelFile = FALSE
		)  


		messagef("\n\n\n======= Train %s, Traindata from Memory, Model to Disk", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataX = trainDataX, 
			trainDatay = trainDatay, 
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			verbose = verbose,
			readModelFile = FALSE,
			modelFile = "/tmp/libsvm_model.txt"
		)  
				

		messagef("\n\n\n======= Test %s, Testdata from Memory, Model from Disk, Predictions to Memory", solver)
		testObj =  testSVM(
			method = solver,
			testDataX = testDataX, 
			testDatay = testDatay, 
			modelFile = "/tmp/libsvm_model.txt",
			verbose = verbose
		)  

		

		messagef("\n\n\n======= Test %s, Testdata from Disk, Model from Disk, Predictions to Memory", solver)
		testObj =  testSVM(
			method = solver,
			testDataFile = './tests/data/australian.train',
			modelFile = "/tmp/libsvm_model.txt",
			verbose = verbose
		)  
		

		
		messagef("\n\n\n======= Test %s, Testdata from Memory, Model from Disk, Predictions to Disk", solver)
		testObj =  testSVM(
			method = solver,
			testDataX = testDataX, 
			testDatay = testDatay, 
			modelFile = "/tmp/libsvm_model.txt",
			predictionsFile = "./tmp/predictions.txt",
			verbose = verbose
		)  

		

		messagef("\n\n\n======= Test %s, Testdata from Disk, Model from Disk, Predictions to Disk", solver)
		testObj =  testSVM(
			method = solver,
			testDataFile = './tests/data/australian.train',
			modelFile = "/tmp/libsvm_model.txt",
			predictionsFile = "./tmp/predictions.txt",
			verbose = verbose
		)  
		
		messagef("\n\n\n======= Finished all demo calls successfully.", solver)
		
}
    
