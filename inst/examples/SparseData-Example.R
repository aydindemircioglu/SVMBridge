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

	library(methods)
	library(devtools)
	load_all ("../..")


#In the following we are going to do a few test runs with SparseData.cpp to simulate certain scenarios

# --- perform a simple reading task
cat("Test #1 Start --- perform a simple reading task\nReading tests/data/australian.train\n")
S = readSparseData ("sparse.data")
print (paste("Data has", nrow(S$X), "points."))
print (paste("Labels are", unique(S$Y), "."))


# --- peform a simple writing task
cat("\n\nTest #2 Start --- peform a simple writing task\nGetting Iris Matrix\n")
X = as.matrix(iris[,1:4])
Y = as.matrix(as.numeric(iris[,5]))
cat("Write the matrix to file ./australian.data\n")
t = tempfile()
writeSparseData (t, X, Y)



# --- perform a reading/writing task with faulty parameter zeroBased = FALSE
#		The code should work without any error even with faulty zeroBased parameter, but it should give out a certain warning message with 			 		informations about the underlying problem. 
cat("\n\nTest #3 Start --- perform a reading/writing task with faulty parameter zeroBased = FALSE\n")
S = readSparseData ("../../tests/data/a0", zeroBased = FALSE, verbose = TRUE) #zeroBased should be TRUE here
writeSparseData ( "../../tmp/test.sparse.data", S$X,  S$Y, verbose = TRUE, zeroBased = FALSE)


# --- perform a reading/writing task with faulty parameter zeroBased = TRUE
#		The code should work without any error even with faulty zeroBased parameter, but it should give out a certain warning message with 			 		informations about the underlying problem.                                                                                                     		In this scenario, the created Matrix should have one additional index value since the data starts with one while the user intended to start 		with zero.
#		hint: if you enable zeroBased flag on data which starts with 1: you get an extra column at the beginning
cat("\n\nTest #4 Start --- perform a reading/writing task with faulty parameter zeroBased = TRUE\n")
S = readSparseData (filename = "../../tests/data/a1", zeroBased = TRUE, verbose = TRUE) #zeroBased should be FALSE here
writeSparseData ( "../../tmp/test.sparse.data", S$X,  S$Y, verbose = TRUE, zeroBased = TRUE)


# --- Cycle Test

tmp = tempfile()
cat("\n\nTest #5 --- Cycle\n")
S = readSparseData (filename = "../../tests/data/sparse.data", verbose = TRUE, zeroBased = FALSE) #dataset starts with one
writeSparseData ( tmp, S$X,  S$Y, verbose = TRUE, zeroBased = FALSE)
S = readSparseData (filename = tmp, verbose = TRUE, zeroBased = FALSE) 
cat("Cycle 1 Done...\n\n")

S = readSparseData (filename = "../../tests/data/sparse.data", verbose = TRUE, zeroBased = TRUE) #dataset starts with one
writeSparseData ( tmp, S$X,  S$Y, verbose = TRUE, zeroBased = TRUE)
S = readSparseData (filename = tmp, verbose = TRUE, zeroBased = TRUE) 
cat("Cycle 2 Done...\n\n")

S = readSparseData (filename = "../../tests/data/sparse.data", verbose = TRUE, zeroBased = FALSE) #dataset starts with one
writeSparseData ( tmp, S$X,  S$Y, verbose = TRUE, zeroBased = TRUE)
S = readSparseData (filename = tmp, verbose = TRUE, zeroBased = FALSE) 
# S = NULL
cat("Cycle 3 Done...\n\n")

S = readSparseData (filename = "../../tests/data/sparse.data", verbose = TRUE, zeroBased = TRUE) #dataset starts with one
writeSparseData ( tmp, S$X,  S$Y, verbose = TRUE, zeroBased = FALSE)
S = readSparseData (filename = tmp, verbose = TRUE, zeroBased = TRUE) 
cat("Cycle 4 Done...\n\n")

cat("Testing Done\n")
