context("SparseData")
#source("././src/SparseData.cpp")



test_that("test if given Arguments for readSparseData are expected correct ones", {
	expect_error(readSparseData(0), "expecting a string")
})

test_that("Test if numerous reading/writing operations with zerobased set to FALSE on the same dataset leads to precision problems", {
	tmp = tempfile()
	S1 = readSparseData (filename = "../data/sparse.data", verbose = FALSE, zeroBased = FALSE)
	writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = FALSE)
	S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = FALSE) 
	expect_equal(S1$X, S2$X)
	expect_equal(S1$Y, S2$Y)
	
	for(i in 1:100)
	{
		S1 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = FALSE)
		writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = FALSE)
		S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = FALSE) 
		expect_equal(S1$X, S2$X)
 		expect_equal(S1$Y, S2$Y)
	}
})

test_that("Test if numerous reading/writing operations with zerobased set to TRUE on the same dataset leads to precision problems", {
	tmp = tempfile()
	S1 = readSparseData (filename = "../data/sparse.data", verbose = FALSE, zeroBased = TRUE)
	writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = TRUE)
	S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE) 
	expect_equal(S1$X, S2$X)
	expect_equal(S1$Y, S2$Y)
	
	for(i in 1:100)
	{
		S1 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE)
		writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = TRUE)
		S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE) 
		expect_equal(S1$X, S2$X)
 		expect_equal(S1$Y, S2$Y)
	}
})

test_that("Test: Increased Matrix row index due to faulty zeroBased value for Reading/Writing", {
	tmp = tempfile()
	#print("A")
	S1 = readSparseData (filename = "../data/sparse.data", verbose = FALSE, zeroBased = TRUE)
	column1 = ncol(S1$X)
	writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = FALSE)
	#print(column1)
	#print("B")
	S2 = readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE) 
	column2 = ncol(S2$X)
	#print(column2)
	expect_true(column1 < column2)
	
})

test_that("Test: Extracting header information from dataset and getting  the right position for readSparseData", {
	z = file("../data/svmmodel")#contains 9 lines of header information
	open(z)
	e = readLines(z, 9)
	L = readSparseDataFromConnection(z)
	close(z)
})

test_that("Test: Multiclass Data", {
	z = "../../../svm_large_data/datasets.multiclass/dna/dna.combined.scaled"
	r = "../data/svmmodel";
	l = readSparseData(z)
	print(l)
	
})

#test_that("Test: Decreased Matrix row index due to faulty zeroBased value for Reading/Writing", {
#	tryCatch({
#		tmp = tempfile()
#		S1 = readSparseData (filename = "../data/sparse.data", verbose = FALSE, zeroBased = FALSE)
#		writeSparseData ( tmp, S1$X,  S1$Y, verbose = FALSE, zeroBased = TRUE)
#		expect_message(readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE), "est")
#		expect_error(readSparseData (filename = tmp, verbose = FALSE, zeroBased = TRUE) )
#	},	
#	
#	interrupt = function(ex){
#		print(ex)
#	},
#		
#	
#		error = function(e) {
#			print(paste(e))
#		}
#	
#	
#	)
#})






