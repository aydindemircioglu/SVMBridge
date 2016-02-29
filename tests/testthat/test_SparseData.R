context("SparseData")



test_that(" if given Arguments for readSparseData are expected correct ones", {
	# was crasheing--  Rcpp bug??
#	expect_error(readSparseData(0), "expecting a string")
})



test_that(" if numerous reading/writing operations with zerobased set to FALSE on the same dataset leads to precision problems", {
	tmp = tempfile()
	S1 = readSparseData (filename = "../data/mnist.data", zeroBased = FALSE)
	writeSparseData ( tmp, S1$X,  S1$Y, zeroBased = FALSE)
	S2 = readSparseData (filename = tmp, zeroBased = FALSE) 
	expect_equal(S1$X, S2$X)
	expect_equal(S1$Y, S2$Y)
	
	for(i in 1:10)
	{
		S1 = readSparseData (filename = tmp, zeroBased = FALSE)
		writeSparseData ( tmp, S1$X,  S1$Y, zeroBased = FALSE)
		S2 = readSparseData (filename = tmp, zeroBased = FALSE) 
		expect_equal(S1$X, S2$X)
 		expect_equal(S1$Y, S2$Y)
	}
})



test_that(" if numerous reading/writing operations with zerobased set to TRUE on the same dataset leads to precision problems", {
	tmp = tempfile()
	S1 = readSparseData (filename = "../data/mnist.data", zeroBased = TRUE)
	writeSparseData ( tmp, S1$X,  S1$Y, zeroBased = TRUE)
	S2 = readSparseData (filename = tmp, zeroBased = TRUE) 
	expect_equal(S1$X, S2$X)
	expect_equal(S1$Y, S2$Y)
	
	for(i in 1:10) {
		S1 = readSparseData (filename = tmp, zeroBased = TRUE)
		writeSparseData ( tmp, S1$X,  S1$Y, zeroBased = TRUE)
		S2 = readSparseData (filename = tmp, zeroBased = TRUE) 
		expect_equal(S1$X, S2$X)
 		expect_equal(S1$Y, S2$Y)
	}
})



test_that(" Increased Matrix row index due to faulty zeroBased value for Reading/Writing", {
	tmp = tempfile()
	S1 = readSparseData (filename = "../data/mnist.data", zeroBased = TRUE)
	column1 = ncol(S1$X)
	writeSparseData ( tmp, S1$X,  S1$Y, zeroBased = FALSE)
	S2 = readSparseData (filename = tmp, zeroBased = TRUE) 
	column2 = ncol(S2$X)
	expect_true(column1 < column2)
})



test_that(" Read/Write functionality of Datasets with multiple alpha vectors", {
	f = file("../data/LIBSVM.mnist.model") #contains 9 columns of header information
	open(f)
	readLines(f, 9)
	tmp = tempfile()
	s = readSparseDataFromConnection(f, 9)
	writeSparseData(filename = tmp, s$X, s$Y)
	s2 = readSparseData(filename = tmp)
	expect_equal(s$X, s2$X)
	expect_equal(s$Y, s2$Y)

 	for(i in 1:100) {
		s = readSparseData(filename = tmp)
		writeSparseData(filename = tmp, s$X, s$Y)
		s2 = readSparseData(filename = tmp)
		expect_equal(s$X, s2$X)
		expect_equal(s$Y, s2$Y)
	}
	close(f)
})



test_that(" Read/Write operations do work on datasets with non binary labels", {
	tmp = tempfile()
	S1 = readSparseData (filename = "../data/mnist.multiclass.data")
	writeSparseData ( tmp, S1$X,  S1$Y)
	S2 = readSparseData (filename = tmp) 
	expect_equal(S1$X, S2$X)
	expect_equal(S1$Y, S2$Y)
})



test_that(" Reading file without labels does work", {
	S1 = readSparseData (filename = "../data/australian.nolabels")
	S2 = readSparseData (filename = "../data/australian.test")
	expect_equal(S1$X, S2$X)
	expect_equal(S1$Y, NULL)
})



