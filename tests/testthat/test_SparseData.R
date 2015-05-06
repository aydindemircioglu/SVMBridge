context("SparseData")



test_that("test if given Arguments for readSparseData are expected correct ones", {
	try {
		readSparseData(0)
		expect_true(1 == 0)	
	} 
	catch (e) {
		expect_true (e.str() == "expecting a string")
	} 
})

test_that("test if given Arguments for writeSparseData are expected correct ones", {
	try {
		writeSparseData("A","B", 0)
		expect_true(1 == 0)	
	} 
	catch (e) {
		expect_true (e.str() == "Please specify a filename.")
	} 
})
