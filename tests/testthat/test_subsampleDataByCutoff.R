context("subsampleDataByCutoff")


test_that("subsampling with specified rate does work", {
	# create a sparse file with 1000 lines
	X = data.frame (f1 = runif(1000), f2 = runif(1000), f3 = runif(1000))
	Y = data.frame (Y = sign (runif(1000) - 0.5))
	orgData = tempfile()
	writeSparseData(orgData, as.matrix(X), as.matrix(Y), verbose = TRUE, zeroBased = FALSE)
	
	# now subsample the file
	for (i in seq(1,100)) {
		outputFile = subsampleDataByCutoff (orgData, i/100)
		lines = R.utils::countLines (outputFile)[1]
		expect_equal (i*10, lines)
		
		# TODO: now re-read the file and check if the data is still equal
		
	}
})


test_that("subsampling with specified number of lines does work", {
	# create a sparse file with 1000 lines
	X = data.frame (f1 = runif(1000), f2 = runif(1000), f3 = runif(1000))
	Y = data.frame (Y = sign (runif(1000) - 0.5))
	orgData = tempfile()
	writeSparseData(orgData, as.matrix(X), as.matrix(Y), verbose = TRUE, zeroBased = FALSE)
	
	# now subsample the file
	for (i in seq(2,999,9)) {
		outputFile = subsampleDataByCutoff (orgData, i)
		lines = R.utils::countLines (outputFile)[1]
		expect_equal (i, lines)
	}
})


test_that("parameters are checked", {
	orgData = tempfile()
	expect_error (subsampleDataByCutoff (""))
	expect_error (subsampleDataByCutoff ("", 44))
	expect_error (subsampleDataByCutoff (orgData, -55))
})


test_that("handling with strange files works", {
	orgData = tempfile()
	# check what happens if file has no lines
	# too many lines etc
})

