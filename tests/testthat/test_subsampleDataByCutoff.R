context("subsampleDataByCutoff")


test_that("subsampling with specified rate does work", {
	# create a sparse file with 5000 lines
	nLines = 5000
	X = data.frame (f1 = runif(nLines), f2 = runif(nLines), f3 = runif(nLines))
	Y = data.frame (Y = sign (runif(nLines) - 0.5))
	orgData = tempfile()
	writeSparseData(orgData, as.matrix(X), as.matrix(Y), verbose = TRUE, zeroBased = FALSE)
	
	# now subsample the file
	for (i in seq(1,100)) {
		outputFile = subsampleDataByCutoff (orgData, i/100)
		lines = R.utils::countLines (outputFile)[1]
		expect_equal (floor(nLines/100*i), lines,  tolerance = 0.01)
		
		# now re-read the file and check if the data is still equal
		t = readSparseData (outputFile)
		attributes(t$X) = attributes(as.matrix(X[1:lines,]))
		attributes(t$Y) = attributes(as.matrix(Y[1:lines,]))
		expect_equal (as.matrix(X[1:lines,]), t$X)
		expect_equal (as.matrix(Y[1:lines,]), t$Y)
	}
})


test_that("subsampling with specified number of lines does work", {
	# create a sparse file with 5000 lines
	nLines = 5000
	X = data.frame (f1 = runif(nLines), f2 = runif(nLines), f3 = runif(nLines))
	Y = data.frame (Y = sign (runif(nLines) - 0.5))
	orgData = tempfile()
	writeSparseData(orgData, as.matrix(X), as.matrix(Y), verbose = TRUE, zeroBased = FALSE)
	
	# now subsample the file
	for (i in seq(2,nLines - 1,9)) {
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

