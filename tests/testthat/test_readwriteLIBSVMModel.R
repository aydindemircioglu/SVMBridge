context("LIBSVM")

test_that(" readModel.LIBSVM (read/write operations) work with binary models", {
	tmp = tempfile()
	solver = "LIBSVM"
	dataset = ("../data/LIBSVM.mnist.model")
	
	svmatrix = readLIBSVMModel  (modelFile = dataset)
	writeLIBSVMModel (svmatrix, tmp)
	svmatrix2 = readLIBSVMModel (modelFile = tmp)

	expect_equal(svmatrix, svmatrix2)
})

test_that(" readModel.LIBSVM (read/write operations) work with multiclass models", {
	tmp = tempfile()
	solver = "LIBSVM"
	dataset = ("../data/LIBSVM.mnist.multiclass.model")
	
	svmatrix = readLIBSVMModel (modelFile = dataset)
	writeLIBSVMModel (svmatrix, modelFile =tmp)
	svmatrix2 = readLIBSVMModel (modelFile = tmp)

	expect_equal(svmatrix, svmatrix2)
})