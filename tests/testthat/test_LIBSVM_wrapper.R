context("LIBSVM")

test_that(" readModel.LIBSM (read/write operations) work with binary models", {
	tmp = tempfile()
	solver = "LIBSVM"
	dataset = ("../data/mnist.binary.model")
	addSVMPackage (method = solver, verbose = FALSE)
	SVMObject = SVMBridgeEnv$packages[[solver]]
	
	svmatrix = readModel.LIBSVM(SVMObject, modelFile = dataset)
	writeModel.LIBSVM(SVMObject, svmatrix, tmp)
	svmatrix2 = readModel.LIBSVM(SVMObject, modelFile = tmp)

	expect_equal(svmatrix, svmatrix2)
})

test_that(" readModel.LIBSM (read/write operations) work with multiclass models", {
	tmp = tempfile()
	solver = "LIBSVM"
	dataset = ("../data/mnist.multi.model")
	addSVMPackage (method = solver, verbose = FALSE)
	SVMObject = SVMBridgeEnv$packages[[solver]]
	
	svmatrix = readModel.LIBSVM(SVMObject, modelFile = dataset)
	writeModel.LIBSVM(SVMObject, svmatrix, modelFile =tmp)
	svmatrix2 = readModel.LIBSVM(SVMObject, modelFile = tmp)

	expect_equal(svmatrix, svmatrix2)
})