context("LIBSVM helper functions")

test_that("reading a LIBSVM model works", {
	modelFile = "../data/LIBSVM.australian.model"
	australianModel = readLIBSVMModel (modelFile) 
	
	expect_equal (australianModel$nSV, c(15, 15))
	expect_equal (australianModel$bias, 0.221514)
	expect_equal (australianModel$modelType, "LIBSVM")
	expect_equal (australianModel$label, c(1, -1))
	expect_equal (australianModel$gamma, 1)
	print (australianModel)
	print (australianModel$SV)
	expect_equal (australianModel$SV[1,], c(-1.0000000, -0.7618045, -0.1785714, -1.0000000, -0.3846154, -0.5000000, -1.0000000, 1.0000000, 1.0000000, -0.6716418, 1.0000000, 0.000000,  -1.0000000, -1.0000000))
	expect_equal (australianModel$SV[30,], 1:14/10)
	a = structure(c(1.23393867049914, 1.19032740745071, 0.802854355014499, 
1.00690214749611, 0.304890618348797, 0.450352404609654, 4, 0.293921098727743, 
0.485767995883826, 0.460083828017044, 3.74566292626991, 0.342750115862192, 
0.748108710813141, 0.595783076879186, 0.557041356312248, -0.670940537534037, 
-0.454072637844643, -0.742498598475257, -0.555284788787071, -0.818203059880298, 
-4, -0.70192632200093, -0.20480773764385, -0.438252991809989, 
-0.774538075245176, -3.26682269927698, -0.783102530810903, -1.25557727044339, 
-0.761137927123363, -1), .Dim = c(30L, 1L))
	expect_equal (australianModel$alpha, a)
} )	
	
	

test_that("reading predictions works", {
	predictionsFile = "../data/LIBSVM.australian.predictions"
	australianPredictions = readLIBSVMPredictions (predictionsFile) 
	
	expect_equal (australianPredictions, c(-1, 1))

} )



test_that("read/write operations for LIBSVM  models work with binary models", {

	modelFile = "../data/LIBSVM.australian.model"
	australianModel = readLIBSVMModel (modelFile) 
	print (australianModel)
	
	tmp = tempfile()
	writeLIBSVMModel (australianModel, modelFile = tmp, verbose = TRUE)
	australianModelReread = readLIBSVMModel (modelFile = tmp)
	print (australianModelReread)

	expect_equal(australianModel, australianModelReread)
})



test_that(" readModel.LIBSVM (read/write operations) work with multiclass models", {
	modelFile = "../data/LIBSVM.australian.model"
	australianModel = readLIBSVMModel (modelFile) 
	
	tmp = tempfile()
	writeLIBSVMModel (australianModel, modelFile = tmp)
	australianModelReread = readLIBSVMModel (modelFile = tmp)

	expect_equal(australianModel, australianModelReread)
})

