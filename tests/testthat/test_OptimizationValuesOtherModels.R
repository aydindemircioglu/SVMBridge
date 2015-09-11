context("OptimizationValuesOtherModels")
# library (SVMBridge)
# devtools::load_all(".")
# setwd("./tests/testthat")

test_that("OptimizationValues works for other models than LIBSVM", {
	
	# for now just set a fixed C
	C = 7.74
	
	# read data
	australian = readSparseData ("../data/australian.train")

	
	# create table of what we expecte
	solver = c("BSGD", "LIBSVM", "LASVM", "CVM", "BVM")
	trainingError = c(0.2289855, 0.257971, 0.2, 0.742029, 0.7362319)
	halfwTw = c(9.086238, 15.66334, 15.8894, 0.5149239, 0.5202258)
	primalValue  = c(2165.622, 1926.384, 1968.645, 2698.021, 2705.437)
	dualValue = c(15.91675678, 16.98221, 17.41129, 1.463981, 1.465854)
	optTable = data.frame(cbind (solver, primalValue, dualValue, halfwTw, trainingError))
	
	# TODO: make this compact :(
	optTable$solver = as.character(solver)
	optTable$primalValue = as.numeric (primalValue)
	optTable$dualValue = as.numeric (dualValue)
	optTable$halfwTw = as.numeric (halfwTw)
	optTable$trainingError = as.numeric (trainingError)
	
	for (r in 1:nrow(optTable)) {
		curModel = optTable[r, ]
		solver = curModel$solver

		# read model first
		modelFile = file.path ("..", "data", paste (solver, "australian", "model", sep = "."))
		modelType = detectModelTypeFromFile (modelFile)

		# load corresponding wrapper
		addSVMPackage (method = solver)
		findSVMSoftware (solver, searchPath = "../../R")

		# read model
		model = readModelFromFile (modelFile, modelType)
		
		# compute values
		X = as.matrix(australian$X)
		Y = as.matrix(australian$Y)[,1]
		
		oV = optimizationValues (X = X, Y = Y, model = model, C = C)
	
		# check values
		expect_equal (oV$primal, curModel$primalValue, tolerance = 0.01)
		expect_equal (oV$dual, curModel$dualValue, tolerance = 0.01)
		expect_equal (oV$weight, curModel$halfwTw, tolerance = 0.01)
		expect_equal (oV$trainingError, curModel$trainingError, tolerance = 0.01)
		
	}
}	)
	
