context("OptimizationValuesOtherModels")
library (SVMBridge)



test_that("OptimizationValues works for other models than LIBSVM", {
	
	# for now just set a fixed C
	C = 7.74
	
	# read data
	australian = readSparseData ("../data/australian.train")

	
	# create table of what we expecte
	solver = c("BSGD", "LASVM", "LIBSVM", "CVM", "BVM")
	dualValue = c(0.5, 0.5, 0.5, 0.5, 0.5)
	primalValue = c(0.5, 0.5, 0.5, 0.5, 0.5)
	halfwTw = c(0.5, 0.5, 0.5, 0.5, 0.5)
	trainingError = c(0.5, 0.5, 0.5, 0.5, 0.5)
	optTable = data.frame(cbind (solver, primalValue, dualValue, halfwTw, trainingError))
	
	# TODO: make this compact :(
	optTable$solver = as.character(solver)
	optTable$primalValue = as.numeric (primalValue)
	optTable$dualValue = as.numeric (dualValue)
	optTable$halfwTw = as.numeric (halfwTw)
	optTable$trainingError = as.numeric (trainingError)
	
	optTable = optTable[,3]
	for (r in 1:nrow(optTable)) {
		curModel = optTable[r, ]
		solver = curModel$solver

		# read model first
		modelFile = file.path ("..", "data", paste (solver, "australian", "model", sep = "."))
		modelType = detectModelTypeFromFile (modelFile)
		
		# load corresponding wrapper
		addSVMPackage (method = solver)
		findSVMSoftware (solver, searchPath = "../../R")

	print (curModel)
	print (oV)
	stop()
		# read model
		print (modelFile)
		print(modelType)
		model = readModelFromFile (modelFile, modelType)
		
		# compute values
		oV = optimizationValues (X = as.matrix(australian$X), Y = as.matrix(australian$Y), model = model, C = C)
	
		# check values
		expect_equal (oV$primal, curModel$primalValues)
		expect_equal (oV$dual, curModel$dualValues)
		expect_equal (oV$half_wTw, curModel$halfwTw)
		expect_equal (oV$trainingError, curModel$trainingError)
	}
}	)
	

	
