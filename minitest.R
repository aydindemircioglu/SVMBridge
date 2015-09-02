library(SVMBridge)


C = 2.0

	model = readLIBSVMModel ("../data/mnist.binary.model")
	mnist = readSparseData ("../data/mnist.data")
	oV = optimizationValues (X = as.matrix(mnist$X), Y = as.matrix(mnist$Y), model = model, C = C)

	source("../computeOptimizationValuesLibSVM.R")
	data = list()
	data$x = mnist$X
	data$y = as.vector(mnist$Y)

	# make sure both are the same length -- TODO: we do it here hardcoded
	model$SV = cbind(model$SV, 0)
	model$L = 1
	model$C = C
	model$X = model$SV
#		trainObj$model$bias = -trainObj$model$bias
	pV = computeOptimizationValuesLibSVM (model, NULL, data = data,  predictionOutput = NULL, verbose = FALSE)
	cat (oV$primal, " ", oV$dual, "\n")
	cat (pV$primal, " ", pV$dual, "\n")

	print (oV$primal)
	print (pV$primal)
	print (oV$dual)
	print (pV$dual)
	
	