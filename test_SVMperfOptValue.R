
library(SVMBridge)

verbose = TRUE
C = 99.1

# add SVMperf wrapper
	solver = "SVMperf"
	addSVMPackage (method = solver, wrapperPath = "./R")
	findSVMSoftware (solver, searchPath = "~/svm_large_scale/software/")

	solver = "LIBSVM"
	addSVMPackage (method = solver, wrapperPath = "./R")
	findSVMSoftware (solver, searchPath = "~/svm_large_scale/software/")

	# read model first
	trainFile = file.path ("~", "lab", "data", "australian", paste ("australian", "train", sep = "."))

	# train a SVMperf model
	cat ("\nTraining SVMperf")
	trainedObj =  trainSVM(
			method = "SVMperf",
			trainDataFile = trainFile,
			cost = C, 
			k = 64,
			gamma = 0.001, 
			epsilon = 0.1, 
			modelFile = "./svmperf.model",
			readModelFile = TRUE,
			verbose = verbose
	)  

	model = trainedObj$model
	data = readSparseData (trainFile)
	oV = optimizationValues (X = data$X, Y = data$Y, model = model, C = C)
	print (oV)
	
	writeModelToFile (model, "./libsvm.model")
	model2 = readModelFromFile ("./libsvm.model")
	oV = optimizationValues (X = data$X, Y = data$Y, model = model2, C = C)
	print (oV)
	
#	print (trainObj)
#$model$modelType
