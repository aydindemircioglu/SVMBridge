
#library(SVMBridge)
devtools::load_all(".")

verbose = FALSE
C = 99.1

# add SVMperf wrapper
	solver = "SVMperf"
	addSVMPackage (method = solver, wrapperPath = "./R")
	findSVMSoftware (solver, searchPath = "~/svm_large_scale/software/")

	solver = "LIBSVM"
	addSVMPackage (method = solver, wrapperPath = "./R")
	findSVMSoftware (solver, searchPath = "~/svm_large_scale/software/libSVM/")

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

	testObj =  testSVM(
		method = "SVMperf",
		testDataFile = trainFile,
		modelFile = "./svmperf.model",
		verbose = verbose
	)  

	cat ("## SVMperf: The predict, vanilla.\n")
	print (testObj)
	
	cat ("## Writing the model in LibSVM format.\n")
	model = trainedObj$model
	model$modelType = "LIBSVM"
	writeModelToFile (model, "./libsvm.model")

	cat ("## LIBSVM: The predict, but now by LIBSVM predict \n")
	testObj =  testSVM(
			method = "LIBSVM",
			testDataFile = trainFile,
			modelFile = "./libsvm.model",
			verbose = TRUE
	)  
	print (testObj)
	
	cat ("## Computing Opt Values from model in memory \n")
	model$alpha = model$alpha 
	data = readSparseData (trainFile)
	oV = optimizationValues (X = data$X, Y = data$Y, model = model, C = C, verbose = TRUE68)
	print (oV)
	
	cat ("## Computing Opt Values from LibSVM model on disk \n")
	model2 = readModelFromFile ("./libsvm.model")
	oV = optimizationValues (X = data$X, Y = data$Y, model = model2, C = C)
	print (oV)

	# funny part: make the model SVMperf again
	cat ("## Writing model in memory to disk as SVMperf model \n")
	model2$modelType = "SVMperf"
	writeModelToFile (model = model2, modelFile = "./svmperfcopy.model")
	
	cat ("## SVMperf: The predict, now written by our routines \n")
	testObj =  testSVM(
			method = "SVMperf",
			testDataFile = trainFile,
			modelFile = "./svmperfcopy.model",
			verbose = TRUE
	)  
	print (testObj)
	
	cat ("## Reread model and compute OptValues  \n")
	model2 = readModelFromFile ("./svmperfcopy.model")
	oV = optimizationValues (X = data$X, Y = data$Y, model = model2, C = C)
	print (oV)
	
	
#	print (trainObj)
#$model$modelType
