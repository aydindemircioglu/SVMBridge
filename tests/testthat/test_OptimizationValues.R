context("OptimizationValues")

#library (SVMBridge)
# 	library(methods)
# 	library(BBmisc)
# 	library(devtools)
# 	load_all (".")
# 	build_vignettes(".")
# 	document(".")
# 	# devtools::build_win()	
# 	# is done by document/loadall anyway?
# 	library(Rcpp)
# 	compileAttributes()

	# run tests
	#devtools::test()
#	devtools::check()
# R CMD check ./SVMBridge_1.0.tar.gz

test_that("OptimizationValues extraction works", {
	solver = "LIBSVM"
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMSoftware (solver, searchPath = "../../../shark/svm_large_data/software/", verbose = TRUE)

	verbose = FALSE
	
# load iris  for now
	shufflediris = iris[sample(nrow(iris)),]

	trainDataX = data.matrix(shufflediris [1:100,1:4])
	trainDatay = data.matrix(as.numeric(shufflediris [1:100, 5]))
	testDataX = data.matrix(shufflediris [-(1:100),1:4])
	testDatay = data.matrix(as.numeric(shufflediris [-(1:100), 5]))

	# FIXME: for now we only accept binary labels
	trainDatay[trainDatay==3] = 1
	testDatay[testDatay==3] = 1
	trainDatay[trainDatay==2] = -1
	testDatay[testDatay==2] = -1
#		writeSparseData (X = trainDataX, Y = trainDatay, file = "iris.sparse")
	tmp = tempfile()
	C = 0.71
	gamma = 0.41
	#We take a regular dataset at this step
	dataset = ("../../../shark/svm_large_data/datasets/aXa/aXa.combined.scaled")
	cat("\n\n\n======= Train ", solver, "Traindata from Memory, Model to Memory")
	trainObj =  trainSVM(
		method = solver,
		trainDataFile = dataset,
# 		trainDataX = trainDataX, 
# 		trainDatay = trainDatay, 
		cost = C, 
		gamma = gamma, 
		epsilon = 0.0000000001, 
# 		modelFile = tmp,
		readModelFile = TRUE,
		subsamplingRate = 0.1,
		subsamplingMethod = "cutoff",
		verbose = verbose
	)  
	
	# extract optimization values from model
	oV = optimizationValues(X = trainObj$model$SVs, Y = trainObj$model$alpha, model = trainObj$model, C = C, values = c(), verbose = FALSE)

	source("../computeOptimizationValuesLibSVM.R")
	data = list()
	data$x = trainObj$model$SVs
	data$y = trainObj$model$alpha
	trainObj$model$L = 1
	trainObj$model$C = C
	trainObj$model$X = trainObj$model$SV
#		trainObj$model$bias = -trainObj$model$bias
	pV = computeOptimizationValuesLibSVM (trainObj$model, NULL, data = data,  predictionOutput = NULL, verbose = FALSE)
	cat (oV$primal, " ", oV$dual, "\n")
	cat (pV$primal, " ", pV$dual, "\n")

    
})
    
   # ~/svm_large_scale/software/libSVM/bin/svm-predict -c 0.71 -o iris.sparse iris.sparse tmpMo A 
# Obtained cost C: 0.710000
# Obtained gamma g: 0.410000
# Current rho: [0.064655]
# Current hingeLoss: [83.120949]
# Current weight: [2.025757]
# Computed primal value: [61.067719]
# Computed dual value: [2.171971]
