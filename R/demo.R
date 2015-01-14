#!/usr/bin/Rscript  --vanilla 

# replace this by libary (SVMBridge)
source ("./callSVM.R")

# should be called by itself
initPackage()

# TODO: allow for user-provided wrappers? i.e. addSVMPackage ("mySVM", mySVM_wrapper.R)????


# wird alle bekannte software-pakete suchen also SVMperf, libSVM, ...
#FIXME: allow path like ~/
findSVMSoftware (searchPath = "../../../../svm_large_scale/software/", verbose = FALSE) 
outputAllSVMSoftwarePaths ()


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
	
#  this will train and test from memory.
# as no modelfile was given, callSVM will create a temporay model file
# and read this after training, so it will be in svmObj$model
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		testDataX = testDataX, 
		testDatay = testDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE
    )  
    

# or, if preferred, load sparse data from disk
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataFile = './datasets/australian/australian.combined.scaled', 
		testDataFile = './datasets/australian/australian.combined.scaled',
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE
    )  

    
# train only from memory (similar from disk)
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE
    )  


# train from memory, but save the model file to disk
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataX = trainDataX, 
		trainDatay = trainDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE,
        modelFile = "./tmp/libsvm_model.txt"
    )  

    
# predict only from memory, model from memory
    svmObj =  callSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE,
        model = svmObj$model
    )  
    
    
# predict only from memory (similar from disk), model from disk
    svmObj =  callSVM(
		method = "LIBSVM",
		testDataX = testDataX, 
		testDatay = testDatay, 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE,
        modelFile = "./tmp/libsvm_model.txt"
    )  

    
# train only
    svmObj =  callSVM(
		method = "LIBSVM",
		trainDataFile = './datasets/australian/australian.combined.scaled', 
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE
    )  

    
# predict only from disk, model from memory
    svmObj =  callSVM(
		method = "LIBSVM",
		testDataFile = './datasets/australian/australian.combined.scaled',
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
        verbose = FALSE,
        model = svmObj$model
    )  

