library(SVMBridge)


pathForMethod = function (method = NA) {
	# stupid check
	path = method
	if (method == "CVM") 
		path = "libCVM"
	if (method == "BVM") 
		path = "libCVM"
	if (method == "BSGD") 
		path = "BudgetedSVM"
	return (path)
}



binaryForMethod  = function (method = NA, bin = NA) {
	# stupid check
	if ((method == "LASVM") && (bin == "test"))
		bin = "la_test"
	if ((method == "LASVM") && (bin == "train"))
		bin = "la_svm"
	if ((method == "LIBSVM") && (bin == "test"))
		bin = "svm-predict"
	if ((method == "LIBSVM") && (bin == "train"))
		bin = "svm-train"
	if ((method == "BSGD") && (bin == "test"))
		bin = "budgetedsvm-predict"
	if ((method == "BSGD") && (bin == "train"))
		bin = "budgetedsvm-train"
	if ((method == "SVMperf") && (bin == "test"))
		bin = "svm_perf_classify"
	if ((method == "SVMperf") && (bin == "train"))
		bin = "svm_perf_learn"
	if ((method == "CVM") && (bin == "test"))
		bin = "svm-predict"
	if ((method == "CVM") && (bin == "train"))
		bin = "svm-train"
	if ((method == "BVM") && (bin == "test"))
		bin = "svm-predict"
	if ((method == "BVM") && (bin == "train"))
		bin = "svm-train"
		
	return (bin)
}

verbose = TRUE
softwarePath = "/home/drunkeneye/timelimited/software/"

solvers = c("CVM", "LASVM", "LIBSVM", "SVMperf", "BVM", "BSGD")

for (solver in solvers) {

	static = list()
	static$solver = solver

	wSolver = paste(static$solver, "walltime", sep = "_")
	wrapperName = paste0 (wSolver, "_wrapper.R")
	wrapperPath = file.path (softwarePath, pathForMethod(static$solver), wrapperName) 
	print (wrapperPath)

	testBinaryPath = file.path(softwarePath, pathForMethod (static$solver), "bin", binaryForMethod (static$solver, bin = "test"))
	trainBinaryPath = file.path (softwarePath, pathForMethod (static$solver), "bin", binaryForMethod (static$solver, bin = "train"))
			
	addSVMPackage (method = wSolver, trainBinaryPath = trainBinaryPath,
		testBinaryPath = testBinaryPath,
		wrapperPath  = wrapperPath ,
		verbose = TRUE)

		
	outputAllSVMSoftwarePackages()
	print (getSVMInstance (wSolver))


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



	cat("\n\n\n======= Train", solver, "Traindata from Memory, Model to Memory\n")
	trainObj =  trainSVM(
			method = wSolver,
			trainDataX = trainDataX, 
			trainDatay = trainDatay, 
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			readModelFile = TRUE,
			verbose = verbose
	)  

}

cat ("\nfinished.\n")
