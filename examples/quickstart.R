
# load the SVMBridge 
library(SVMBridge)

# add the prepackaged wrapper 
addSVMPackage("LIBSVM", wrapperPath =  file.path (path.package("SVMBridge"), "wrapper"))

# let SVMBridge find the compiled/downloaded LIBSVM package. 
found = findSVMSoftware (method = 'LIBSVM', searchPath = "./software/LIBSVM")

# use the SVMBridge to train LIBSVM
svmObj =  trainSVM("LIBSVM", trainDataFile = './data/australian_scale', cost = 1, gamma = 1)

# print the time needed for training
cat( paste("LIBSVM took", svmObj$trainTime, "seconds to train.\n"))

# use the SVMBridge to test LIBSVM (here on the same data set)
testObj = testSVM(testDataFile = './data/australian_scale', model = svmObj$model)

# print the test error
cat( paste("Test error is ", round(testObj$testError,2), ". Testing took ", round (testObj$testTime,3) , " seconds.\n", sep = ''))

