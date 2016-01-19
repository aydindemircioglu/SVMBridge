context("Find SVM Software")

# make sure everything is clean
#detach("package:SVMBridge", unload = TRUE)
#library(SVMBridge)
verbose = FALSE


test_that("finding works in general", {
	addSVMPackage ("oneSVM", wrapperPath = "../dummy/oneSVM")
#	print (getSVMObject("oneSVM"))
	r = findSVMSoftware ("oneSVM", searchPath = "../dummy/oneSVM")
#	print (r)
#	print (getSVMObject("oneSVM"))

	svmObject = getSVMObject ("oneSVM")
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$wrapperName, "oneSVM_wrapper.R")
	expect_equal (class(svmObject)[2], "SVMWrapper")
	expect_equal (svmObject$trainBinaryPath, "../dummy/oneSVM/oneSVM-learn")
	expect_equal (svmObject$testBinaryPath, "../dummy/oneSVM/oneSVM-predict")
}	)



test_that("finding non existent software returns error code", {
	addSVMPackage ("oneSVM", wrapperPath = "../dummy/twoSVM")
	retValue = findSVMSoftware ("oneSVM", searchPath = "../dummy/twoSVM")

	svmObject = getSVMObject ("oneSVM")
	expect_equal (retValue, FALSE)
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$wrapperName, "oneSVM_wrapper.R")
	expect_equal (class(svmObject)[2], "SVMWrapper")
	expect_null (svmObject$trainBinaryPath)
	expect_null (svmObject$testBinaryPath)
}	)
	


test_that("finding software without wrapper does not work", {
	addSVMPackage ("oneSVM")
	e = findSVMSoftware ("oneSVM", searchPath = "../dummy/fourSVM")
	expect_equal (FALSE, findSVMSoftware ("oneSVM", searchPath = "../dummy/fourSVM"))
#	expect_equal ("A", 	(getwd()))
}	)


	
test_that("finding software in a sub-sub-directory works", {
	addSVMPackage ("oneSVM", wrapperPath = "../dummy/fourSVM")
	retValue = findSVMSoftware ("oneSVM", searchPath = "../dummy/fourSVM", verbose = verbose)

	svmObject = getSVMObject ("oneSVM")
	expect_equal (retValue, TRUE)
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$wrapperName, "oneSVM_wrapper.R")
	expect_equal (class(svmObject)[2], "SVMWrapper")
	expect_equal (svmObject$trainBinaryPath, "../dummy/fourSVM/dir/subdir/oneSVM-learn")
	expect_equal (svmObject$testBinaryPath, "../dummy/fourSVM/dir/subdir/oneSVM-predict")
#	expect_equal ("A", 	(getwd()))
}	)
	
	
