context("Find SVM Wrapper")


test_that("trying to find a non existent wrapper returns error code", {
	addSVMPackage ("oneSVM", verbose = FALSE)
	expect_warning (findSVMWrapper ("oneSVM", searchPath = "../dummy/empty"), "No wrapper found")
	
	# workaround 
	testReturnValue = function () {
		retValue = findSVMWrapper ("oneSVM", searchPath = "../dummy/empty")
		expect_equal (retValue, FALSE)
	}
	
	# test return value 
	suppressWarnings (testReturnValue())

	# object must stell be there
	svmObject = getSVMObject ("oneSVM")
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$wrapperName, "oneSVM_wrapper.R")
	expect_equal (class(svmObject)[2], "SVMWrapper")
	expect_null (svmObject$wrapperPath)
	expect_null (svmObject$trainBinaryPath)
	expect_null (svmObject$testBinaryPath)
}	)
	

test_that("finding wrapper in a sub-sub-directory works", {
	# will not work, as the wrapper is not in main directory
	addSVMPackage ("oneSVM", wrapperPath = "../dummy/")
	svmObject = getSVMObject ("oneSVM")
	expect_null (svmObject$wrapperPath)
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$wrapperName, "oneSVM_wrapper.R")
	
	# but we can find it by searching
	retValue = findSVMWrapper ("oneSVM", searchPath = "../dummy/fiveSVM")

	svmObject = getSVMObject ("oneSVM")
	expect_equal (retValue, TRUE)
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$wrapperName, "oneSVM_wrapper.R")
	expect_equal (svmObject$wrapperPath, "../dummy/fiveSVM/dir/subdir/oneSVM_wrapper.R")
	expect_equal (class(svmObject)[2], "SVMWrapper")
	expect_null (svmObject$trainBinaryPath)
	expect_null (svmObject$testBinaryPath)
	
} )
