context("Adding SVM Packages")


test_that("empty method produces an error", {
	# call without parameters to get an exception
	expect_error(addSVMPackage(), "May not be NA")
	
	# 
}	)
	

test_that("adding (empty) SVM Packages work", {
	# add just a package
	addSVMPackage ("oneSVM")
	svmObject = getSVMObject ("oneSVM")
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$wrapperName, "oneSVM_wrapper.R")
	expect_equal (class(svmObject)[2], "SVMWrapper")
}	)
	

test_that("adding a SVM Package with wrapper in same path works", {
	# add just a package
	addSVMPackage ("oneSVM", wrapperPath = "../dummy/oneSVM")
	svmObject = getSVMObject ("oneSVM")
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$wrapperName, "oneSVM_wrapper.R")
	expect_equal (svmObject$wrapperPath, "../dummy/oneSVM/oneSVM_wrapper.R")
}	)
	

test_that("adding a SVM Package with differently named wrapper in same path works", {
	# add just a package
	addSVMPackage ("oneSVM", wrapperPath = "../dummy/twoSVM")
	svmObject = getSVMObject ("oneSVM")
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$wrapperName, "oneSVM_wrapper.R")
	expect_equal (svmObject$wrapperPath, "../dummy/twoSVM/oneSVM_wrapper.R")
}	)
	
	
test_that("adding a SVM Package with wrapper not in path works", {
	# add just a package
	addSVMPackage ("threeSVM", wrapperPath = "../dummy/threeSVM")
	svmObject = getSVMObject ("oneSVM")
	expect_equal (svmObject$method, "oneSVM")
}	)
	

test_that("adding a SVM Package with software in same path works", {
	# add just a package
	addSVMPackage ("oneSVM", wrapperPath = "../dummy/oneSVM", softwarePath = "../dummy/oneSVM")
	svmObject = getSVMObject ("oneSVM")
	expect_equal (svmObject$trainBinaryPath, "../dummy/oneSVM/oneSVM-learn")
	expect_equal (svmObject$testBinaryPath, "../dummy/oneSVM/oneSVM-predict")
}	)
	

test_that("adding a SVM Package with software in ./bin path works", {
	# add just a package
	addSVMPackage ("threeSVM", wrapperPath = "../dummy/threeSVM", softwarePath = "../dummy/threeSVM")
	svmObject = getSVMObject ("threeSVM")
	expect_equal (svmObject$trainBinaryPath, "../dummy/threeSVM/bin/threeSVMtrain")
	expect_equal (svmObject$testBinaryPath, "../dummy/threeSVM/bin/threeSVMtest")
}	)
	

test_that("having a software path without a wrapper path will not work", {
	# add just a package
	addSVMPackage ("oneSVM", wrapperPath = "../dummy/twoSVM", softwarePath = "../dummy/twoSVM")
	svmObject = getSVMObject ("oneSVM")
	expect_null (svmObject$trainBinaryPath)
	expect_null (svmObject$testBinaryPath)
}	)
	
