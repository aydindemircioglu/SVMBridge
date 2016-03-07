context("Creating SVM Packages")


test_that("empty method produces an error", {
	# call without parameters to get an exception
	expect_error(createSVMWrapper(), "applicable method for 'createSVMWrapper' applied to an")
}	)
	

test_that("creating SVM Package works", {
	# create just a package
	svmObject = createSVMWrapperInternal ("oneSVM", par.set = ParamHelpers::makeParamSet(ParamHelpers::makeNumericLearnerParam(id="budget",default=128, lower=1)), properties=c("twoclass", "multiclass"), note="A")
	expect_equal (svmObject$method, "oneSVM")
	expect_equal (svmObject$note, "A")
	expect_equal (class(svmObject)[2], "SVMWrapper")
}	)
	

