context("Storing SVM Objects work")


test_that("storing an SVM object works", {
	# call without parameters to get an exception
	svmObject = createSVMWrapper ("randomString903489534")
	setSVMObject ("randomString903489534", svmObject)
	
	svmObjectStored = getSVMObject ("randomString903489534")
	expect_equal (svmObject$method, svmObjectStored$method)
	expect_equal (class(svmObject), class(svmObjectStored))
}	)
	

test_that("overwriting an SVM object works", {
	# create just a package
	svmObject = createSVMWrapper ("randomString903489534")
	setSVMObject ("randomString903489534", svmObject)
	
	svmObjectSecond = createSVMWrapper ("randomString239423222")
	setSVMObject ("randomString903489534", svmObjectSecond)

	svmObjectStored = getSVMObject ("randomString903489534")
	expect_equal (svmObjectSecond$method, svmObjectStored$method)
	expect_equal (class(svmObjectSecond), class(svmObjectStored))
}	)
	


test_that("erasing an SVM object works", {
	# create just a package
	svmObject = createSVMWrapper ("randomString903489534")
	setSVMObject ("randomString903489534", svmObject)
	setSVMObject ("randomString903489534", NULL)
	
	svmObjectStored = getSVMObject ("randomString903489534")
	expect_equal (svmObjectStored, NULL)
}	)
	
