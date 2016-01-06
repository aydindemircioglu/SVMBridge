context("Expand Tilde")


test_that("something without a tilde does not change", {

	path = "/this/is\\a\\strange/thing#we/do\\not&^$#@!anything}{L:\"about\n\n"
	expPath = expandTilde (path = path)
	expect_equal (path, expPath)
}	)
	

test_that("tilde gets expanded", {
	# problem is a little bit: we do not know the home directory.
	# so we only test here if the tilde is finally gone or not
	path = "~/testdir"
	expPath = expandTilde (path = path)
	expect_equal (grepl("~", expPath), FALSE)
}	)
	

