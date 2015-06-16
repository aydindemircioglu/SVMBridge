#library (SVMBridge)
	library(methods)
	library(devtools)
	library(microbenchmark)
	load_all (".")
	build_vignettes(".")
	document(".")
	# devtools::build_win()	
	# is done by document/loadall anyway?
	library(Rcpp)
	compileAttributes()

	# run tests
	#devtools::test()
#	devtools::check()
# R CMD check ./SVMBridge_1.0.tar.gz

modelFile = "tests/data/poker.combined.scaled"
verbose = FALSE

con  <- file(modelFile, open = "r")

cat("\nStarting microbenchmark: readSparseDataFromConnection\n")
microbenchmark( 
svmatrix = readSparseDataFromConnection(con),
times = 1
)

cat("\nStarting microbenchmark: readSparseFormat\n")
microbenchmark( 
svmatrix = readSparseFormat(modelFile),
times = 1
)



