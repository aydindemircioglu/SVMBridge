#library (SVMBridge)
	library(methods)
	library(devtools)
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

OldreadModel.LIBSVM = function (x, modelFile = '/home/hanna/svmmodel', verbose = FALSE) {
		if (verbose == TRUE) {
			BBmisc::messagef ("Reading LIBSVM model from %s.", modelFile)
		}
		
		# open connection
		con  <- file(modelFile, open = "r")

		while ((oneLine <- readLines(con, n = 1, warn = FALSE)) != "SV") {
			# gamma value
			if (grepl("gamma", oneLine) == TRUE) {
				pattern <- "gamma (.*)"
				gamma = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			}  
		
			# rho/bias
			if (grepl("rho", oneLine) == TRUE) {
				pattern <- "rho (.*)"
			bias = as.numeric(sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			}
			
			# order of labels
			if (grepl("label", oneLine) == TRUE) {
				pattern <- "label (.*)"
				order = (sub(pattern, '\\1', oneLine[grepl(pattern, oneLine)])) 
			
				if ((order != "1 -1") && (order != "-1 1")) {
					stop ("Label ordering %s is unknown!", order)
				}
				# LABEL ORDERING IS NOT USED for libsvm!
			}  
		}
	
	
		# read and interprete data 
		# basically all data is sparse data format, but the data around this differs
		svmatrix = readSparseDataFromConnection(con)

	
		# add header information
		svmatrix$gamma = gamma
		svmatrix$bias = bias
		svmatrix$modelname = "LIBSVM"
		
		# close connection
		close(con)
		
		# return
		return (svmatrix)
	}