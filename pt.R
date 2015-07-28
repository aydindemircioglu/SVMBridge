#!/usr/bin/Rscript  --vanilla 

#
# SVMBridge 
#
#		(C) 2015, by Aydin Demircioglu
# 
# SVMBridge is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SVMBridge is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# Please do not use this software to destroy or spy on people, environment or things.
# All negative use is prohibited.
#

#library (SVMBridge)
	library(methods)
	library(BBmisc)
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

char_vec = c( "SVMperf")# "LLSVM", "Pegasos")


for(solver in char_vec)
{
	# as the libary already loads default wrappers this works
		addSVMPackage (method = solver, verbose = FALSE)
		findSVMSoftware (solver, searchPath = "../../../svm_large_scale/software/", verbose = TRUE)

	# wird alle bekannte software-pakete suchen also SVMperf, libSVM, ...
	#FIXME: allow path like ~/
		outputAllSVMSoftwarePackages ()
	# 

		verbose = TRUE
		
	for (x in seq(0,1,0.01)) {
		messagef("\n\n\n======= Train %s, Traindata from File, Model to Memory", solver)
		trainObj =  trainSVM(
			method = solver,
			trainDataFile = '~/splits/spektren_1/trainData.dat',
			subsamplingRate = x,
			cost = 1, 
			gamma = 1, 
			epsilon = 0.01, 
			wallTime = 2,
			readModelFile = TRUE,
			verbose = verbose
		)  
	}
		
		
}
    
