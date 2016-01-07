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

library (SVMBridge)


solvers = c( "LASVM", "LIBSVM", "SVMperf" ,"BSGD", "BVM", "CVM", "LLSVM") 

for(solver in solvers)
{
	# as the libary already loads default wrappers this works
	addSVMPackage (method = solver, verbose = FALSE)
	findSVMWrapper (solver, searchPath = "./R", verbose = TRUE)
	findSVMSoftware (solver, searchPath = "~/svm_large_scale/software/", verbose = TRUE)

	verbose = TRUE

	cat("Generating model for ", solver, "\n")
	trainObj =  trainSVM(
		method = solver,
		trainDataFile  = "./tests/data/australian.train",
		cost = 1, 
		gamma = 1, 
		epsilon = 0.01, 
		modelFile = paste0( "./tests/data/tmp", solver, ".model"),
		verbose = verbose
	)  
}
    
