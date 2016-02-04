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


detectModeltests = function (solver, verbose) {
	
	# detect model type
	modelFile = paste0 ("../data/", solver, ".australian.model")
	if (verbose == TRUE) {
		cat ("Will test file ", modelFile, " for solver ", solver, "\n")
	}
	modelName =  detectModelTypeFromFile (modelFile, defaultModel = solver, verbose = verbose)
	
	# some models are the same
	solverName = solver

	testthat::expect_equal (modelName, solverName)
}
