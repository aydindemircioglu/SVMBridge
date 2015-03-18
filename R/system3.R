#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		system3.R
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
 

#' system3
#'
#' @param     binPath		binary to execute
#' @param     args		arguments for binary
#' @param     verbose		verbose output? (including executed command line)
#

system3 <- function (binPath, args, verbose = FALSE) 
{ 
	if (verbose == TRUE) {
		BBmisc::messagef ("----- Arguments:")
		BBmisc::messagef ("%s %s", binPath, paste(args, collapse=" "))
	}

	s = BBmisc::system3(binPath, args, stop.on.exit.code = FALSE, stdout = TRUE)

	if (verbose == TRUE) {
		BBmisc::messagef ("----- Output:")
		BBmisc::messagef ("%s %s", binPath, paste(s$output, collapse="\n"))
		BBmisc::messagef ("-------------")
	}

	return (s)
}
