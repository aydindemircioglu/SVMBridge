#!/usr/bin/Rscript  --vanilla 
#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		findSVMSoftware.R
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
 


#' Expand path with tilde 
#'	
#' Given a path, try to expand the path, crossplatform independent.
#'
#' @param 	path		path to expand
#' @param	verbose			print messages while searching?
#'
#' @export
expandTilde <- function(path = NULL, verbose = FALSE){
	#look for tilde characters and expand them
	if(grepl("~", path) == TRUE){
		if(.Platform$OS.type == "windows"){
			firstPart = path.expand("~")
			firstPart = substr(firstPart, 1, nchar(firstPart) - 10)
			secondPart = substr(path, 2, nchar(path))
			path = paste(firstPart, secondPart, sep="")
		}
		else
			path = path.expand(path)
		path = gsub("[\\]", "/", path)
		
		if(verbose == TRUE){
			BBmisc::messagef("  Expanded path: %s", path)
		}
	}
	return (path)
}

