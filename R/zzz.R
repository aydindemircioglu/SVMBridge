#
# SVMBridge 
#		(C) 2015, by Aydin Demircioglu
#
#		zzz.R
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


#' @useDynLib SVMBridge
#' @importFrom Rcpp sourceCpp

 
# create own environment
SVMBridgeEnv = new.env(parent = emptyenv())


.onLoad <- function(libname, pkgname) {
    op <- options()
    op.devtools <- list(
        devtools.path = "~/R-dev",
        devtools.install.args = "",
        devtools.name = "Aydin Demircioglu",
        devtools.desc.author = '"Aydin Demircioglu <aydin.demircioglu@ini.rub.de> [aut, cre]"',
        devtools.desc.license = "GPL-3",
        devtools.desc.suggests = NULL,
        devtools.desc = list()
    )
    toset <- !(names(op.devtools) %in% names(op))

    if(any(toset)) 
        options(op.devtools[toset])

    invisible()
}


.onAttach <- function (libname, pkgname) {
    packageStartupMessage("SVMBridge v0.1 loaded.")
    
    addSVMPackage ("BSGD")
    addSVMPackage ("BVM")
    addSVMPackage ("CVM")
    addSVMPackage ("LASVM")
    addSVMPackage ("LIBSVM")
    addSVMPackage ("LLSVM")
    addSVMPackage ("Pegasos")
    addSVMPackage ("SVMperf")
}

