#!/usr/bin/Rscript  --vanilla 

# work around
    system2(command="./cleanup", args="")
    
library(methods)
library(devtools)
library(roxygen2)
library(Rcpp)
compileAttributes ()

roxygenize(".")
load_all (".")
build_vignettes(".")
document(".")

devtools::test()
devtools::check()


# work around
system2(command="R", args="CMD build .")
system2(command="R", args="CMD check --as-cran ./LiblineaR.ACF_1.94-2.tar.gz")
