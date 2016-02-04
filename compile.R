#!/usr/bin/Rscript  --vanilla 

# work around
system2(command="./cleanup", args="")
    
library(methods)
library(devtools)
library(roxygen2)
library(Rcpp)

cat ("## Super cleanup\n")
system2(command="rm", args="-rf inst")
system2(command="mkdir", args="inst")
system2(command="mkdir", args="inst/doc")

cat ("## Compiling Rcpp attributes\n")
compileAttributes ()
cat ("## Roxygenizing\n")
roxygenize(clean = TRUE)
cat ("## Documenting\n")
document()
cat ("## Building Vignettes\n")
build_vignettes()

cat ("## Checking package\n")
devtools::check()
cat ("## Testing package\n")
devtools::test()


# work around
cat ("## Building package\n")
system2(command="R", args="CMD build .")
cat ("## Checking package for CRAN\n")
system2(command="R", args="CMD check --as-cran ./SVMBridge_0.1.tar.gz")
