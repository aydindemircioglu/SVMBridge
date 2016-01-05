
have another function like 
changeSVMPackage ( , key=value)?

# expect to simply fail
addSVMPackage ()

# FIRST BATCH
# will just create an object with empty wrapper and empty software.
# both can be added later via findSVM....R

# add a new package
addSVMPackage (method = "LIBSVM")


# SECOND BATCH
# empty package + wrapper gets specified

# + will locate the wrapper at ./wrapperPath/LIBSVM_wrapper.R
addSVMPackage (method = "LIBSVM", wrapperPath = "./wrapperPath")

# wont work
addSVMPackage (method = "LIBSVM", wrapperPath = "./wrapperPath/my_wrapper.R")

# will work
addSVMPackage (method = "LIBSVM", wrapperName = "my_wrapper.R", wrapperPath = "./wrapperPath")




#THIRD BATCH
# this will try to find the wrapper at ./softwarePath/LIBSVM_wrapper.R and also the binaries exactly there.
# mmh, no, try ALSO ./bin/!
addSVMPackage (method = "LIBSVM", softwarePath = "./softwarePath")

# this will try to find the wrapper at ./softwarePath/my_wrapper.R
addSVMPackage (method = "LIBSVM", wrapperName = "my_wrapper.R", softwarePath = "./softwarePath")


# we need a possibility to change internals--
# e.g. if we know the binary is called differently?
# we can change that by 
changeSVMPackage (method = "LIBSVM", list = list(binaryPath = "./here"))


