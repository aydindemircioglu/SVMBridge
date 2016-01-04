
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

# + will locate the wrapper at ./wrapperPath/specified.R
addSVMPackage (method = "LIBSVM", wrapperPath = "./wrapperPath/specified.R")

# + will search the wrapper below './wrapperPath'
addSVMPackage (method = "LIBSVM", wrapperSearchPath = "./wrapperPath")

# expect to fail, cannot work with both? OR: will try to locate first, if not, will search for it.
addSVMPackage (method = "LIBSVM", wrapperPath = "./wrapperPath", wrapperSearchPath = "./wrapperPath")

# override default wrapper name, so you can use findWrapper and find a differently named wrapper.
addSVMPackage (method = "LIBSVM", wrapperName = "my_wrapper.R")

# expect to fail, cannot work with both? OR: will try to locate first, if not, will search for it.
addSVMPackage (method = "LIBSVM", wrapperName = "my_wrapper.R", wrapperSearchPath = "./wrapperPath")
addSVMPackage (method = "LIBSVM", wrapperName = "my_wrapper.R", wrapperPath = "./wrapperPath")




#THIRD BATCH
# empty package + wrapper gets specified + software gets specified

# + will locate the wrapper at ./wrapperPath/LIBSVM_wrapper.R
addSVMPackage (method = "LIBSVM", wrapperPath = "./wrapperPath")

# + will locate the wrapper at ./wrapperPath/specified.R
addSVMPackage (method = "LIBSVM", wrapperPath = "./wrapperPath/specified.R")


# NOTE: there is no case where we have no wrapper, but software!

# FOURTH BATCH: mutants

# this will try to find the wrapper at ./softwarePath/LIBSVM_wrapper.R
addSVMPackage (method = "LIBSVM", softwarePath = "./softwarePath")

# this will try to find the wrapper at ./softwarePath/my_wrapper.R
addSVMPackage (method = "LIBSVM", wrapperName = "my_wrapper.R", softwarePath = "./softwarePath")


# we need a possibility to change internals--
# e.g. if we know the binary is called differently?
# we can change that by 
changeSVMPackage (method = "LIBSVM", list = list(binaryPath = "./here"))


