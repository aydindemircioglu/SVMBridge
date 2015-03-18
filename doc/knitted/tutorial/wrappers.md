**SVMBridge** comes with a couple of wrappers for different SVM packages. Here we list these and provide  details on how to use them.

# LIBSVM

LIBSVM is practially the de-facto standard SVM solver. It can be found at http://


# LASVM

LASVM is very similar to LIBSVM wrapper, as both packages use the same parameters and the same model. LASVM has many more options than LIBSVM, as for our purpose we did not need them, for now these are not supported. Please feel free to enhance the wrapper and share the results with us, so we can include these in future versions of **SVMBridge**.


# LLSVM

Part of the BudgetedSVM package, at http://


# Budgeted SGD

Part of the BudgetedSVM package, at http://


# Pegasos

Part of the BudgetedSVM package, at http://


# SVMperf


Currently the package can be found at http://
Note: Please do not mix SVMperf with SVMlight or SVMRank.

SVMperf has many options, so instead of providing all of these as parameter, we decided instead to give the user a choise over several 'configurations'. 




# CVM

libCVM, found at http://

# BVM

Though BVM is practially the same software as CVM, sharing models and parameters and executables with CVM, the philosophy of **SVMBridge** is to have a separate wrapper for each method. Thus, though BVM has its own wrapper, all the parameters etc are the same as CVM, so please read the CVM section.

