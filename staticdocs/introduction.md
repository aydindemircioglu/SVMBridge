


# Introduction

This introduction gives you a short overview and a quickstart example.
The intend is to make you familiar with the basic concept of the **SVMBridge**.
For more detailed information, please see [the next chapter](detailed.Rmd).

The SVMBridge is a simple R package that provides a structured way to
executing any SVM binary package directly within R.
For this, it needs three parts:

- A 'wrapper'. This is an R source code file that contains directions on how
to execute the SVM, read and write models etc.
- The SVM binaries. These consist of two executables, one for training, one for testing.
- Data. Usually one data set for training and one for validating/testing.

In this tutorial, we aim to use the LIBSVM package, the probably best known SVM solver, for a simple SVM classification task. Notice that there are already packages like e1071 which allow direct use of LIBSVM within R. Though these approaches are elegant, they do not help in case other SVM packages need to be called. (Please remember that LIBSVM is not the only one way to solve the SVM problem). For example, to use SVMperf within R, usually one would need to modify the corresponding C/C++ sources and repackage them into an R package, which is a considerable amount of work.

The **SVMBridge** provides instead a structured way to call other SVM packages by command line. This has the advantage that only a (often simple) wrapper in R has to be written. No sources have to be changed (in case the SVM solver can readily be called by command line), nor an R package has to be written containing these sources.

To understand the way the **SVMBridge** works, let us now train LIBSVM on a simple data set.  As the **SVMBridge** is just an easy way to call a precompiled SVM package, we first need to obtain LIBSVM. Download the source package from the LIBSVM webpage (currently at http://www.csie.ntu.edu.tw/~cjlin/libsvm/), either as zip/tar or directly from github. We assume here that you put the sources into a directory './software/LIBSVM'.

If you are using windows, the package should contain already precompiled binaries, in this case you can skip the rest of this paragraph. If you use any other OS, you need to compile LIBSVM. For this, go to the directory and call 'make' from the command line (Make sure to have all the necessary c++ packages installed). After successful compilation you will have two binary executables: svm-train and svm-predict. Make sure you can execute them by just calling './svm-train' (an extensive help page should be shown).

The next thing we need is the wrapper (for more details on this and also on how to write your own wrapper, see  [Wrappers](wrappers.md)). The **SVMBridge** comes with several prepackaged wrappers already, and we will use the LIBSVM wrapper for our purpose, so for this tutorial there is nothing to do (The wrappers can be found in the folder 'wrapper' just below the installation folder of the package.)

Before we can use the **SVMBridge**, we need some data. Remember, that data of a general machine learning problem consists of at least two parts: Training and testing data. The LIBSVM dataset webpage provides many different benchmark data sets (currently at 'http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/'). To keep things simple, we will use the same data set for testing and training. Download the scaled australian data set (currently at 'http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/australian_scale') and put it into a folder. We assume here that you download it to  'data/australian_scale'.

Finally, you should now have the following structure

- /data/australian_scale
- /software/LIBSVM/ (with all the LIBSVM files)

Now,  you can write your first simple R-script utilizing the **SVMBridge**:

```splus
# load the SVMBridge
library(SVMBridge)

# add the wrapper (wrapperPath can be skipped for prepackaged wrappers)
addSVMPackage("LIBSVM", wrapperPath =  file.path (path.package("SVMBridge"), "wrapper"))

# let SVMBridge find the binaries for the LIBSVM package.
found = findSVMSoftware (method = 'LIBSVM', searchPath = "./software/LIBSVM")

# use the SVMBridge to train LIBSVM
svmObj =  trainSVM("LIBSVM", trainDataFile = './data/australian_scale', cost = 1, gamma = 1)
```

**Note:** In case something goes wrong, you can add the verbose = TRUE flag to the trainSVM function. Sometimes the paths are wrong, i.e. **SVMBridge** cannot find the LIBSVM executable or the data set.

The ```trainSVM``` function will return an object that contains the trained model and some basic information about the training process, e.g. the training time (including I/O). See  [In Depth Introduction](detailed.md) for more information about the returned SVM object.

Here we print the training time LIBSVM needed and then go ahead and test the model on the test data (as said, for convenience we use the very same training data set for testing, greatly underestimating the generalization error). To do so, we need to pass the trained SVM model to the testSVM routine:

```splus
# print the time needed for training
cat( paste("LIBSVM took", svmObj$trainTime, "seconds to train.\n"))

# use the SVMBridge to test LIBSVM (here on the same data set)
testObj = testSVM(testDataFile = './data/australian_scale', model = svmObj$model)

# print the test error
cat( paste("Test error is ", round(testObj$testError,2), ". Testing took ", round (testObj$testTime,3) , " seconds.\n", sep = ''))
```

Congratulations, you trained your first LIBSVM model via the **SVMBridge**.
