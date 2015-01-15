# Introductory example

**Info:** this guide gives you an overview of the typical call of some SVM Package with **SVMBridge**. For a much more
detailed introduction see [the next chapter](introduction.md).

Here we provide a Quickstart example for you to make yourself familiar with **SVMBridge**. We aim to use the LIBSVM package for a simple SVM classification. Remember, that a general machine learning problem usually consists of two parts: Training and Testing. 

As **SVMBridge** is just an easy way to call a precompiled SVM package (also called a 'wrapper'), we first need such an SVM package. In this introduction, we simply use the probably best known SVM package, LIBSVM. Download the source package from LIBSVM webpage (currently at http://www.csie.ntu.edu.tw/~cjlin/libsvm/). Proceed by downloading the package as zip/tar or directly from github. Put it into any folder you like. We here assume that you have a software directory and unzipped LIBSVM under 'software/LIBSVM'. Then go to this directory and compile LIBSVM, by calling 'make' from the command line (Make sure to have all the necessary c++ packages installed). After successful compilation you will have two binary executables: svm-train and svm-predict. Make sure you can execute them by just calling './svm-train' (an extensive help page should be shown). 
You also need some training and test data. The LIBSVM currently provides many different benchmark data sets (currently at 'http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/'). To keep things simply, we will use the same data set for testing and training. Download the scaled australian data set (currently at 'http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/australian_scale') and put it into a folder, we assume here that you download it to  'software/data/australian_scale'.
Now you can write your first simple R-script:

```splus
library(SVMBridge)

# let SVMBridge find the LIBSVM package. We assume here that you are in the './software' directory
findSVMSoftware (method = 'LIBSVM', searchPath = "./LIBSVM") 

# use the SVMBridge to train LIBSVM 
svmObj =  callSVM(method = "LIBSVM", trainDataFile = './data/australian_scale', cost = 1, gamma = 1) 
```

**Note:** In case something goes wrong, you can add the verbose = TRUE flag to the callSVM function. Most probably your pathes are wrong, e.g. **SVMBridge** cannot find the LIBSVM executable or the data set.

The ```callSVM``` function will return an object that contains basically the training error, testing time (including I/O) and the final SVM model. See ... for more information about the SVM object, callSVM returns. We are going to print the time LIBSVM needed for training and then go ahead and test it on the same data set (for convenience, in  general testing on the training set will greatly underestimate the generalization error). For that we need to pass the trained model to the callSVM routine. Notice, that callSVM will automatically skip training, if no training file is given:

```splus
# print the time needed for training
print( paste("LIBSVM took", model$trainTime, "seconds to train."))

# use the SVMBridge to test LIBSVM (here on the same data set)
svmObj =  callSVM(method = "LIBSVM", testDataFile = './data/australian_scale', model = svmObj$model) 
```

**SVMBridge** comes in handy, when one has the data already in memory. Let us generate some training and test data by using the build-in data set iris: 


