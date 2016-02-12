


# Writing Wrappers

To write your own wrapper, you simply have to provide an R-file that handles the most important things like reading and writing models, and communicating with the executables. Technically, a wrapper is just an S3 Object. We will talk about the methods below.

Notice that if you write your own findSoftware routine for your wrapper, you might need some platform dependent checks, e.g. the LIBSVM binray is simply called "svm-train" on Linux and Mac OSX, but "svm-train.exe" on Windows. You can use the functionality provided by R to check for the platform. An example can be seen in the LIBSVM wrapper code.


### S3 Object

A wrapper is a simple S3 Object. If you do not know about S3 Object, you might similarly think about providing several callback functions to the **SVMBridge**. The most complex part will be provide functions to read and write the model. As a starter, you might want to look into the prepackaged wrappers, **SVMBridge** provides.

The following parts have to be written, where x is the S3 Object.


##### createTrainingArguments

```splus
createTrainingArguments = function (x, trainDataFile = NULL, modelFile = NULL, verbose = FALSE, ...)
  trainArgs = ...
  return (trainArgs)
```

This function will transform given parameters into a string that will be used as arguments when executing the corresponding SVM train binary. It will return the string.


##### createTestArguments

```
createTestArguments = function (x, testDataFile = NULL, modelFile = NULL, predictionsFile = NULL, verbose = FALSE, ...) {
  trainArgs = ...
  return (trainArgs)
```

This function will transform given parameters into a string that will be used as arguments when executing the corresponding SVM test binary. It will return the string.


##### extractTrainInfo

```
extractTrainInfo = function (x, output, verbose = FALSE) {
	trainList = ...
  return (trainList)
}
```

This function will extract information from the output of the SVM train binary.
In general, either a list or a scalar can be returned. In case of a scalar,
this must be the training error. If it is a list, then training error should
be part of the this list.



##### extractTestInfo

```
extractTestInfo = function (x, output, verbose = FALSE) {
  testList = ...
  return (testList)
}
```

This function will extract information from the output of the SVM test binary.
In general, either a list or a scalar can be returned. In case of a scalar,
this must be the test  error. If it is a list, then test error should
be part of the this list.


##### readModel

```
readModel = function (x, modelFile = './model', verbose = FALSE) {
  model = ...
  return (model)
}
```

This function will read a model on disk into memory. The model should be stored as a list and returned.


##### writeModel

```
writeModel = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
  err = ...
  return (err)
}
```

This function will write a model to disk. It will return an error code.


##### detectModel

```
detectModel = function (x, modelFile = "./model", verbose = FALSE) {
	return (bool)
}
```

This function will determine if a given model on disk is compatible with the
model for the SVM solver. Usually a simple check (e.g. does the file start
with the line 'SVM') is conducted. It will return either TRUE or FALSE.



##### readPredictions

```
readPredictions = function (x, predictionsFile = NULL, verbose = FALSE) {
	return (predictions)
}
```

This function will read the predictions from the predictions file and
return them as a list.


##### findSoftware

```
findSoftware = function (x, searchPath = "./", verbose = FALSE) {
	x$trainBinaryPath = ...
  x$testBinaryPath = ...
  return (x)
}
```

This function will determine whether the  binaries of the SVM solver
exist in the given searchPath or not. It shall not do any kind of
recursive search (this is done in the SVMBridge itself),  but only seek
out for the test and trining binary. Both should be put back into the
object x and x must be returned. If the binaries are not found, NULL
should be put into the variables instead.
