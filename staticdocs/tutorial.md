

# Tutorial

In this section we give all details necessary to use the **SVMBridge**.
We talk about all  parts needed to successfully use the package: Data, Models, Wrappers and Binaries.


1. [General Workflow](#GeneralWorkflow)
2. [Adding an  SVM package](#AddingSVMPackage)
3. [Software](#Software)
4. [Data](#Data)
5. [Models](#Models)
6. [Training](#Training)
7. [Testing](#Testing)
8. [Predicting](#Predicting)
9. [Vocabulary](#Vocabulary)


## <a name="GeneralWorkflow"></a>General Workflow

In general, the workflow is as follows:

- Register a new SVM package by calling addSVMPackage together with the path to a wrapper.
- Specify the path to the SVM solver, or let the SVMBridge search for the package.
- Load or create data in memory or on disk.
- Call ```trainSVM``` to train the SVM solver on your given data. It will produce a model.
- Call ```predictSVM``` to make prediction on new data.



## <a name="AddingSVMPackage"></a>Adding an  SVM package

The first thing to do is to make **SVMBridge** aware of a new SVM package.
This is done by the ```addSVMPackage``` function. It takes a method name, and optionally a wrapperName (the filename of the wrapper),  a wrapperPath (the path where to find the wrapper) and a softwarePath (the path
  where the software can be found).

If no wrapperPath is given, the default name "method_wrapper.R" will be assumed,
e.g. ```addSVMPackage ("LASVM")``` will assume "LASVM_wrapper.R" as wrapper name.
Similarly the wrapperPath is optional. If given, the **SVMBridge** will look
directly into this path, if the wrapper can be found there. No recursive search will be conducted. If no wrapper can be found, the **SVMBridge** will finally
look, if a  prepackaged wrapper (in the packages installation path) exists.

If a softwarePath is given, the SVMBridge will try to locate the corresponding
binaries in this path or in softwarePath/bin. Again, no recursive search will
be done to save resources.


##### Prepackaged wrappers

In case you want to use one of the prepackaged wrappers, you simply drop the wrapperPath.
In absense of a wrapperPath, the **SVMBridge** will then search the wrapper
 in the installation path of **SVMBridge**:

```splus
# will search automatically in the installation path of the SVMBridge
addSVMPackage("LASVM")
```

Alternatively, you can specify the installation path of the package by yourself:
```splus
addSVMPackage("LASVM" , wrapperPath = file.path (path.package("SVMBridge"), "wrapper"))
```

See [Wrappers](wrappers.Rmd) for more information about wrapper.


##### Searching for the wrapper

For convenience, ```findSVMWrapper``` will search for a wrapper recursively in a given folder. This comes in handy when training several different packages, especially when the software name and the name of the solver differ. The function takes only a method name and a searchPath and a flag whether the search should be conducted recursively.

If multiple wrappers exist inside subdirectories of the search path, **SVMBridge** will take the very first wrapper it finds.

**Notice**: This function will list all files in all subdirectories. This might have
a heavy toll on your general file I/O, so be cautious.



## <a name="Software"></a>Software

After adding a wrapper, the **SVMBridge** needs to know where the software binaries
can be found. You can either specify this directly via ```addSVMPackage```,
or the **SVMBridge** can search for the software binaries for you via the ```findSVMSoftware```
command.
Remember that if you forget to specify the softwarePath,  **SVMBridge** will not
understand where to find the binaries and subsequent calls like training or testing
 will not work.

Letting the **SVMBridge** find the software for you can be helpful in case you want to test several different solvers which are all located in a common directory, e.g. 'software/', but you' do not want to specify the directory directly. This might be the case, if the method you are calling and the path the software is located in differ, e.g. LLSVM is part of the BudgetedSVM
package, so to test several solvers together with LLSVM one would need to  write the following code

```splus
# train all SVMS on a problem
solvers = c("LLSVM", "LIBSVM", "SVMperf")
for (s in solvers) {
  path = s
  if (s == "LLSVM")
    path = "BudgetedSVM"
  addSVMPackage (s, softwarePath = file.path ("./software", path))
  # do now testing, training etc
}
```

This becomes somewhat easier with findSVMSofware:

```splus
# train all SVMS on a problem
solvers = c("LLSVM", "LIBSVM", "SVMperf")
for (s in solvers) {
  addSVMPackage (s)
  findSVMSoftware (s, "./software")
  # do now testing, training etc
}
```

For convience, there is also a ```findAllSVMSoftware``` function that will try to find the software for **all** registered SVM packages based on the given search path, e.g., if all your SVM software can be found inside the ./software directory, you can call ```findAllSVMSoftware(searchPath = "./software")```.


**Notice**: This function will list all files in all subdirectories. This might have
a heavy toll on your general file I/O, so be cautious.




## <a name="Data"></a>Data

In general, as any data is passed forward to the underlying binary
of the software package, the **SVMBridge** does not really care
about the format of the data. Nonetheless, practically all major
SVM packages use the "Sparse Format" (also called LIBSVM or SVMlight
sparse format), so
the **SVMBridge** internally chooses the  same format when it has to
write data from memory to disk.
This is true for both, training and test data.

You are not forced to use this format, in case your SVM solver uses a
different way to read and store data. But it is encouraged to use
LIBSVM's sparse format where possible as it became the de-facto
standard in the SVM machine learning community.

To allow reading and storing this LIBSVM sparse format,
 SVMBridge provides two routines ```readSparseData``` and ```writeSparseData```.
 The reason we do not use e1071's similar routines is that our routines
 are converting the data directly into a _dense_ R matrix. Furthermore,
 our routines are written completely in C, so they are much faster than
 e1071 (in case the data is not too sparse). You can use the e1071 routines
 in your wrappers in case you want to have a workflow with only truly
 sparse matrices.

Using the routines for sparse format inside the **SVMBridge** is quite simple.

```
D = readSparseData (filename = "./sparsedata.dat")
```

The data will be read into D, which is a list with a matrix X
corresponding to the data points and a vector Y corresponding to the labels.

Writing a data set is similar:
```
writeSparseData (filename = "./sparsedata.dat", X = D$X, Y = D$Y)
```

Notice that the data is written starting with the index "1:...". There are a few
solver that start indexing with 0:, in that case you can use the zeroBased
flag of both routines.


## <a name="Models"></a>Models

A model is just a list of important information about the trained SVM model.
It will contain all the support vectors (as a matrix), their corresponding
coefficient and some information about the used kernel.


## <a name="Training"></a>Training

In general training is performed by calling
```trainSVM``` function.
As the SVM solver, i.e. the wrapper and the corresponding software,
must already be known to the **SVMBridge**, training needs basically
only two parameters: The name of the SVM solver and the training data.

The training data can be given either as a path to the training file,
```trainDataFile``` or as a pair, ```trainDataX``` and ```trainDataY```.
Notice that you cannot specify both, trainDataFile and trainDataX or trainDataY.
In the latter case the **SVMBridge** will
first use the ```writeSparseData``` function to write the data temporarily
as LIBSVM sparse data to disk. The resulting path is then passed to the SVM software.
In this case the **SVMBridge** assumes that the SVM solver can deal with
LIBSVM sparse data.

There are some cases where it is necessary to pass other parameters to the SVM binary.
In this case, one can use ```extraParameter```.

For convenience, ```trainSVM``` also includes an easy subsampling algorithm to speed up training.
The only method is currently 'cutoff`, which will simply truncate the given data after the
specified rate (or number of elements). It will not re-shuffle the data.

On non-windows platforms there is also the parameter ```timeOut```. This will **force** the
SVM solver to stop after the given amount of time. This is helpful if a solver stalls.
Notice that a solver that times out will not produce a trained model, as it is hard stopped.



##### The training parameters

Besides the above parameters, the most important ones are those that control the
training of the SVM solver itself, like a cost value and the kernel parameter gamma.

You can specify such parameters just by adding them to the trainSVM call.
Thus, a common training call
looks like

``` trainObj = trainSVM ("LIBSVM", trainDataFile = "./data/australian.sparse", cost = 1, gamma = 1)```.

The training parameters are specific to the wrapper and SVM software package.



##### Trained Models



Usually all underlying SVM solvers will create a model on disk after training.
When training, you have the possibility to specify where this model should be
written to and whether you want to read the trained model file back into memory or not.
At times, if there is no need to peek into the model file, it is more performant
to skip reading back the model into R.

You can control this behaviour by setting the two parameters ```modelFile``` and
```readModelFile``` accordingly.

The ```modelFile``` parameter will make the trainSVM method to pass this path
to the underlying wrapper and the SVM solver resp.
If you do not specify the ```modelFile``` parameter, then (as practically all
command-line  SVM solvers will write their model to disk) the **SVMBridge** will
create a temporary model file.

The flag ```readModelFile``` controls whether the model of the SVMBridge will
be read by ```trainSVM``` and appended to the returned SVM Object.
By default, ```trainSVM``` will use a temporary model file and read
the model back into memory.

Notice that large data sets will usually produce large models, thus reading
the model might take some time and may even too large to be read into memory.
In general, if you are only interested in the subsequent test call and do not
need the model for further predictions, setting the readModelFile flag to FALSE
is a good idea, so no resources are spend to read the model, as testing will
need to write to model back to disk.

Notice that not specifying ```modelFile``` and setting ```readModelFile``` to FALSE will
result in a temporary model file which is not read back, so the trained model
will be simply lost. This might only make sense in very rare, special applications,
e.g. whent the primary goal is simply to see how long training takes.
In these cases, depending on the platform you are using, you might also want to
consider passing '/dev/null' as ```modelFile```. The underlying solver will then write its model
directly to '/dev/null', which is much faster than writing to disk, thereby reducing I/O times.





##### SVM  Train Result Object

Training will return a specific list as return object. It has currently the following two fields:

- trainTime
- model

Train time contains the time needed for the whole training process, measured with microbenchmark. The model is the trained model as discussed above.






## <a name="Testing"></a>Testing

Testing is done very similar to training by calling the ```testSVM``` or the ```predictSVM``` function, depending on whether one wants to predict or test.

**Note** that practically all supported SVM packages do not provide a
method for  prediction, but only for testing. The difference between both is that testing assumes that the given test data contains the correct labels (i.e. is in normal  LIBSVM sparse data format), so the accuracy on the test set can be computed.  Prediction on the other hand does not need any labels. Both are supported in the SVMBridge, here we use the ```testSVM```.


Similarly, there is the option to use test data from memory or from file system.
As above, in case you specify data from memory and from disc at the same time,
the **SVMBridge** will throw an error, as it will be confused about which source to use.

Furthermore, testing needs a trained model. Again you can either pass a model in
memory, e.g. part of an SVM training result object, or a path to a model on the file system.
A general ```testSVM``` call looks like this:

``` testObj = testSVM (model = trainObj$model, testDataFile = "./data/australian.sparse")```

By default, the predictions are written into a temporary file and are discarded after testing.
If you want to store the predictions permanently on disk, you can pass a path to the testSVM
function.
You can make ```testSVM``` to read the predictions to memory, if they are in a text format
where one label corresponds to one line, by using the ```readPredictions``` flag. In case your predictions look more complex you need to read them by yourself.

Depending on the purpose, the predictions are not always needed,
for example when tuning the SVM parameters. To avoid extra I/O and memory
consumption, reading back the predictions can be turned of by
using the ```readPredictions``` options.



##### Test parameters

Very similar to training parameters, the test parameter callback will
assemble a given model file and test data and possibly other options.
Additional parameters can again be passed by ```extraParameter```.


##### SVM Test  Result Object

Testing will produce a result object that will contain usually

- testError
- testTime

which correspond to  the error on the test data file and the execution time.
If specifiied, the predictions be read and  stored in the SVM test return object
as ```testObj$predictions```.





## <a name="Predicting"></a>Predicting

Predicting is a special form of Testing, thus all the comments on Testing hold also for predicting. The only difference is that prediction does not need any kind of labels, while testing does need that (for computing the accurcy on the test set). Technically, the difference between ```testSVM``` and ```predictSVM``` is that the later does not need any labels. Instead, when ```predictSVM``` is called, dummy labels are created and then ```testSVM``` is called. Therfore, the returned object is just a SVM test result object, though the testError is now removed, as it does not contain anything meaningful.

**Note:** When calling ```predictSVM``` with unlabeled data on disk, dummy labels have to be added to the data. Thus, the data has to read into memory, modified and written back. This prodcedure is therefore not cheap, but costs some I/O times. If speed is necessary, it might be a good option to label the data upfront, e.g. when creating the test data file, if applicable.




## <a name="Vocabulary"></a>Vocabulary

The SVM software is the compiled object on the file system, e.g. when you download and compile LIBSVM you will end  up with two binaries, one for training, one for testing. These files form an SVM software for us.

Glue code that handles controlling the SVM software is called an SVM wrapper. This contains e.g. routines for reading and writing an SVM model.

Finally, an SVM package is for us the internal object in the SVMBridge, that has basically consists of the wrapper and the software (e.g. a path to the binary).
