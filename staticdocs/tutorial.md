

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
8. [Vocabulary](#Vocabulary)


## <a name="GeneralWorkflow"></a>General Workflow

In general, the workflow is as follows:

- Register a new SVM package by calling addSVMPackage together with the path to a wrapper.
- Specify the path to the SVM solver, or let the SVMBridge search for the package.
- Load or create data in memory or on disk.
- Call ```trainSVM``` to train the SVM solver on your given data. It will produce a model.
- Call ```testSVM``` to make prediction on new data.



## <a name="AddingSVMPackage"></a>Adding an  SVM package

The first thing to do is to make **SVMBridge** aware of a new SVM package.
This is done by the ```addSVMPackage``` function.
It takes a method name, a wrapperName (the filename of the wrapper), a wrapperPath (the path where to find the wrapper) and a softwarePath.

If no wrapperPath is given, it defaults to "method_wrapper.R", e.g. ```addSVMPackage ("LASVM")``` would have "LASVM_wrapper.R" as wrapper name. Similarly the wrapperPath is optional. If given the wrapper will be searched in this path. If no wrapper can be found, the **SVMBridge** will search for the wrapper among the prepackaged wrappers.

If a softwarePath is given, the SVMBridge will try to detect the corresponding binaries directly in this path or in softwarePath/bin. It will not do any recursive search.

##### Prepackaged wrappers

In case you want to use one of the prepackaged wrappers, you simply drop the wrapperPath. The **SVMBridge** will then search the wrapper
 in the installation path of **SVMBridge**:

```splus
# will search automatically in the installation path of the SVMBridge
 addSVMPackage("LASVM")
```

Alternatively, you can specify the installation path of the package by yourself:
```splus
addSVMPackage("LASVM" , file.path (path.package("SVMBridge"), "wrapper"))
 ```

 After that you can add the path of the compiled software.


See [Wrappers](wrappers.Rmd) for more information about wrapper.


##### Searching for the wrapper

For convenience, ```findSVMWrapper``` will search for a wrapper recursively in a given folder. This comes in handy, if training different several packages. The function takes only a method name and a searchPath and a flag whether the search
should be conducted recursively.


Sometimes it is convenient to allow the user to specify the SVM software he needs. In this case it is no good to specify neither the wrapper nor the software by hand. Therefore **SVMBridge** allows you not only to automatically find the software, but also the wrapper. This only works, if the wrapper follows the naming convention, i.e. the wrapper for mySVM needs to be called mySVM_wrapper.R.  To initiate the search, just call ```findSVMWrapper (method = "mySVM", searchPath = "./mySVM")```. The specified searchPath will, just as ```findSVMSoftware```, be searched recursively. Notice also that if multiple such wrappers exist inside a directory (in subdirectories), **SVMBridge** will stop searching after the very first hit.





## <a name="Software"></a>Software

After you added your wrapper, if you did not specified a softwarePath, **SVMBridge** can find the corresponding binary files for you.
Remember that if you forget to specify the softwarePath,  **SVMBridge** will not understand where to find the binaries and subsequent calls like training will not work.

Letting the **SVMBridge** find the software for you can be helpful in case you want to test several different solvers which are all located in a common directory, e.g. 'software/', but you' do not want to specify the directory directly. This might be the case, if the method you are calling and the path the software is located in differ, e.g. LLSVM is part of the BudgetedSVM
package, so to test several solvers together with LLSVM one would need to  write the following code

```
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

```
# train all SVMS on a problem
solvers = c("LLSVM", "LIBSVM", "SVMperf")
for (s in solvers) {
  addSVMPackage (s)
  findSVMSoftware (s, "./software")
  # do now testing, training etc
}
```

For convience, there is also a findAllSVMSoftware function that will try to find the software of all registered SVM packages based on the given search path, e.g., if all your SVM software can be found inside the ./software directory, you can call ```findAllSVMSoftware(searchPath = "./software")```.


## <a name="Data"></a>Data

As LIBSVM writes its data in a 'Sparse Format',
the **SVMBridge** internally handles the same format.
You are not forced to use this format, in case your SVM solver uses a different way to read and store data. But it is encouraged to use LIBSVM's sparse format as it became the de-facto standard in the SVM machine learning community.
To allow reading and storing this LIBSVM sparse format,
 SVMBridge provides two routines readSparseData and writeSparseData.
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

Notice that the data is written starting with the index "1:...". There are a few solver that start indexing with 0:, in that case you can use the zeroBased flag of both routines.






## <a name="Models"></a>Models

Usually the underlying SVM solvers will create a model on disk after training. 
You have two parameters in ```trainSVM``` to work with these models: modelFile and readModelFile.

The modelFile parameter will make the trainSVM method to pass this path
to the underlying wrapper and the SVM solver resp.
 Usually, the SVM solver will write its trained model then to this file.
 If you do not specify and modelFile parameter, then (as several solvers like LIBSVM etc need a path to write their model to) the **SVMBridge** will create a temporary model file.

At times, the model needs to be re-read into memory, e.g. to analyze it.
The flag 'readModelFile' controls whether the model of the SVMBridge will be read by ```trainSVM``` and appended to the returned SVM Object.
Notice that large data sets will usually produce large models, thus rereading the model might take some time and may even too large to be read into memory.
In general, if you are only interested in the subsequent test call and do not need the model for further predictions, setting the readModelFile flag to FALSE is a good idea, so no resources are spend to read the model.

Notice that not specifying  a modelFile and setting readModelFile to FALSE will result in a temporary model file which is not read back, so the trained model will be simply lost. This might only make sense in very rare, special applications.
In these cases, depending on the platform you are, you might also want to consider passing '/dev/null' as modelFile. The underlying solver will then write its model
directly to '/dev/null', which is much faster than writing to disk, thereby reducing I/O times.

By default, ```trainSVM``` will use a temporary model file and read the model back into memory.



## <a name="Training"></a>Training

In general training is performed by calling
```trainSVM``` function. 
As the SVM solver, i.e. the wrapper and the corresponding software, 
must already be known to the **SVMBridge**, training needs basically 
only two parameters: The name of the SVM solver and the training data.

The training data can be given either as a path to the training file (trainDataFile) or 
as a pair, trainDataX and trainDataY. In the later case the **SVMBridge** will 
first use the writeSparseData function to write the data temporarily 
as LIBSVM sparse data  to disk. The resulting path is then passed to the SVM software.
Notice that you cannot specify both, trainDataFile and trainDataX or trainDataY.

By default the trained model, which is usually written to disk by the SVM software,
will be read back and stored in the SVM result object. You can control this behaviour
by setting the flag readModelFile accordingly.

There are some cases where it is necessary to pass other parameters to the SVM binary.
In this case, one can use extraParameter. 

For convenience, trainSVM also includes an easy subsampling algorithm to speed up training.
The only method is currently 'cutoff`, which will simply truncate the given data after the 
specified rate (or number of elements). 

On non-windows platforms there is also the parameter 'timeOut'. This will *force* the 
SVM solver to stop after the given amount of time. This is helpful if a solver stalls.
Notice that a solver that times out will not produce a trained model, as it is hard stopped.



##### The training parameters

Besides the above parameters, the most important ones are those that control the 
training of the SVM solver, like a cost (C) and the kernel parameter gamma.

You can specify such parameters just by adding them to the trainSVM call.
Thus, a common training call 
looks like ```svmObj = trainSVM ("LIBSVM", trainDataFile = "./data/australian.sparse", cost = 1, gamma = 1)```.

The training parameters are specific to the wrapper and SVM software package.




##### SVM  Train Result Object

Training will return a specific list as return object. It has currently the following two fields:

 -trainTime
 -model
 
 Train time contains the time needed for the whole training process, measured with microbenchmark. The model is the trained model.
 
 




## <a name="Testing"></a>Testing

Testing is done very similar to training by calling the ```testSVM``` function 
Similarly, there is the option to use test data from memory or 
from file system. 
As above, in case you specify data from memory and from disc, 
the **SVMBridge** will throw an error, as it will be confused about which source to use.

Furthermore, testing needs a trained model. Again you can either pass a model in
memory, e.g. part of an SVM training result object, or a path to a model on the file system.

By default, the predictions are written into a temporary file and are discarded after testing.
You can make testSVM to re-read the predictions (in case they are exactly in a text format
with one label=one line) by using the readPredictions flag.
If you want to store the predictions permanently on disk, you can pass a path to the testSVM 
function. 



##### Test parameters

Very similar to training parameters, the test parameter callback will 
assemble a given model file and test data and possibly other options.
Additional parameters can again be passed by extraParameter.



##### Predictions

After testing, if specifiied, the predictions will again be stored in the SVM test return object and 
can be accessed by e.g. ```testObj$predictions```. 
Depending on the purpose, the predictions are not always needed, 
for example when tuning the SVM parameters. To avoid extra I/O and memory 
consumption, reading back the predictions can be turned of by 
using the ```readPredictions``` options. 




## <a name="Vocabulary"></a>Vocabulary

The SVM software is the compiled object on the file system, e.g. when you download and compile LIBSVM you will end  up with two binaries, one for training, one for testing. These files form an SVM software for us.

Glue code that handles controlling the SVM software is called an SVM wrapper. This contains e.g. routines for reading and writing an SVM model.

Finally, an SVM package is for us the internal object in the SVMBridge, that has basically consists of the wrapper and the software (e.g. a path to the binary).
