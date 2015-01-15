Writing your own wrapper is also possible, and not too complicated. You must provide several callback functions. The most complex part will be provide functions to read and write the model. As a starter, you might want to look into the wrappers, **SVMBridge** provides. 

# SVM Parameter


Every SVM has some parameter, tha usually must be tuned. While LIBSVM has (for RBF kernel) two prominent parameter, the regularization term $C$ and the kernel bandwidth $\gamma$, this might not be true for other methods. For example, Budgeted SVM operates with $\lambda$ instead of $C$, with $\lambda = \frac{1}{2 C}$, and the $\gamma$ is twice the gamma of LIBSVM. Because of this, **SVMBridge** does not have a prebuild set of parameters. Every wrapper is free to use the parameter it needs. This entails that to use a wrapper, you must first know which parameter it uses. You can use the help function to get a description of a wrapper.


LIBSVMHelp() { }


# Training


Training can be either performed from data in memory (probably the most useful case), from the file system or skipped altogether. 
Note: In case you specify data from memory and from disc, **SVMBridge** will throw an error, as it will be confused about which source to use.


# Testing

Testing is done very similar to training. You have the option to use data from memory, from file system or skip testing. 
Note: In case you specify data from memory and from disc, **SVMBridge** will throw an error, as it will be confused about which source to use.


# Model file

If you do not specify any model file, ```callSVM``` will ask R for a temporary file and use this to write the model. After training, this model file will be read by the callback function of the wrapper. In some cases you might not need the model to be returned to you. In this case you can use the option 'skipModel'. For example:...

skipModel == TRUE

```splus
library(SVMBridge)
```

# Predictions

After testing, the predictions will again be stored in the SVM Object and can be accessed by e.g. ```SVMObj$predictions```. Depending on the purpose, the predictions are not always needed, for example when tuning the SVM parameters. To avoid extra I/O and memory consumption, reading back the predictions can be turned of by using the ```skipPredictions``` options. By default, ```skipPredictions``` is false, so ```callSVM``` will return the predictions in the SVM Object.

skipPredictions == TRUE



# Writing your own wrapper

The method name will define all function names! This is very important, if you do not abide to this rule, callSVM will not find the corresponding callbacks! Suppose you want to write a wrapper for mySVM. In this case, you will need to create a file exactly called "mySVM_wrapper.R'. Please note that the filename is case-sensitive. This is also true for all the callback functions. 

Your mySVM software must be split into two parts: Training and Testing. This might be the same executable, but be aware, that testing must work without training data. In case your package needs testing data at training time (e.g. if the model only specifies the index of the support vector in the training file instead of copying them into the model), you need to perform quite heavy tricks to get this going. In general, using other packages and skip **SVMBridge** might be a good idea, as **SVMBridge** strictly follows the LIBSVM way of training and testing. 


## The training parameters

You will get a bunch of parameters. You will need to decide which ones you want to handle. E.g. the LIBSVM wrapper handles $C$, $\gamma$ and $\epsilon$, but also uses $kernelCacheSize$. On the other hand, the LIBSVM wrapper currently has no option to control the shrinking, although LIBSVM has a parameter for this. Similarly you will need to decide, whether you want the user to have control over all parameters or only a relevant subset. The callback will then gather all the parameter and create a command-line string.

## Test parameters

Very similar to training parameters, the test parameter callback will assemble a given model file and test data and possibly other options.


extraParameters for testing??




