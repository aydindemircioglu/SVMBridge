

# Advanced

In this section we talk about some advanced things.


### Implementing your own data format

To implement your own data format, recall that you have two ways to provide data
to your SVM software. The first is by specifiying a path to a data file on the filesystem.
In this case, SVMBridge will not explicitly work with the data, so you are free to use any
 format, as long as your SVM software can handle this. The second way is to pass data as
 a matrix to the SVMBridge. Here, SVMBridge needs to dump the given data first. By
 default, the SVMBridge will write the data in the LIBSVM Sparse Format. You are
 free to overwrite this behaviour, by rewriting the convertData function of the
 SVM Wrapper Object.


Let us give an easy example.

LABELS..
MULTICLASS..



# Advanced

In some cases you might to want to modify the internal object corresponding to a method.
**SVMBridge** stores these in its own environment. You can access these Conveniently
by using ```getSVMObject()```. After modifying them, you can store the object back by
overwriting ```setSVMObject()``, so that the SVMBridge can work with it. Notice that
broken objects might lead to unforseen behaviour and crashes.
