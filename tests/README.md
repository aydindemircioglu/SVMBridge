# Tests

Due to the technical awkwardness of the **SVMBridge** being used on different platforms, there are different (level of) tests to make sure the **SVMBridge** is working as intended.

## Unit Tests

These are normal unit tests that test the code of the **SVMBridge**. E.g. whether creating SVM Objects does work. These test cannot test specific wrappers as ```LASVM_wrapper.R```, because this depends on the LASVM binaries being available. In general we cannot (and should not) assume that.

## Wrapper Tests

To make sure the wrappers included in this package work as expected, there is another layer of tests. These tests are called outside the testthat environment (and thus also do not get executed for CRAN)-- but they **should** be called before any release (Actually there is a release flag in the compile script in the main directory of the repository). The scripts will do several OS-specific stuff: Downloading all the binaries (either from our github repository or from the source directly), and test if the wrappers can work with these binaries.

## Directory structure

There is different data (usual datasets, models and predictions) in the ```tests/data``` folder. The ```tests/dummy``` directory contains a dummy wrapper and dummy executable. Moreoever, the wrapper tests can be found under ```tests/wrappertests/```.  Finally the unit tests are below ```tests/testthat```.
