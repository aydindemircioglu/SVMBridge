The first step of MBO requires an initial set of evaluation points which is then evaluated by the black box function.
The basic procedure of MBO is an iterating loop of the following steps:
Firstly, a user defined surrogate model is fitted on the evaluated points, secondly, a new evaluation point is proposed
by an infill criterion and lastly, its performance is evaluated.
The result of this sequential procedure is the optimization path containing the best
parameter setting and the fitted surrogate model.

Once again, take a look at the already introduced basic example of the optimization of the one dimensional Rastrigin function.


```splus
library(soobench)
library(mlr)
library(mlrMBO)
obj.fun = generate_rastrigin_function(1)

par.set = makeNumericParamSet(len = 1, id = "x", lower = lower_bounds(obj.fun), upper = upper_bounds(obj.fun))
learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
control = makeMBOControl(
  propose.points = 1,
  iters = 5,
  infill.crit = "ei"
)

result = mbo(makeMBOFunction(obj.fun), par.set = par.set, learner = learner, control = control)
```

From this example we can easily recognize some **mlrMBO** essentials like parameters, learners and the control object.
Basically the following steps are needed to start a surrogate-based optimization with our package. Each step ends with
an R object, which is than passed to ```mbo()```, i. e, to the working horse of mlrMBO.

1. define the objective function
1. define the parameters of the objective function
1. (optionally generate an initial design)
1. define a learner, i. e., the surrogate model
1. set up a MBO control object, which offers a load of options
1. finally start the optimization

This web page will provide you with an in-depth introduction on how to set the ``mbo()`` parameters depending on the
desired kind of optimization.

# Objective Function


The first argument of ``mbo()`` is the name of the object function to minimize. The first argument of this object function has to be a list of values.
The function has to return a single numerical value. We demonstrate in this tutorial optimization of two simple functions: 5 dimensional ``ackley function`` from
package **soobench** (``objfun1``) and a self-constructed sine und cosine combination (``objfun2``). ``objfun1`` depends on 5 numeric parameters
while ``objfun2`` assumes 2 numeric and 1 discrete parameters..


```splus
library(mlrMBO)
library(soobench)
objfun1 = generate_branin_function() # old soobench version: objfun1=branin_function()
```

```
## Error: konnte Funktion "generate_branin_function" nicht finden
```

```splus
objfun2 = function(listOfValues)
{
  x = listOfValues[[1]]
  k = listOfValues[[2]]
  method = listOfValues[[3]]
  perf = ifelse(listOfValues[[3]] == "a", k * sin(x) + cos(x),
               sin(x) + k * cos(x))
  return(perf)
}
```


We aim to maximize ``objfun2``. In a following Section will be shown how to set the ``MBOControl`` object in order to switch the maximization problem into a minimization one.

Parameter set
=============

The second argument of ``mbo()`` function, ``par.set``,  has to be a ParamSet object from **ParamHelpers** package, which provides information about parameters
of the objective function and their constraints for optimization.
The lower and upper bounds for ``objfun1`` parameters can be easily obtained using **soobench** function ``lower_bounds``. For ``objfun2`` we
assume ``x`` from interval [0,1] and ``k`` from interval [1,2]. Parameter ``method`` can be either ``"a"`` or ``"b"``.


```splus
library(ParamHelpers)
par.set1 = makeNumericParamSet(len = number_of_parameters(objfun1), lower = lower_bounds(objfun1), upper = upper_bounds(objfun1))
```

```
## Error: Objekt 'objfun1' nicht gefunden
```

```splus
par.set2 = makeParamSet(
  makeNumericParam("x", lower = 0,upper = 1),
  makeIntegerParam("k", lower = 1, upper = 2),
  makeDiscreteParam("method", values = c("a", "b"))
)
```


Initial Design
==============
The third argument of ``mbo()`` function - ``design`` - is the initial design  with default setting ``NULL``.
Users have two options for initializing of this design:
either to create itself a design und assign the ``design`` parameter with it or
to provide settings for design generation in  ``MBOControl`` object.

In the first case it is recommendable to use ``generateDesign`` function from **ParamHelpers** package.
However, if special designs are desired (e.g., orthogonal designs), its interface
has to be the same as the interface of the ``generateDesign`` objects. Particular attention has to be paid to the setting of the ``trafo`` attribute.

In the second case following parameters of ``MBOControl`` object
are relevant:
 * ``init.design.points``: Initial design size, default setting is 20.
 * ``init.design.fun``: Any function from **lhs** package, default is ``maximinLHS``.
 * ``init.design.args``:  List of further arguments passed to ``init.design.fun``, default is empty list.

Here we will use the first option for ``objfun1`` and the second option for ``objfun2``. In the both cases we will generate an initial design of size
5*(dimension of the object function).



```splus
library(lhs)
init.design.points1 = 5 * sum(getParamLengths(par.set1))
```

```
## Error: Objekt 'par.set1' nicht gefunden
```

```splus
init.design.fun1 = randomLHS
set.seed(1)
design1 = generateDesign(n = init.design.points1, par.set = par.set1, fun = init.design.fun1, trafo = FALSE)
```

```
## Error: Objekt 'init.design.points1' nicht gefunden
```

```splus
# will be used later as makeMBOControl()  arguments
init.design.points2 = 5 * sum(getParamLengths(par.set2))
init.design.fun2 = maximinLHS
init.design.args2 = list(k = 3, dup = 4)
```


Surrogate Model
==============
Attribute ``learner`` of the ``mbo()`` function allows us to choose an appropriate surrogate model for the parameter optimization.
It can be easily done using the ``makeLearner`` function from **mlr** package.
List of implemented learners can be seen using ?learners command. <!-- Here an appropriate link!-->
The choice of the surrogate model depends on the parameter set of the objective function.
While kriging models are advisable for the numeric parameters, random forest models can be used if at least one parameter is factorial.
In our example we consider these two surrogate models:
``kriging`` for optimizing of ``objfun1``  and ``random forest`` for ``objfun2``.


```splus
library(mlr)
learner_km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
```

```
## Loading required package: DiceKriging
```

```splus
learner_rf = makeLearner("regr.randomForest")
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

However, in some cases it is necessary to modify the learners (e.g., in order to get the standard error prediction for design points).
This will be discussed und illustrated in the section "Experiments and Output".


MBOControl
==========
User can change the default settings of the ``mboControl`` object
using ``makeMBOControl`` function, in order to configure ``mbo()`` in the desired
way. Settings regarding the initial design were already mentioned in Section "Initial Design".

Attribute ``infill.crit``
-------------------------
One of the most important issues is to define how the next design points in
the sequential loops have to be chosen. Firstly, we have to choose the infill criterion using the  ``infill.crit`` attribute.
At the moment four possibilities are implemented:
* ``mean``: mean response of the surrogate model,
* ``ei``: expected improvement of the surrogate model,
* ``aei``: augmented expected improvement, which is especially useful for the noisy functions,
* ``lcb``: lower confidence bound which is the additive combination of mean response and mean standard error estimation
of the surrogate model (response - lambda * standard.error). The default value of lambda is 1, but it can be easily changed by
the ``infill.crit.lcb`` attribute.


Attribute ``infill.opt.fun``
--------------------------

The attribute ``infill.opt.fun`` sets how the next point to evaluate should be proposed given an infill criterion. The possibilities are:
``focussearch``, ``cmaes`` and ``ea``.

In the first case a focus search is employed. The common procedure is as follows: in the first step an lhs design is sampled in the
parameter space (by ``randomLHS`` function) and the design point with the best  prediction of the infill criterion is determined. User can set the
size of this design by ``infill.opt.focussearch.points`` attribute of ``MBOControl`` object (default value is 10000). In the second step the parameter
space is shrunk around the best design point in a certain way which should not be discussed in detail here. First and second steps are repeated
iteratively ``infill.opt.focussearch.maxit`` times (default is 5) while the best seen value of the infil criterion is passed back.


If ``infill.opt.fun`` is ``cmaes``, the point, which optimizes the
infill criterion, is chosen via ``cma_es`` function of  **cmaes** package. Control argument for ``cmaes`` optimizer can be provided in
``infill.opt.cmaes.control`` attribute (default is empty list).


If ``infill.opt.fun`` is ``ea`` a simple (mu+1)-evolutionary optimization algorithm is used to optimize the infill criterion.
The population size, mu, can be set by ``infill.opt.ea.mu`` attribute (default value is 10).
(mu+1) means that in each population only one child is generated using crossover und mutation operators (from **emao** package).
Parameters ``eta`` and ``p`` of these both operators can be adjust via ``infill.opt.ea.sbx.eta``, ``infill.opt.ea.sbx.p``,
``infill.opt.ea.pm.eta`` and ``infill.opt.ea.pm.p`` attributes respectevely.
The default number of EA iterations is 500 and can be changed by ``infill.opt.ea.maxit`` attribute.

As all three infill optimization strategies do not guarantee  to find the global optimum, users can set the number of restarts by
``infill.opt.restarts`` attribute (default value is 1).
After conducting the desired number of restarts the design point with the best infill criterion value is passed back to the MBO loop.

Please note that just ``focussearch`` optimizer is suitable for the case of factor parameters in the parameter set!


Further attributes
------------------

The number of sequential steps (iterations) can be set in attribute ``iters`` (default setting 10).
Furthermore, user can specify whether the function have to be minimized or maximized in attribute ``minimize`` (default setting TRUE , e.g.,  minimization).

There are also many other attributes which user can set in a desired way like how often should the surrogate model be stored or resampled during the optimization.
The list of all attributes is provided in the software documentation.


Constructing  of ``mboControl`` object
------------------

Let us construct ``mboControl`` objects for our two object functions.


```splus
control1 = makeMBOControl(
  iters = 10,
  infill.crit = "ei",
  infill.opt = "cmaes")


control2 = makeMBOControl(
  minimize = FALSE,
  iters = 10,
  infill.crit = "mean",
  infill.opt = "focussearch",
  init.design.points = init.design.points2,
  init.design.fun = init.design.fun2,
  init.design.args = init.design.args2)
```

Experiments and Output
======================

Now we will apply the mbo() function to optimize the both objective functions

Optimization of ``objfun1``
---------------------------
As the first argument of the objective function has to be a list of values and for the
objfun1 it is two-dimensional numeric vector, we wrap objfun1 with ``makeMBOFunction()`` function
which was created extra for this purpose.



```splus
library(mlrMBO)
library(BBmisc)

mbo1 = mbo(makeMBOFunction(objfun1), par.set1, design = design1, learner = learner_km, control = control1, show.info = TRUE)
mbo1
getOptPathY(mbo1$opt.path, "y") # get all y values
```

The output of mbo function is a structure of several variables. The most important are:

* x: the best point of the parameter space
* y: the associated best value of the objective function
* opt.path: optimization path. See **ParamHelpers** for further information.
* models: If no other setting was provided in the ``MBOControl`` object, the last estimated surrogate is given here.
* ...


<!-- FIXME: get optimization path as data.frame !-->

We can also change some attributes of the ``MBOControl`` object and run mbo() function again


```splus
control1$infill.crit = "mean"
control1$infill.opt.fun = "focussearch"
mbo1=mbo(makeMBOFunction(objfun1), par.set1, design = design1, learner = learner_km, control = control1, show.info = FALSE)
mbo1$y
```


Optimization of ``objfun2``
---------------------------

Let us apply **mlrMBO** package to optimize object2 function, which contains one factor variable.
We have already mentioned before, that in this case just ``focussearch`` infill optimization function is suitable.
If we use ``mean`` infill criterion any kind of model which can handle with factor variables can be used here (like random tree,
random forest, linear model and many others).



```splus
mbo2 = mbo(objfun2, par.set2, design = NULL, learner = learner_rf, control = control2, show.info = FALSE)
mbo2$y
```

In contrast, if one will apply ``ei`` or ``lcb`` infill criteria,
the ``predict.type`` attribute of the learner have be set to ``se``, if possible. A list of regression learners which support it can be viewed by:


```splus
#listLearners(type = "regr", se = TRUE)
```

<!-- If no comment here, we get a lot warning message !-->

We hence modify the random forest learner and optimize ``objfun2`` by ``ei`` infill criterion.


```splus
learner_rf = makeLearner("regr.randomForest", predict.type = "se")
control1$infill.crit = "ei"
mbo2 = mbo(objfun2, par.set2, design = NULL, learner = learner_rf, control = control2, show.info = FALSE)
mbo2$y
```


Finally, if a learner which not support the ``se`` prediction type should by applied for the optimization with ``ei`` infill criterion,
there is a possibility to create a bagging model with the desired characteristics. Attribute ``bag.iters`` provide the number of models in the
ensemble, see documentation for ``makeBaggingWrapper`` of **mlr** package.



```splus
learner_rt = makeLearner("regr.rpart")
bag_rt = makeBaggingWrapper(learner_rt, bag.iters = 5,predict.type = "se")
mbo2 = mbo(objfun2, par.set2, design = NULL, learner = learner_rf, control = control2, show.info = FALSE)
mbo2$y
```

<!--
 TODO

1) noisy optimization example

2) mulicrit

3) multipoint
!-->
