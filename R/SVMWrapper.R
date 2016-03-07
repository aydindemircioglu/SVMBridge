#
# SVMBridge
#		(C) 2015, by Aydin Demircioglu
#
# SVMBridge is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SVMBridge is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# Please do not use this software to destroy or spy on people, environment or things.
# All negative use is prohibited.
#



#' Return supported SVM properties.
#'
#' @param 	type 		Type of SVM. Will be currently ignored!
#'
#' @note	type parameter is currently ignored!

getSupportedSVMProperties = function(type = NA_character_) {
  return (c("twoclass", "multiclass"))
}




#' General class for any SVM wrapper.
#'
#' This will create an object of class SVMWrapper for a given name.
#
#' @param 	name		Name of the class/object to create
#' @param 	par.set		Parameter set (as a ParamHelpers class) for object
#' @param 	properties		Properties in form of strings, e.g. multiclass, twoclass. See docs for more info.
#' @param 	note		Notes for object, e.g. full name.
#'
#' @note	This function will first remove all member functions to make sure
#'		that no trash is lying around.
#'
#' @export

createSVMWrapperInternal = function (name, par.set, properties = character(0L), note = "") {

	# check first
	checkmate::assertString(name)
	checkmate::assertString(note)
	checkmate::assertClass(par.set, classes = "ParamSet")
	checkmate::assertSubset(properties, getSupportedSVMProperties("SVM"))


	# remove old stuff lying around
	# TODO: one must be able to do this better, but how? can anyone fix this :/
	method = name
# 	suppressWarnings( rm (envir = globalenv(), list = c(
# 		paste0("checkModel", ".", method),
# 		paste0("createTestArguments", ".", method),
# 		paste0("createTrainingArguments", ".", method),
# 		paste0("computeOptimizationValues", ".", method),
# 		paste0("detectModel", ".", method),
# 		paste0("extractTestInfo", ".", method),
# 		paste0("extractTrainingInfo", ".", method),
# 		paste0("findSoftware", ".", method),
# 		paste0("print", ".", method),
# 		paste0("readModel", ".", method),
# 		paste0("readPredictions", ".", method),
# 		paste0("writeModel", ".", method) )))


	svmPackage = BBmisc::setClasses(list(
    method = method,
    properties = unique(properties),
    par.set = par.set,
    name = name,
		note = note
  ), c(name,"SVMWrapper"))

	return (svmPackage)
}


createSVMWrapper = function (x) {
	UseMethod ("createSVMWrapper")
}


createTrainingArguments = function (x, trainDataFile = NULL, modelFile = NULL, verbose = FALSE, ...) {
	UseMethod("createTrainingArguments")
}


createTestArguments = function (x, testDataFile = NULL, modelFile = NULL, predictionsFile = NULL, verbose = FALSE, ...) {
	UseMethod("createTestArguments")
}


extractTrainInfo = function (x, output, verbose = FALSE) {
	UseMethod ("extractTrainInfo")
}


extractTestInfo = function (x, output, verbose = FALSE) {
	UseMethod ("extractTestInfo")
}


readModel = function (x, modelFile = './model', verbose = FALSE) {
	UseMethod("readModel")
}


writeModel = function (x, model = NA, modelFile = "./model", verbose = FALSE) {
	UseMethod("writeModel")
}


detectModel = function (x, modelFile = "./model", verbose = FALSE) {
	UseMethod("detectModel")
}


readPredictions = function (x, predictionsFile = NULL, verbose = FALSE) {
	UseMethod ("readPredictions")
}


findSoftware = function (x, searchPath = "./", verbose = FALSE) {
	UseMethod ("findSoftware")
}
