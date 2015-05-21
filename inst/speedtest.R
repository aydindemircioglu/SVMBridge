	
	
# 08/15 svm
#	s = ssvm (x, y, gamma = 0.585568587668891, epsilon = 1e-6)

# 	writeSparseData (x,y, filename = "./tmp/testOutput.sparse")
# 	e = readSparseData (filename = "./tmp/testOutput.sparse", normalizeLabels = FALSE, verbose = TRUE)
# 	
# 	messagef("Testing Reading, e1071 vs swarmsvm, 5 times")
# 	t = microbenchmark( read.matrix.csr("../lab/data/poker/poker.combined.scaled"), times = 5)
# 	messagef("e1071:")	
# 	print (t)
# 	t = microbenchmark( readSparseData (filename = "../lab/data/poker/poker.combined.scaled"), times = 5)
# 	messagef("swarmsvm:")	
# 	print(t)
# 
# 	messagef("Testing Writing, e1071 vs swarmsv, 3 times")
# 	t = microbenchmark( write.matrix.csr(x, "tmp/poker.combined.scaled", y), times = 30)
# 	messagef("e1071:")	
# 	print (t)
# 	t = microbenchmark( writeSparseData (x, y, filename = "tmp/poker.combined.scaled"), times = 30)
# 	messagef("swarmsvm:")	
# 	print(t)


