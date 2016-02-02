
# prepare data
X = diag(c(1,2,3,4,5))
gamma = 0.01

rbf <- kernlab::rbfdot (sigma = gamma)
k_sv = kernlab::kernelMatrix(rbf, X)
print (k_sv)

