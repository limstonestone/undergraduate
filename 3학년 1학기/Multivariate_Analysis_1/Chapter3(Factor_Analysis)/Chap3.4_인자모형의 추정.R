### CODE 3.4.1
# PCFA Steps for KLPGA data
# [Step 1] Data Matrix X
Data1.3.2 = read.table("klpga.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.2
rownames = rownames(X)
p = ncol(X)
head(X)

# [Step 2] Covariance Matrix S(or Correlation Matrix R)
R = round(cor(X), 3)
R

# [Step 3] Spectral Decomposition (# of factor)
eigen.R = eigen(R)
round(eigen.R$values, 2)  # Eigenvalues
V = round(eigen.R$vectors, 2) # Eigenvectors
V

# [Step 4] Number of factors : m (# of factor)
gof = eigen.R$values/p*100 # Goodness-of fit
round(gof, 3)
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Factor Number", ylab="Eigenvalue")

# [Step 5] Factor Loadings and Communality
V2 = V[, 1:2]
L = V2 %*% diag(sqrt(eigen.R$values[1:2])) # Loading matrix : 인자적재 행렬
rownames(L) = colnames(X)
colnames(L) = c("요인1", "요인2")
round(L, 3)
round(diag(L %*% t(L)), 3) # Communality : 공통성 -> 설명력과 유사한 느낌

# [Step 6] Specific Variance : 특정분산(Psi) (= 1- Communality)
Psi = diag(R - L %*% t(L))
round(Psi, 3)

# [Step 7] Residual Matrix (전체 = 공통성 + 특정분산 + 잔차)
Rm = R - (L %*% t(L) + diag(Psi))
round(Rm, 3)

# PCFA using the principal()
library(psych)
pcfa = principal(R, rotate="none")
pcfa

round(pcfa$values, 2)
gof = pcfa$values / p*100 # Goodness-of fit
round(gof, 3)
round(pcfa$residual, 2)

### CODE 3.4.3
# MLFA Steps for KLPGA data
# Data Matrix X
Data1.3.2 = read.table("klpga.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.2
rownames = rownames(X)
p = ncol(X)
Z = scale(X, scale=T)

# Covariance Matrix S(or Correlation Matrix R)
R = round(cor(X), 3)
R

# ML Estimation using the factanal()
library(psych)
mlfa = factanal(Z, factors=2, rotation="none", score="regression")
mlfa # number of Factors -> Cumulative var!

# Residual Matrix
L = mlfa$loadings[, 1:2] # factor loading
Psi = mlfa$uniquenesses # specific variance
Rm = R - (L %*% t(L) + diag(Psi))
round(Rm, 3)