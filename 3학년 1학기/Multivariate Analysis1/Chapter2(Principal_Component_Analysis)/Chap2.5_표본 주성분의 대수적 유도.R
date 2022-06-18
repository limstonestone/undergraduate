### CODE 2.5.1
# PCA Steps for KLPGA

# [Step 1] Data Matrix X
Data1.3.2 = read.table("klpga.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.2
rownames=  rownames(X)

# [Step 2] Covariance Matrix S(or Correlaiton Matrix R)
R = round(cor(X), 3)
R

# [Step 3] Spectral Decomposition
eigen.R = eigen(R)
round(eigen.R$values, 2)  # Eigenvalues
V = round(eigen.R$vectors, 2) # Eigenvectors

vec = eigen.R$vectors
val = diag(eigen.R$values)
vec %*% val %*% t(vec)  # Spectral Decomposition

# [Step 4] Choice of Eigenvalues and Eigenvectors
gof = eigen.R$values/sum(eigen.R$values)*100 # Goodness-of fit
round(gof, 2)
par(mfrow=c(1,1))
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")  # Find elbow point! (2~3)

# [Step 5] PCs : linear combination of original variables
V2 = V[, 1:2]
V2

# [Step 6] PCS, PCs Scores and New Data Matrix P
Z = scale(X, scale=T) # Stnadardized Data Matrix
head(Z)
P = Z %*% V2 # PCs Scores
head(round(P, 3))

# [Step 7] Plot of PCs Scores
plot(P[,1], P[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2], labels=rownames, cex=0.8, col="blue", pos=3)
abline(v=0, h=0)

# Correlation between PCs and variables
D = diag(sqrt(eigen.R$values[1:2]))
corr = V2 %*% D
corr
colnames(corr) = c("gamma1", "gamma2")
rownames(corr) = colnames(X)
corr

### CODE 2.5.2
# PCA Steps based on the SD for Skull Data

# [Step 1] Data Matrix X(standized data)
Data1.3.2 = read.table("skull.txt", header=T)
Z = as.matrix(Data1.3.2)
rownames = rownames(Z)
colnames = colnames(Z)
n = nrow(Z)

# [Step 2] Covariance Matrix S(or Correlation Matrix R)
R = (t(Z) %*% Z/(n-1))
R # =cov(Z)

# [Step 3] Spectral Decomposition
eigen.R = eigen(R)
round(eigen.R$values, 2)  # Eigenvalues
V = eigen.R$vectors # Eigenvectors
round(V, 2)

# [Step 4] Choice of Eigenvalues and Eigenvectors
gof = eigen.R$values/sum(eigen.R$values)*100  # Goodness-of fit
round(gof, 2)
plot(eigen.R$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue") 
## reasonable num of components = 3 (elbow point=4, and the values behind 4 are too small)

# [Step 5] PCs : linear combination of original variables
V3 = V[, 1:3]
round(t(V3), 2)

# [Step 6] PCS, PCs Scores and New Data Matrix P
Z # Standardized Data Matrix
P = Z %*% V3 # PCs Scores
head(round(P, 3))

# [Step 7] Plot of PCs Scores
par(mfrow=c(2,2))
plot(P[,1], P[, 2], main="(a) Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P[,1], P[, 3], main="(b) Plot of PCs Scores", xlab="1st PC", ylab="3rd PC")
text(P[,1], P[, 3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P[,2], P[, 3], main="(c) Plot of PCs Scores", xlab="2nd PC", ylab="3rd PC")
text(P[,2], P[, 3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

# Classifying Man and Woman using different pixel color
substr_Right = function(x, n){
  substr(x, nchar(x) - n+1, nchar(x)) # 문자열 일부 추출
}

levels = substr_Right(rownames, 1)
levels = as.factor(levels)
plot(P[,1], P[, 2], main="(a) Plot of PCs Scores", xlab="1st PC", ylab="2nd PC", col=levels, pch=16)
text(P[,1], P[, 2], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

### CODE 2.5.3
# PCA Steps based on the SVD for Skull Data

# [Step 1] Data Matrix X
Data1.3.2. = read.table("skull.txt", header=T)
Z = as.matrix(Data1.3.2)
rownames = rownames(Z)
colnames = colnames(Z)
n = nrow(Z)

# [Step 2] Singular Values Decompoisiton
svd.Z = svd(Z)
U = svd.Z$u # Right singular vectors
V = svd.Z$v # Left singhular vectors : Eigenvectors
round(V, 2)
D = diag(svd.Z$d)

# [Step 3] Choice of Singular Values and Eigenvectors
round(svd.Z$d, 2)
eigen = (svd.Z$d)^2
round(eigen/(n-1), 2)
gof = eigen/sum(eigen)*100  # Goodness-of-fit
round(gof, 2)

# [Step 5] PCs : linear combination of original variables
V3 = V[, 1:3]
V3
round(t(V3), 2)

# [Step 6] PCS, PCs Scores and New Data Matrix P
Z # Stnadardized Data Matrix
P = U %*% D # PCs Scores : P = Z %*% V3
round(P, 3)

# [Step 7] Plot of PCs Scores
plot(P[,1], P[, 2], main="(a) Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P[,1], P[, 3], main="(b) Plot of PCs Scores", xlab="1st PC", ylab="3rd PC")
text(P[,1], P[, 3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(P[,2], P[, 3], main="(c) Plot of PCs Scores", xlab="2nd PC", ylab="3rd PC")
text(P[,2], P[, 3], labels=rownames, cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

#Correlations bt PCs and variables
D=diag(svd.Z$d[1:3]/sqrt(n-1))
corr=V3%*%D
round(corr, 3)