### CODE 3.5.1
# MLFA : None and Varimax Rotation for KLPGA
# Data Matrix X
Data1.3.2 = read.table("klpga.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.2
rownames = rownames(X)
p = ncol(X)

# Covariance Matrix S (or Correlation Matrix R)
R = round(cor(X), 3)
R

# ML Estimation using the factanal(): None
library(psych)
mlfa = factanal(covmat=R, factors=2, rotation="none")
mlfa

# Residual Matrix
L = mlfa$loading[, 1:2]
Psi = mlfa$uniquenesses
Rm = R - (L %*% t(L) + diag(Psi))
round(Rm, 3)

# Factor Loadings Plot : None
par(mfrow=c(1, 2))
par(family="AppleGothic")

lim<-range(pretty(L))
plot(L[,1], L[,2],main="Plot of Factor Loadings : none ",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(L[,1], L[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1)

# ML Estimation using the factanal( ): Varimax rotation
mlfa<-factanal(Z, factors = 2, rotation="varimax", scores="regression")
mlfa

# Residual Matrix
L=mlfa$loading[, 1:2]
L
Psi=mlfa$uniquenesses
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm, 3)

# Factor Loadings Plot : Varimax
lim<-range(pretty(L))
plot(L[,1], L[,2],main="Plot of Factor Loadings : Varimax ",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(L[,1], L[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1)

# biplot
par(mfrow=c(1,1))
biplot(mlfa$scores, mlfa$loadings, main="Plot of Factor Loadings : Varimax", xlab="f1", ylab="f2")
abline(v=0, h=0)
