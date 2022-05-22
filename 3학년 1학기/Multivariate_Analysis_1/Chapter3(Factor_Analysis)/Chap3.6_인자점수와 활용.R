### CODE 3.6.1
### air pollution data
Data2.8.2<-read.table("airpollution.txt", header=T)
X=Data2.8.2
rownames<-rownames(X)
p=ncol(X) 
n=nrow(X)
boxplot(X)
Z<-scale(X, scale=T)

# Covariance Matrix S(or Correlation Matix R)
R=round(cor(X),3)
R

##### PCFA using the principal( ) #######
library(psych)
pcfa<-principal(Z, nfactors=3, rotate="varimax")
#R(상관행렬)과 Z(표준화자료행렬) 둘다 사용 가능하지만, pcfa$scores를 이용해야하기에 Z를 사용.
pcfa
round(pcfa$values, 3)
gof=pcfa$values/p*100 # Goodness-of fit
round(gof, 3)

# Residual Matrix
L=pcfa$loading[, 1:3]
round(L, 3)
Psi=pcfa$uniquenesses
round(Psi, 3)
Rm = R-(L%*%t(L) + diag(Psi))
round(Rm, 3)

# Plot of PC Factor Loadings 
par(mfrow=c(2,2))
lim<-range(pretty(L))
plot(L[,1], L[,2],main="(a) PC Factor Loadings : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(L[,1], L[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 2], col=2, code=2, length=0.1)

plot(L[,1], L[,3],main="(b) PC Factor Loadings : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(L[,1], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,1], L[, 3], col=2, code=2, length=0.1)

plot(L[,2], L[,3],main="(c) PC Factor Loadings : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(L[,2], L[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, L[,2], L[, 3], col=2, code=2, length=0.1)


# Factor Scores : Regression Method
fpc=pcfa$scores
round(fpc, 3)

# Plot of Factor Scores : PFA 
par(mfrow=c(2,2))
par(pty="s")
lim<-range(pretty(fpc))
plot(fpc[,1], fpc[,2],main="(a) Factor Scores : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(fpc[,1], fpc[,2], labels=rownames(fpc), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(fpc[,1], fpc[,3],main="(b) Factor Scores : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(fpc[,1], fpc[,3], labels=rownames(fpc), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(fpc[,2], fpc[,3],main="(c) Factor Scores : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(fpc[,2], fpc[,3], labels=rownames(fpc), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

### Using Biplot()
par(mfrow=c(2,2))
biplot(pcfa$scores[,1:2], pcfa$loadings[,1:2], main="(a) Factor Scores : f1 and f2",  xlab="f1", ylab="f2")
abline(v=0, h=0)
biplot(pcfa$scores[,1:3], pcfa$loadings[,1:3], main="(b) Factor Scores : f1 and f3",  xlab="f1", ylab="f3")
abline(v=0, h=0)
biplot(pcfa$scores[,2:3], pcfa$loadings[,1:3], main="(c) Factor Scores : f2 and f3",  xlab="f2", ylab="f3")
abline(v=0, h=0)


##### MLFA using the factanal( ) ####
library(psych)
mlfa<-factanal(Z, factors = 3, rotation="varimax", score="regression")
mlfa

# Residual Matrix
Lm=mlfa$loading[, 1:3]
round(L, 3)
Psi=mlfa$uniquenesses
Rm = R-(Lm%*%t(Lm) + diag(Psi))
round(Rm, 3)

# ML Factor Loadings Plot
par(mfrow=c(2,2))
lim<-range(pretty(L))
plot(Lm[,1], Lm[,2],main="(a) ML Factor Loadings : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(Lm[,1], Lm[, 2], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, Lm[,1], Lm[, 2], col=2, code=2, length=0.1)

plot(Lm[,1], Lm[,3],main="(b) ML Factor Loadings : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(Lm[,1], Lm[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, Lm[,1], Lm[, 3], col=2, code=2, length=0.1)

plot(Lm[,2], Lm[,3],main="(c) ML Factor Loadings : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(Lm[,2], Lm[, 3], labels=rownames(L), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
arrows(0,0, Lm[,2], Lm[, 3], col=2, code=2, length=0.1)

# Factor Scores : Regression Method
fml=mlfa$scores
round(fml, 3)

# Plot of Factor Scores : MLFA
par(mfrow=c(2,2))
par(pty="s")
lim<-range(pretty(fml))
plot(fml[,1], fml[,2],main=" (a) Factor Scores : f1 and f2",  xlab="f1", ylab="f2",
     xlim=lim, ylim=lim)
text(fml[,1], fml[,2], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(fml[,1], fml[,3],main=" (b) Factor Scores : f1 and f3",  xlab="f1", ylab="f3",
     xlim=lim, ylim=lim)
text(fml[,1], fml[,3], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

plot(fml[,2], fml[,3],main="(c) Factor Scores : f2 and f3",  xlab="f2", ylab="f3",
     xlim=lim, ylim=lim)
text(fml[,2], fml[,3], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)

### Using Biplot()
par(mfrow=c(2,2))
biplot(mlfa$scores[,1:2], mlfa$loadings[,1:2], main="(a) Factor Scores : f1 and f2",  xlab="f1", ylab="f2")
abline(v=0, h=0)
biplot(mlfa$scores[,1:3], mlfa$loadings[,1:3], main="(b) Factor Scores : f1 and f3",  xlab="f1", ylab="f3")
abline(v=0, h=0)
biplot(mlfa$scores[,2:3], mlfa$loadings[,1:3], main="(c) Factor Scores : f2 and f3",  xlab="f2", ylab="f3")
abline(v=0, h=0)

# Plot of Factor Scores : Pairs(MLFA, PCFA)
par(pty="s")
par(mfrow=c(2,2))
plot(fml[,1], fpc[,1],main="(a) Factor Scores : ml f1 and pc f1",  xlab="ml f1", ylab="pc f1",
     xlim=lim, ylim=lim)
text(fml[,1], fpc[,1], labels=rownames(fml), cex=0.8, col="blue", pos=1)

abline(v=0, h=0)

plot(fml[,2], fpc[,2],main="(b) Factor Scores : ml f2 and pc f2",  xlab="ml f2", ylab="pc f2",
     xlim=lim, ylim=lim)
text(fml[,2], fpc[,2], labels=rownames(fml), cex=0.8, col="blue", pos=1)

abline(v=0, h=0)

plot(fml[,3], fpc[,3],main="(c) Factor Scores : ml f3 and pc f3",  xlab="ml f3", ylab="pc f3",
     xlim=lim, ylim=lim)
text(fml[,3], fpc[,3], labels=rownames(fml), cex=0.8, col="blue", pos=1)

abline(v=0, h=0)

plot(-fml[,3], fpc[,3],main="(d) Factor Scores : (-)ml f3 and pc f3",  xlab="(-)ml f3", ylab="pc f3",
     xlim=lim, ylim=lim)
text(-fml[,3], fpc[,3], labels=rownames(fml), cex=0.8, col="blue", pos=1)
abline(v=0, h=0)
