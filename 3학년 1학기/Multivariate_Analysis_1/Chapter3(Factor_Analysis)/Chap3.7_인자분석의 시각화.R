# CODE 3.7,1
### air pollution data
Data2.8.2<-read.table("airpollution.txt", header=T)
X=Data2.8.2
rownames<-rownames(X)
p=ncol(X) 
n=nrow(X)
Z<-scale(X, scale=T)

svd.Z <- svd(Z) 
U <- svd.Z$u    
V <- svd.Z$v 
D <- diag(svd.Z$d)
F <- (sqrt(n-1)*U)[,1:2]  # Factor Scores Matrix : F
L <- (sqrt(1/(n-1))*V%*%D)[,1:2] # Factor Loadings Matrix : Lambda
C<- rbind(F, L)
rownames(F)<-rownames(X)
rownames(L)<-colnames(X)

# Godness-of-fit
eig <- (svd.Z$d)^2 
per <- eig/sum(eig)*100
gof <- sum(per[1:2])
per
gof


# Biplot: Joint Plot of Factor Loadings and Scores
par(mfrow=c(1,2))
par(pty="s")
lim1 <- range(pretty(L))
lim2 <- range(pretty(F))
biplot(F,L, xlab="f1",ylab="f2", main=" (a) Unrotated Biplot",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

# Varimax Rotated Biplot: Joint Plot of Rotated Factor Loadings and Scores

varimax<-varimax(L)
Lt = varimax$loadings 
T=varimax$rotmat
T
Ft= F%*%T
biplot(Ft,Lt, xlab="f1",ylab="f2", main="(b) Varimax Rotated Biplot",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

##### Use principal() and factonal()!
##### You should use Data Matrix Z or H to get factor scores.
##### Biplot (Factor scores + Factor Loadings)
