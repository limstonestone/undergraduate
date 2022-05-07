### CODE 2.6.1
# PC Biplots for 5 Subjects Exam
X = read.table("5subjects.txt", header=T)
n = nrow(X)
rownames(X)
colnames(X)
X = X[, -1]
joinnames = c(rownames(X), colnames(X))

Y = scale(X, scale=F)

# Biplot based on the Singular Value Decomposition
svd.Y = svd(Y)
U = svd.Y$u
V = svd.Y$v
D = diag(svd.Y$d)
G = (sqrt(n-1)*U)[, 1:2]  # 표본
H = (sqrt(1/(n-1))*V %*% D)[, 1:2]  # 변수
C = rbind(G, H)
rownames(G) = rownames(X)
rownames(H) = colnames(X)
rownames(C) = joinnames

# Goodness-of-fit
eig = (svd.Y$d)^2
per = eig/sum(eig)*100
gof = sum(per[1:2])

# Biplots
par(mfrow=c(2,2))
par(pty="s")
lim1 = range(pretty(H))

plot(H[,1],H[,2],xlab="1st PC",ylab="2nd PC", main="(a) 5 Subjects",
     xlim=lim1,ylim=lim1,pch=15,col=2, type="n")
abline(v=0,h=0)
text(H[,1], H[,2],colnames(X),cex=0.8,col=1,pos=3)
arrows(0,0,H[,1],H[,2],col=2,code=2, length=0.1)

## 화살표가 이루는 각도 : 상관관계
## 화살표가 이루는 길이 : 분산

lim2 <- range(pretty(G))
plot(G[,1],G[,2],xlab="1st PC",ylab="2nd PC", main="(b) 88 Students",
     xlim=lim2,ylim=lim2,pch=16, type="n")
abline(v=0,h=0)
text(G[,1],G[,2],rownames(X),cex=0.8,pos=3)

lim3 <- range(pretty(C))
plot(C[,1],C[,2],xlab="1st PC",ylab="2nd PC",  main="(c) 5 Subjects and 88 Students",
     xlim=lim3,ylim=lim3,pch=16,  type="n")
abline(v=0,h=0)
text(C[,1],C[,2],joinnames,cex=0.8,pos=3)
arrows(0,0,C[89:93,1],C[89:93,2],col=2,code=2, length=0.1)

biplot(G,H, xlab="1st PC",ylab="2nd PC", main="(d) biplot function",
       xlim=lim2,ylim=lim2,cex=0.8,pch=16)
abline(v=0,h=0)

### CODE 2.6.2
# PC Biplot for KLPGA
# Data Matrix X
Data1.3.2 = read.table("klpga.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.2
n = nrow(X)
rownames(X)
colnames(X)

Y = scale(X, scale=T)

# Biplot based on the Singular Value Decomposition
svd.Y = svd(Y)
U = svd.Y$u
V = svd.Y$v
D = diag(svd.Y$d)
G = (sqrt(n-1)*U)[, 1:2]
H = (sqrt(1/(n-1))*V %*% D)[, 1:2]
rownames(G) = rownames(X)
rownames(H) = colnames(X)

# Goodness-of-fit
eig = (svd.Y$d)^2
per = eig/sum(eig)*100
gof = sum(per[1:2])
round(per, 2)
round(gof, 2)

# PC Biplot
par(mfrow=c(1,1))
lim = range(pretty(G))
biplot(G,H, xlab="1st PC(71.83%)",ylab="2nd PC(18.64%)", main="Biplot for KLPGA Data ",
       xlim=lim,ylim=lim,cex=0.8,pch=16)
abline(v=0,h=0)
