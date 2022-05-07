### CODE 2.2.1
# 3subjects data의 PCA 수행단계

# [Step 1] Data Matrix X
Data1.1.1 = read.table("3subjects.txt", header=T)
X = Data1.1.1[-1]

# [Step 2] Covariance Matrix S(or Correlation Matrix R)
S = round(cov(X), 3)
S

# [Step 3] Spectral Decomposition
eigen.S = eigen(S)
round(eigen.S$values, 3)  # Eigenvalues
V = round(eigen.S$vectors, 3) # Eigenvectors
V

# [Step 4] Choice of Eigenvalues and Eigenvectors
gof = eigen.S$values/sum(eigen.S$values)*100  # Goodness-of fit
round(gof, 2)

# [Step 5] PCs : linear combination of original variables
V2 = V[,1:2]
V2

# [Step 6] PCS, PCs Scores and New Data Matrix P
Y = scale(X, scale=F) # Centered Data Matrix
Y
P = Y%*%V2 # PCs Scores
P
pretty(P)
# [Step 7] Plot of PCs Scores
par(pty="s")  # pty="s" : 가로, 세로 비율 똑같이
lim = range(pretty(P))  # pretty func computes a sequence of equally spaced round values
plot(P[, 1], P[, 2], main="Plot of PCs Scores", xlim=lim, ylim=lim,
     xlab="1st PC", ylab="2nd PC")
text(P[,1]+2, P[, 2], rownames(X), cex=0.8, col="blue", pos=3)

abline(v=0, h=0)

### CODE 2.2.2
# 5subjects data의 PCA 수행단계

# Steps for PCA
# [Step 1] Data Matrix X
X = read.table("5subjects.txt", header=T)
head(X)
X = X[, -1]
dim(X)

#[Step 2] Covariance Matrix S(or Correlation Matix R)
S=round(cov(X),3)
S

#[Step 3] Spectral Decomposition (SD)
eigen.S=eigen(S)
round(eigen.S$values, 3) # Eigenvalues
V=round(eigen.S$vectors, 3) # Eigenvectors
V

#[Step 4] Choice of Eigenvalues and Eigenvectors
gof=eigen.S$values/sum(eigen.S$values)*100 # Goodness-of fit
round(gof, 2)
plot(eigen.S$values, type="b", main="Scree Graph", xlab="Component Number", ylab="Eigenvalue")

#[Step 5] PCs : liner combination of original variables 
V2=V[,1:2]
V2

#[Step 6] PCS, PCs Scores and New Data Matrix P
Y=scale(X, scale=F) # Centred Data Matrix (Covariance<Centred>, Correlation<Standardized>)
P=Y%*%V2            # PCs Scores
head(P)
rownames(P) <- 1:88

#[Step 7] Plot of PCs Scores
plot(P[,1], P[, 2], main="Plot of PCs Scores", xlab="1st PC", ylab="2nd PC")
text(P[,1], P[, 2]+2, labels=rownames(P), cex=0.8, col="blue")
abline(v=0, h=0)

## Plot 에서 알 수 있듯이 PCA를 통한 그룹핑 또한 가능하다.



