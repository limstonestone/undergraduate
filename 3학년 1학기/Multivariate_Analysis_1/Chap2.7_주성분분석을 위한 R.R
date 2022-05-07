### CODE 2.7.1
# Easy way to use PCA -> princomp() or prcomp()
# Example for KLPGA
Data1.3.2 = read.table("klpga.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.2

# PCA based on the Sd using princomp()
pca.R = princomp(X, cor=T)  # cor=F -> covariance, cor=T -> correlation
summary(pca.R, loadings=T)  # 설명력, 주성분계수
round(pca.R$scores, 3)  # 주성분점수
par(mfrow=c(1,1))
screeplot(pca.R, type="lines")  # Scree Plot

# Principle component biplot(SD)
biplot(pca.R, scale=0, xlab="1st PC", ylab="2nd PC",
       main="PC Biplot for KLPGA Data")   
abline(v=0, h=0)

# PCA on the SVD using prcomp()
pcasvd.Z = prcomp(X, scale=T)
summary(pcasvd.Z) # explanation
round(pcasvd.Z$rotation, 3) # PC coefficient
pcasvd.Z$scale
screeplot(pcasvd.Z, type="lines")

# Principle component biplot (SVD)
biplot(pcasvd.Z, scale=0,  xlab="1st PC", ylab="2nd PC",
       main="PC Biplot for KLPGA Data")
abline(v=0, h=0)
