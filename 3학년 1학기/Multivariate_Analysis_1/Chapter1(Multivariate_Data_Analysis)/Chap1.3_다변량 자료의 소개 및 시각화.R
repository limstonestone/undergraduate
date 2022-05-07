# CODE 1.3.1
Data1.3.1 = read.table('rmtda/5subjects.txt', header=T)
X = Data1.3.1[, -1]

# Multiple Scatter Plot
plot(X)

# Box Plot
boxplot(X)

# CODE 1.3.2
rm(list=ls())
Data1.3.2 = read.table("rmtda/klpga.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.2[, -1]

# Descriptive Statistics
summary(X)

# Covariance Matrix
cov(X)

# correlation Matrix
cor(X)

# Multiple Scatter plot
plot(X)

# Boxplot of 3 Subjects
boxplot(X)

# CODE 1.3.3
rm(list=ls())
Data1.3.3 = read.table("rmtda/protein.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.3[,-(1:2)]
par(family="AppleGothic") # MacOS kor error

# Multiple Scatter plot
plot(X)

# Barplot of 25 Countries
X<-as.matrix(Data1.3.3[, -(1:2)])
rownames(X)<-Data1.3.3[,2]
X<-t(X)
par(las=2)
par(mar=c(8,8,4,2)) 
barplot(X, legend=rownames(X), horiz=TRUE)


# Star Plot
X<-scale(as.matrix(Data1.3.3[, -(1:2)]))
rownames(X)<-Data1.3.3[, 2]
stars(X,key.loc=c(0,2), full = FALSE)

# CODE 1.3.4
rm(list=ls())
Data1.3.4 = read.table("rmtda/irisflower.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.4[, -1]

# Box Plot
par(mfrow=c(2,2))
boxplot(²É¹ÞÄ§±æÀÌ~group, data=X, xlab="º×²É Á¾·ù", ylab="X1 : ²É¹ÞÄ§ ±æÀÌ")
boxplot(²É¹ÞÄ§Æø~group, data=X, xlab="º×²É Á¾·ù", ylab="X2 : ²É¹ÞÄ§ Æø")
boxplot(²ÉÀÙ±æÀÌ~group, data=X, xlab="º×²É Á¾·ù", ylab="X3 : ²ÉÀÙ ±æÀÌ")
boxplot(²ÉÀÙÆø~group, data=X, xlab="º×²É Á¾·ù", ylab="X4 : ²ÉÀÙ Æø")

# Multiple Scatter Plot
plot(X[, 1:4], pch=unclass(X[, 5]), col=1:3)

# CODE 1.3.5
rm(list=ls())
Data1.3.5 = read.table("rmtda/economicview.txt", header=T, fileEncoding="euc-kr")
X = Data1.3.5[, -1]

# Multiple Scatter Plot
plot(X)

# Star Plot
X = scale(as.matrix(X[, c(1,2,3,5)]))
rownames(X) = Data1.3.5[, 1]
stars(X, key.loc=c(8, 2), full=FALSE)

# Parallel Coordinate Plot
# install.packages("gclus")
# install.packages("dplyr")
library(gclus)
library(dplyr)
parcoordlabel<-function(x, col = 1, lty = 1,  var.label=F,...) 
{
  rx <-lapply(X, range, na.rm = TRUE)
  matplot(1L:ncol(x), t(x), type = "l", col = col, lty = 1:nrow(X), lwd=1.5,
          xlab = "", ylab = "", axes = FALSE, 
          ylim=c(-4, 4), xlim=c(1, nrow(X)), ...)
  axis(1, at = 1L:ncol(x), labels = colnames(x))
  legend("top", horiz=F, legend=colnames(X), lty=1:nrow(X), 
         col=1, cex=0.8, lwd=1.5)
  for (i in 1L:ncol(x))
    invisible()
}
# available only on Windows
# windows(height=5, width=12)
parcoordlabel(t(X))

# CODE 1.3.6
rm(list=ls())
Data1.3.6 = read.table("rmtda/sucidefreq.txt")
Data1.3.6
freq = as.matrix(Data1.3.6)

sex = c("Male", "Female")
age = c("10-20", "25-35", "40-50", "55-65", "70-90")
method = c("Poison", "Gas", "Hang", "Drown", "Gun", "Jump")
data = expand.grid(Sex=sex, Age=age, Method=method)

data = cbind(data, freq)

# Make it into a 3-way table
table = xtabs(freq~Sex+Age+Method, data=data)

# Print table
ftable(table)

# Chi-square test of independence
summary(table)

# Mosaic Plot
mosaicplot(table, main="Mosaic Plot for Sucide Rates")

# CODE 1.3.7
dev.off()
rm(list=ls())
# install.packages("shapes")
library(shapes)
digit3 = read.in("rmtda/digit3.txt", 13, 2)
digit3_two = digit3[, , 5:6]
digit3_two
bookdigit3 = bookstein2d(digit3_two)
bookdigit3
plotshapes(digit3_two, symbol=1, joinline=c(1:13))
plotshapes(bookdigit3$bshpv, color=1:6, symbol=1, joinline=c(1:13))
plotshapes(bookdigit3$mshape, color=1:6, symbol=1, joinline=c(1:13))