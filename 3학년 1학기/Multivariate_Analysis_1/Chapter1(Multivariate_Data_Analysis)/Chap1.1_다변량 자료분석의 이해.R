setwd('/Users/imdohyeon/Desktop/3-1/Multivariate_Analysis_1')
getwd()
dir()

# CODE 1.1.1
Data1.1.1 = read.table("data/3subjects.txt", header=T)
X = Data1.1.1[, -1]

# Descriptive Statistics
summary(X)

# Covariance Matrix
cov(X)

# Correlation Matrix
cor(X)

# Multiple Scatter plot
plot(X)

# Boxplot of 3 Subjects
boxplot(X, xlab="3 Subjects", ylab="Exam Marks")

# Stem-and-leaf plot
stem(X[,1])
stem(X[,2])
stem(X[,3])

# CODE 1.1.2
Data1.1.1 = read.table("data/3subjects.txt", header=T)
X = Data1.1.1[,-1]

# install.packages("rgl")
library(rgl)

# Observations in Variables Space
lim = c(0, 100)
plot3d(X[,1], X[,2], X[,3], xlim=lim, ylim=lim, zlim=lim,
       xlab="Mechanics", ylab="Algebra", zlab="Statistics")
text3d(X[,1], X[,2], X[,3], rownames(X))

# Variables in Observations Space
plot3d(X[,1], X[,2], X[,3], xlim=lim, ylim=lim, zlim=lim,
       xlab="Student1", ylab="Student2", zlab="Student3")
text3d(X[1,], X[2,], X[,3], colnames(X))