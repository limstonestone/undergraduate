### CODE 1.8.1
# iris data chi-square plot 구현
data(iris)
setosa = iris[1:50, 1:4]		# Iris data only for setosa
versicolor = iris[51:100, 1:4]	# Iris data only for vesicolor
virginica = iris[101:150, 1:4]	# Iris data only for virginica

# Chi-square Plot for Checking MVN
x = setosa
n = dim(x)[[1]]	# num of data
p = dim(x)[[2]]	# num of variables
S = cov(x)
xbar = colMeans(x)
m = mahalanobis(x, xbar, S)	# mahalanobis() : mahalanobis distance 를 반환하는 method
m = sort(m)
id = seq(1, n)
pt = (id - 0.5)/n
q = qchisq(pt, p)	# chi-square 분위수 계산
plot(q, m, pch="*", xlab="Quantile", ylab="Ordered Squared Distance")
abline(0, 1)

# Correlation Coefficient Test for Normality
rq = cor(cbind(q, m))[1, 2]	# mahalanobis distance 와 chi-square 분위수 간의 상관계수, 1에 가까울 수록 정규성 만족
rq

### CODE 1.8.2
# 왜도(skewness)와 첨도(kurtosis)를 활용한 정규성 검정
# install.packages("MVN")
library(MVN)
iris
# MVN tests based on the Skewness and Kurtosis Statistics
par(mfrow=c(1,3))
setosa = iris[1:50, 1:4] # Iris data only for setosa and four variables
versicolor = iris[51:100, 1:4] # Iris data only for versicolor and four variables
virginica = iris[101:150, 1:4] # Iris data only for virginica and four variables

result_setosa = mvn(setosa, multivariatePlot="qq")		# q-q plot 상으로 정규성 만족
result_versicolor = mvn(versicolor, multivariatePlot="qq")
result_virginica = mvn(virginica, multivariatePlot="qq")

result_setosa	# p-value < 0.05 이면 정규성 만족 X
result_versicolor
result_virginica