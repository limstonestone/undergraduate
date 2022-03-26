# The Z-test
## 현실 데이터에서는 사용하기 힘든 test (모표준편차를 모르기때문에)
data(golub, package="multtest")
golubFactor = factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
x = golub[2058, golubFactor=="ALL"]
n = length(x)
sigma = 0.25 ; mu0 = 0 # 현실에서 알기는 거의 불가능

z.value = sqrt(n)*(mean(x)-mu0)/sigma
2*pnorm(-abs(z.value)) # Significance level 0.05 보다 훨씬 크기 때문에 Not reject H0

## plotting
f = function(x) dnorm(x, 0, 1)
alpha = 0.05
qz = qnorm(1-alpha/2)
x1 = seq(-4, -qz, 0.01) ; y1 = dnorm(x1, 0, 1)
x2 = seq(-qz, qz, 0.01) ; y2 = dnorm(x2, 0, 1)
x3 = seq(qz, 4, 0.01) ; y3 = dnorm(x3, 0, 1)

plot(f, -4, 4, cex.lab=1.5, xlab="x", ylab="f(x)", 
     main="Normal probability density function f(x)")

polygon(c(-4, x1, -qz), c(0, y1, 0), col='red') # 그래프 색칠(적분처럼)
polygon(c(-qz, x2, qz), c(0, y2, 0), col="lightblue")
polygon(c(qz, x3, 4), c(0, y3, 0), col="red")

arrows(-3, 0.15, -3, 0.03)
text(-3, 0.23, "Rejection")
text(-3, 0.20, "Region")
text(-3, 0.17, expression(alpha/2))

arrows(3, 0.15, 3, 0.03)
text(3, 0.23, "Rejection")
text(3, 0.20, "Region")
text(3, 0.17, expression(alpha/2))

text(0, 0.23, "Acceptance")
text(0, 0.20, "Region")

## confidence interval
mean(x) + qnorm(0.025) * sigma/sqrt(n)
mean(x) + qnorm(0.975) * sigma/sqrt(n)
mean(x) + c(-1, 1) * qnorm(0.975) * sigma/sqrt(n) # 95% confidence interval

# One Sample T-test
## Z-test 는 현실적으로 모집단의 표준편차를 모르기 때문에 적용 불가능!
## 대체로 표본표준편차를 사용하는 T-test 를 사용한다.
f = function(x) dt(x, 5) # 자유도 5인 student-t distribution
alpha = 0.05
qt5 = qt(1-alpha/2, 5)

x1 <- seq(-4, -qt5, 0.01)
y1 <- f(x1)
x2 <- seq(-qt5, qt5, 0.01)
y2 <- f(x2)
x3 <- seq(qt5, 4, 0.01)
y3 <- f(x3)

plot(f, -4, 4, xlab="x", ylab="f(x)", main="T-distribution probability density function f(x)")
polygon(c(-4, x1, -qt5), c(0, y1 , 0), col="red")
polygon(c(-qt5, x2, qt5), c(0, y2, 0), col="lightblue")
polygon(c(qt5, x3, 4), c(0, y3, 0), col="red")

arrows(-3, 0.15, -3, 0.03)
text(-3, 0.23, "Rejection")
text(-3, 0.20, "Region")
text(-3, 0.17, expression(alpha/2))

arrows(3, 0.15, 3, 0.03)
text(3, 0.23, "Rejection")
text(3, 0.20, "Region")
text(3, 0.17, expression(alpha/2))

text(0, 0.23, "Acceptance")
text(0, 0.20, "Region")

mtext(expression(t[0.025]), side=1, at=-qt5, col="red")
mtext(expression(t[0.975]), side=1, at=qt5, col="red")

## Example
x = golub[2058, golubFactor="ALL"]
mu0 = 0
n = 27
t.value = sqrt(n)*(mean(x) - mu0)/sd(x)

t.value
2*pt(-abs(t.value), 26) # not reject H0

alpha = 0.05
mean(x) + c(qt(alpha/2, n-1), qt(1-alpha/2, n-1)) * sd(x)/sqrt(n)

## built-in function
t.test(x, mu=0)
t.test(x, mu=0, alternative="greater")
t.test(x, mu=0, alternative="less")

## Example
ccnd3 = grep("CCND3", golub.gnames[, 2], ignore.case=TRUE)
ccnd3

par(mfrow=c(1,2))
stripchart(golub[ccnd3, ] ~ golubFactor, method="jitter", cex.lab=1.5,
           ylab="", vertical=TRUE, col=c("red", "darkgreen"), xlab="Leukemia subtype")
boxplot(golub[ccnd3,] ~ golubFactor, cex.lab=1.5, main=NULL,
        xlab="Leukemia subtype", col=c("purple","green"),
        ylab="CCND3 (Cyclin D3) Expression")

ALL = golubFactor=="ALL"
t.test(golub[ccnd3, ALL], mu=0, alternative="greater") # perfectly reject H0
t.test(golub[ccnd3, !ALL], mu=0, alternative="greater")

t.test(golub[ccnd3, ALL], mu=0, alternative="less")
t.test(golub[ccnd3, !ALL], mu=0, alternative="less")

# Two Sample T-test with Unequal Variances
## 어떠한 요인이 통계적으로 차이를 보이냐? 에 유리한 검정
## 항상 등분산성 검정이 우선되어야 함

## Welch two-sample t-test (var.equal = FALSE)
t.test(golub[ccnd3,] ~ golubFactor, var.equal=FALSE)
t.test(golub[ccnd3,ALL], golub[ccnd3, !ALL], var.equal=FALSE)

t.test(golub[ccnd3, ALL], golub[ccnd3, !ALL], var.equal=FALSE,
       alternative="greater") # mu_x > mu_y
t.test(golub[ccnd3, ALL], golub[ccnd3, !ALL], var.equal=FALSE,
       alternative="less")
       
## Pooled two-sample t-test (var.equal = TRUE)
## 등분산 검정 우선되어야 함
t.test(golub[ccnd3,] ~ golubFactor, var.equal=TRUE)

# Permutation Test
## 현실 데이터는 대개 분포가 알려져 있지 않고 sample size 가 제한되어 있기 때문에 사용
## 단점으로는 시간이 많이 걸림
## resampling-based test
golubFactor
sample(golubFactor)
sample(as.numeric(golubFactor))

y = as.numeric(golubFactor)
K = 1000
mat.y = matrix(y, length(y), K)

per.y = apply(mat.y, 2, sample)
t.test(golub[ccnd3, ] ~ per.y[,1], var.equal=FALSE)
t.test(golub[ccnd3, ] ~ per.y[,1], var.equal=FALSE)$stat  # to get test-statistics

fun = function(t) t.test(golub[ccnd3,]~t, var.equal=FALSE)$stat
p.stat = apply(per.y, 2, fun)
hist(p.stat, nclass=50, col="orange") # looks like Normal Dist

tobs = t.test(golub[ccnd3,]~golubFactor, var.equal=FALSE)$stat
mean(abs(p.stat) > abs(tobs)) # p-value
(sum(abs(p.stat) > abs(tobs)) + 1)/(K+1)  # significantly reject H0

gdf5 = grep("Gdf5", golub.gnames[,2], ignore.case=TRUE)
fun2 = function(t) t.test(golub[gdf5,]~t, var.equal=FALSE)$stat
p.stat2 = apply(per.y, 2, fun2)
hist(p.stat2, nclass=50, col="orange")

tobs2 = t.test(golub[gdf5,]~golubFactor, var.equal=FALSE)$stat
abline(v=c(-tobs2, tobs2), col="red", lty=2, lwd=2)

mean(abs(p.stat2) > abs(tobs2))
(sum(abs(p.stat2) > abs(tobs2))+1)/(K+1)  # not reject H0

# Test for Equal Variances
## t-test 의 가정에 두 모집단의 등분산성을 가정
## 등분산성을 어떻게 검정?
var.test(golub[ccnd3, ] ~ golubFactor)  # not reject H0
var.test(golub[gdf5, ] ~ golubFactor)   # "pooled t-test" 사용 가능
## F-test는 정규성에 민감하므로 대체로 다음 test도 많이 사용
## The Bartlett test
## The Fligner-Killeen (median) test
## The Levenee test
bartlett.test(golub[ccnd3, ] ~ golubFactor)
bartlett.test(golub[gdf5, ] ~ golubFactor)

fligner.test(golub[ccnd3,], golubFactor)
fligner.test(golub[gdf5,], golubFactor)

install.packages("car")
library(car)

leveneTest(golub[ccnd3,], golubFactor)
leveneTest(golub[gdf5,], golubFactor)

# Binomial Test
## descrete distribution 이므로 등호 주의
## Example : microRNA of length 22 contains 18 purines
## H0 : p=0.7
sum(dbinom(18:22, 22, 0.7)) # P(X>=18)
1 - pbinom(17, 22, 0.7)     # 1 - P(X<17)

binom.test(18, 22, p=0.7, alternative="greater", conf.level=0.95)

# Chi-squared Test
## 하나 이상의 특정 확률을 따르냐에 대한 검정
## Example : Let the probability H0 : (1/4, 1/4, 1/4, 1/4)
install.packages("ape")
library(ape)

zyxinfreq = table(read.GenBank(c("X94991.1"), as.character=TRUE))
zyxinfreq
chisq.test(zyxinfreq) # significantly reject H0 -> probabilities are very different

f = function(x) dchisq(x, 3)
plot(f, 0, 25, cex.lab=1.5, xlab="q", ylab="f(q)",
     main="Chi-squared probability density function f(q)")

alpha = 0.05
x1 = seq(0, qchisq(1-alpha, 3), 0.01)
x2 = seq(qchisq(1-alpha, 3), 20, 0.01)

polygon(c(0,x1,qchisq(1-alpha,3)),c(0,f(x1),0),col="lightblue")
polygon(c(qchisq(1-alpha,3),x2,20), c(0,f(x2),0), col="red")

arrows(10,0.045,10,0.02)
text(10,0.06,"Rejection")
text(10,0.05,"Region")
text(3,0.04,"Acceptance")
text(3,0.03,"Region")
mtext(expression(chi[3]^2),side=1,at=qchisq(1-alpha,3))

pi = c(0.75, 0.25)
x = c(5474, 1850)
chisq.test(x, p=pi) # not reject H0 => H0 : (pi1, pi2) = (0.75, 0.25) is True