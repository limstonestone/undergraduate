# Least Square Estimation
data(golub, package="multtest")

zyxin = grep("Zyxin", golub.gnames[,2], ignore.case=TRUE)
cmyb = grep("c-myb", golub.gnames[,2], ignore.case=TRUE)
x = golub[zyxin, ]
y = golub[cmyb, ]

leastSquares = lm(y ~ x)
leastSquares$coef

plot(x, y, pch=19, xlab="Relative zyxin gene expression", ylab="Relative c-MYB gene expression", cex.lab=1.5, col="blue")
abline(leastSquares$coef, lwd=3, lty=2, col="red")

lmSummary = summary(leastSquares)
lmSummary

## Signif.codes : more stars represents more significance
## R^2 : indicates how well the data points fit the statistical model 
## (percentage of variance in the dependent variable that is explained by the model)
## adjusted R-sqaured : more predictors -> improve R-squared. adjusted R-sqaured can adjust this unreasonable issue.

# F Test Statistic
## F-value means "better than random"
## F-value is the ratio of the variance explained over the unexplained variance (residual)

f.value = (coef(lmSummary)[2, "t value"])^2 # In the simple case -> F = t^2
f.value
lmSummary$fstat

# Linear Model with a Factor
y = c(2, 3, 1, 2, 8, 7, 9, 8, 11, 12, 13, 12)
factor = gl(3, 4) # gl : Generate factors by specifying the pattern of their levels.
factor

model.matrix(y ~ factor - 1)  # model.matrix = design matrix X, -1 indicates model has no intercept

## estimation of the coefficients = estimates of the population group means from the sample data

summary(lm(y ~ factor -1))  # p-values are too small -> Reject H0 (mu_j = 0)

## linear model is useful for testing hypotheses about group means

# One-way Analysis of Variance
## ANOVA can perform for testing three or more population means are equal to each other
## SSW(sum of squares within) is some of the squared deviation of the measurements to their group mean
## SSB(sum of squares betwwen) is the sum of sqaures the deviances of the group mean w.r.t total mean
## f-value is defined by {SSB/(g-1)}/{SSW/(N-g)}
## if F-value > f => H0(mu_1 = mu_2 = mu_3 ,,, ) is rejected

y = c(2, 3, 1, 2, 8, 7, 9, 8, 11, 12, 13, 12)
factor = gl(3, 4)

groupMeans = as.numeric(tapply(y, factor, mean))  # tapply : perform function about factor
groupMeans
mean(y)

g = 3
n = 4
N = 12

ssb = 0
for (j in 1:g) {
  ssb = ssb + (groupMeans[j] - mean(y))^2
}
SSB = n * ssb
SSB

SSW = 0
for (j in 1:g) {
  SSW = SSW + sum((y[factor==j] - groupMeans[j])^2)
}

f.value = (SSB/(g-1)) / (SSW/(N-g))
f.value
pf(f.value, g-1, N-g, lower.tail=FALSE) # lower.tail : calculate only tail

g = lm(y ~ factor)
anova(g)
summary(aov(g))

## fator : SSB
## residuals : SSW

# Linear Model in ANOVA
set.seed(123)
y = c(rnorm(10, -1, 1), rnorm(10, 0, 1), rnorm(10, 1, 1))
y
factor = gl(3, 10)
factor

model.matrix(y ~ factor -1)
model.matrix(y ~ factor)  # Reference : factor1

g1 = lm(y ~ factor -1)
g2 = lm(y ~ factor)

summary(g1)$coef  # we can see mean of reference equals to estimates of intercept
summary(g2)$coef

anova(g1) # Wrong result (because of df=3)
anova(g2)

summary(g1)$sigma
summary(g2)$sigma
cbind(residuals(g1), residuals(g2))

cbind(fitted(g1), fitted(g2))
tapply(y, factor, mean)

factor = relevel(factor, ref=2) # reference level=2
model.matrix(y ~ factor)
g3 = lm(y ~ factor)

summary(g2)$coef
summary(g3)$coef
anova(g2)
anova(g3)

## anova function has same result regardless of reference level

# Contrast
## one-way ANOVA is not clear which of the means differ from the others.
## constructing contrast matrix can solve this problem

## using t-test example
## estimated intercept is the mean of Group1
## factor2 is the difference between Group2 and Group1
## factor3 is the difference between Group3 and Group1

factor = gl(3, 10)
summary(lm(y ~ factor))
anova(lm(y ~ factor))

# All Pairwise Test
## By above, the difference between Group2 and 3 is not tested
## pairwise.t.test() : all possible pairwise t-tests
pairwise.t.test(y, factor, p.adjust.method="bonferroni")
pairwise.t.test(y, factor, p.adjust.method="holm")

## A popular multiple comparison test for post-hoc ANOVA is Tukey's HSD test
TukeyHSD(aov(y ~ factor))
plot(TukeyHSD(aov(y ~ factor)))

## Example : No difference between the means
set.seed(1234)
y = rnorm(30, 1.9, 1)
y

factor = gl(3, 10)
g = lm(y ~ factor)
anova(g)

pairwise.t.test(y, factor, p.adjust.method="bonferroni")
TukeyHSD(aov(g))
plot(TukeyHSD(aov(g)))

# Example of One-way ANOVA
BiocManager::install("ALL")
library(ALL)
data(ALL)
?ALL

dim(ALL)
ALL[1:10, 1:5]
str(ALL)
exprs(ALL)[1:10, 1:5]
dim(exprs(ALL))

table(ALL$BT)
B1B2B3 = ALL$BT %in% c("B1", "B2", "B3")
ex = exprs(ALL)
y = as.numeric(ex[row.names(ex)=="1866_g_at", B1B2B3])
factor = factor(ALL$BT[B1B2B3], labels=c("B1", "B2", "B3"))

col = c("orange", "darkgreen", "blue")
xlab = "B-cell ALL stage"
ylab = "SKT-like oncogene expression"
par(mfrow=c(1, 2))
stripchart(y ~ factor, method="jitter", cex.lab=1.5, xlab=xlab,
           col=col, vertical=TRUE, ylab=ylab)
boxplot(y ~ factor, cex.lab=1.5, main=NULL, boxwex=0.3, col=col,
        xlab=xlab, ylab=ylab)

## We test experimental effects
g = lm(y ~ factor)
summary(g)
anova(g)
summary(aov(g))

pairwise.t.test(y, factor, p.adjust.method="bonferroni")
TukeyHSD(aov(g))
par(mfrow=c(1,1))
plot(TukeyHSD(aov(g)))

## other example
y2 = as.numeric(ex[row.names(ex)=="1242_at", B1B2B3])
ylab = "Ets2 expression"

par(mfrow=c(1, 2))
stripchart(y2 ~ factor, method="jitter", cex.lab=1.5,
           vertical=TRUE, xlab=xlab, ylab=ylab, col=col)
boxplot(y2 ~ factor, cex.lab=1.5, main=NULL, xlab=xlab,
        boxwex=0.3, ylab=ylab, col=col)

g = lm(y2 ~ factor)
summary(g)
anova(g)
summary(aov(g))

pairwise.t.test(y2, factor, p.adjust.method="bonferroni")
TukeyHSD(aov(g))
par(mfrow=c(1,1))
plot(TukeyHSD(aov(g)))

# Multiple Testing for ANOVA
## How many genes of the ALL data is H0 of equal means rejected?
dim(ex[, B1B2B3])
fun = function(t) anova(lm(t ~ factor))$Pr[1]
anova.pValues = apply(ex[, B1B2B3], 1, fun)

pBF = p.adjust(anova.pValues, method="bonferroni")
pHO = p.adjust(anova.pValues, method="holm")
pBH = p.adjust(anova.pValues, method="BH")

alpha = 0.05
c(sum(pBF < alpha), sum(pHO < alpha), sum(pBH < alpha))

# Checking Assumptions
## there are two assumptions : normality / homoscedasticity
## normality can be tested by Shapiro-Wilk test
## homoscedasticity can be tested by Breusch and Pagan test
library(ALL)
data(ALL)

B1B2B3 = ALL$BT %in% c("B1", "B2", "B3")
ex = exprs(ALL)
y = as.numeric(ex[row.names(ex)=="1866_g_at", B1B2B3])
factor = factor(ALL$BT[B1B2B3], labels=c("B1", "B2", "B3"))

res = residuals(lm(y ~ factor))
shapiro.test(res) # reject H0 -> normality does not exist

par(mfrow=c(1, 2))
qqnorm(res, pch=19, cex.lab=1.5, col="red", main=NULL)
qqline(res)
hist(res, nclass=20, col="orange", xlab="residuals", main="") # looks like skewd normal distribution

## Breusch and Pagan Null hypothesis : error variances are all equal
install.packages("lmtest")
library(lmtest)
bptest(lm(y ~ factor), studentize=FALSE)  # reject H0 -> error variances are not equal

bartlett.test(y ~ factor)
fligner.test(y, factor)
library(car)
leveneTest(y, factor)

fit = lm(y ~ factor)$fit
par(mfrow=c(1,1))
plot(fit, res, pch=19, col="red", xlab="Fitted values", ylab="Residuals")
