# Example of Classification
## Classification via CCND3 expression : Let's consider cutoff point 1.27

data(golub, package="multtest")
Labels = factor(golub.cl, levels=0:1, labels=c("ALL", "notALL"))
ccnd3 = grep("CCND3", golub.gnames[,2], ignore.case=TRUE)

decision = golub[ccnd3, ] > 1.27
c(sum(decision), ncol(golub))

Pred = factor(decision, levels=c("TRUE", "FALSE"), labels=c("ALL", "notALL"))
table(Labels, Pred)

tab = table(Labels, Pred)
sensitivity = tab[1, 1] / sum(tab[1, ])
specificity = tab[2, 2] / sum(tab[2, ])
PV.positive = tab[1, 1] / sum(tab[ ,1])
PV.negative = tab[2, 2] / sum(tab[, 2])
c(sensitivity, specificity, PV.positive, PV.negative) # good cutoff

decision = golub[ccnd3, ] > 2.18
Pred = factor(decision, levels=c("TRUE", "FALSE"), labels=c("ALL", "notALL"))

tab = table(Labels, Pred)
sensitivity = tab[1, 1] / sum(tab[1, ])
specificity = tab[2, 2] / sum(tab[2, ])
PV.positive = tab[1, 1] / sum(tab[ ,1])
PV.negative = tab[2, 2] / sum(tab[, 2])
c(sensitivity, specificity, PV.positive, PV.negative) # bad cutoff

decision = golub[ccnd3, ] > 0.4
Pred = factor(decision, levels=c("TRUE", "FALSE"), labels=c("ALL", "notALL"))

tab = table(Labels, Pred)
sensitivity = tab[1, 1] / sum(tab[1, ])
specificity = tab[2, 2] / sum(tab[2, ])
PV.positive = tab[1, 1] / sum(tab[ ,1])
PV.negative = tab[2, 2] / sum(tab[, 2])
c(sensitivity, specificity, PV.positive, PV.negative) # bad cutoff

sort(golub[ccnd3, Labels=="ALL"], decreasing=TRUE)
sort(golub[ccnd3, Labels!="ALL"], decreasing=TRUE)

# Example of ROC Curve
install.packages("ROCR")
library(ROCR)

true = factor(golub.cl, levels=0:1, labels=c("TRUE", "FALSE"))
predccnd3 = prediction(golub[ccnd3, ], true) # 윗부분을 true로 예측
perfccnd3 = performance(predccnd3, "tpr", "fpr") # parameter : "tp rate", "fp rate"
plot(perfccnd3, lwd=4, col="blue")

slotNames(perfccnd3)
list(perfccnd3@x.name, perfccnd3@x.values) # False positive rate
list(perfccnd3@y.name, perfccnd3@y.values) # True positive rate
list(perfccnd3@alpha.name, perfccnd3@alpha.values) # Cutoff

performance(predccnd3, "auc")@y.values # AUC score

gdf5 = grep("GDF5", golub.gnames[, 2], ignore.case=TRUE) # unlabeled gene
predgdf5 = prediction(golub[gdf5, ], true)
perfgdf5 = performance(predgdf5, "tpr", "fpr")
plot(perfgdf5, lwd=4, col="magenta") # 그림이 이상함(y=x 직선아래에 있을 수 없음, 즉 prediction 부호가 반대일 것)

## We can improve Gdf5-classifier by reversing the classification labels
true2 = factor(golub.cl, levels=0:1, labels=c("FALSE", "TRUE"))
predgdf5 = prediction(golub[gdf5, ], true2)
perfgdf5 = performance(predgdf5, "tpr", "fpr")
performance(predgdf5, "auc")@y.values

plot(perfgdf5, lwd=4, col="magenta")
plot(perfccnd3, lwd=4, col="blue", add=TRUE)

## Still low performance, because ccnd3 is a biomarker for ALL, but Gdf5 is not.

# Example of Logistic Regression
data(golub, package="multtest")
Factor = factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
ccnd3 = grep("CCND3", golub.gnames[, 2], ignore.case=TRUE)

## glm : generalized linear model 
g = glm(Factor ~ golub[ccnd3, ], family=binomial) # glm func은 lm func과는 다르게 not continuous type도 포함가능
summary(g)
summary(g)$coef
pchisq(deviance(g), df.residual(g), lower=FALSE) # not reject H0 => model well fitted! (called goodness-of-fit)

## predictive accuracy
eta = cbind(1, golub[ccnd3, ]) %*% g$coef # 1 : intercept , golub[ccnd3, ] : X
exp(eta) / (1 + exp(eta)) # probability p(y_i = 1 | x_i)
predict(g, type="response") # equals to probability

x = golub[ccnd3, ]
y = 1 - golub.cl
u = order(x)

plot(x, y, pch="|", col="orange", ylim=c(-0.1,1.1),
     xlab="Gene expression values", ylab="Probability of ALL")
abline(h=c(0, 1), lty=2)
points(x[u], 1-g$fit[u], col="blue", lwd=2, pch=20)

pred = predict(g, type="response") < 0.5 # 0.5 ; threshold, 이 값이 변화하는 것에 따른 sensitivity, specifity 를 관찰하는게 ROC curve
est = factor(pred, levels=c(TRUE, FALSE), labels=c("ALL", "not ALL"))
table(est, Factor)

library(ROCR)
true = factor(golub.cl, levels=0:1, labels=c("FALSE", "TRUE"))
yprob = predict(g, type="response")

pred = prediction(yprob, true)
perf = performance(pred, "tpr", "fpr")
plot(perf, lwd=2, col="blue")
performance(pred, "auc")@y.values

# Example of Classification Trees
## Example : Optimal gene expression thresholds
install.packages(c("rpart", "rpart.plot"))
library(rpart)
library(rpart.plot)

n = 10
factor = factor(c(rep(1, n), rep(2, n), rep(3, n)))
levels(factor) = c("ALL1", "ALL2", "AML")
factor

set.seed(123)
sigma = 0.5
geneA = c(rnorm(n, 0, sigma), rnorm(n, 2, sigma), rnorm(n, 4, sigma))

tapply(geneA, factor, range) # tapply : data를 factor별로 apply하는 함수
boxplot(geneA ~ factor, cex.lab=1.5, main=NULL, boxwex=0.3,
        col=c("lightblue", "orange", "lightgreen"),
        xlab="Type of leukemia", ylab="Gene expression")

data = data.frame(factor, geneA)
rpartFit = rpart(factor ~ geneA, method="class", data=data) # method="class" : classification
prp(rpartFit, branch.lwd=4, branch.col="darkgreen", extra=101) # decision tree의 유용한 시각화

rpartFit # can check estimated splits
summary(rpartFit) # can check concrete procedure of classification trees

## Example : Gene selection
sigma = 0.5
factor = factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
levels(factor) = c("ALL1", "ALL2", "AML")

set.seed(123)
geneA = c(rnorm(20, 0, sigma), rnorm(10, 2, sigma))
geneB = c(rnorm(10, 0, sigma), rnorm(20, 2, sigma))
geneC = c(rnorm(30, 1, sigma))
data = data.frame(factor, geneA, geneB, geneC)

par(mfrow=c(1,3))
boxplot(geneA ~ factor, main="Gene A", boxwex=0.3, ylab="",
        col=c("lightblue", "orange", "lightgreen"), xlab="")
boxplot(geneB ~ factor, main="Gene B", boxwex=0.3, ylab="",
        col=c("lightblue", "orange", "lightgreen"), xlab="")
boxplot(geneC ~ factor, main="Gene C", boxwex=0.3, ylab="",
        col=c("lightblue", "orange", "lightgreen"), xlab="")

## from resultant boxplot,
## Gene A discriminates well between ALL and AML
## Gene B discriminates well between ALL1 and ALL2/AML
## Gene C does not discriminate at all

tapply(geneA, factor, range)
tapply(geneB, factor, range)
tapply(geneC, factor, range)

rpartFit = rpart(factor ~ geneA + geneB + geneC, method="class", data=data)
par(mfrow=c(1,1))
prp(rpartFit, branch.lwd=4, branch.col="blue", extra=101)

## Example : Classification by jCCND3 gene expression
data(golub, package="multtest")
golubFactor = factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
ccnd3 = grep("CCND3", golub.gnames[,2], ignore.case=TRUE)

boxplot(golub[ccnd3, ] ~ golubFactor, main="", boxwex=0.3,
        col=c("lightblue", "orange"), xlab="Type of patients",
        ylab="Cyclin D3 gene expression")
tapply(golub[ccnd3, ], golubFactor, range)

gene = golub[ccnd3, ]
tree = rpart(golubFactor ~ gene, method="class")
prp(tree, branch.lwd=4, branch.col="blue", extra=101)
summary(tree)

predict(tree, type="prob")
predict(tree, type="class")
pred = predict(tree, type="class")
table(golubFactor, pred) # pretty well predicted

## Example : Application to the acute lymphoblastic leukemia data
library(ALL)
data(ALL)

ALLB123 = ALL[, ALL$BT %in% c("B1", "B2", "B3")]
ALLB123$BT
table(ALLB123$BT)
table(ALL$BT)

names = featureNames(ALL)
names

BiocManager::install("hgu95av2.db")
library(hgu95av2.db)

symb = mget(names, env=hgu95av2SYMBOL)
unlist(symb) # convert list to vector
unlist(symb)[1:100]

ALLBTnames = ALLB123[names, ]
dim(ALLBTnames)
dim(ALL)

probeData = as.matrix(exprs(ALLBTnames))
row.names(probeData) = unlist(symb)
probeData[1:20, 1:5]

## Since 12,625 gene expressions is too large, we select the genes p-value samller than 0.00001
fun = function(x) anova(lm(x ~ ALLB123$BT))$Pr[1]
anova.pValue = apply(exprs(ALLB123), 1, fun)

ww = anova.pValue < 0.00001
sum(ww)

diagnosed = factor(ALLBTnames$BT)
diagnosed
Data = data.frame(t(probeData[ww, ]))
dim(Data)

library(rpart)
library(rpart.plot)
fit = rpart(diagnosed ~ ., data=Data)
prp(fit, branch.lwd=4, branch.col="blue", extra=101)

pred = predict(fit, type="class")
table(diagnosed, pred)

prob = predict(fit, type="prob")
out = data.frame(prob, predicted=pred, diagnosis=diagnosed)
out # can compare the predicted probability of each class
out[pred!=diagnosed, ]

## Generally predictive accuracy is applied to evaluate a classification model
## It is common practice to split the data two parts: Training set and a Test(Validation) set
## Construct confusion matrix -> evaluate predictive accuracy

## Example : Separate training and test sets
set.seed(123)
train = sample(1:78, 39, replace=FALSE)
test = setdiff(1:78, train)
table(diagnosed[train])
table(diagnosed[test])

fit.tr = rpart(diagnosed ~ ., data=Data, subset=train)
pred.tr = predict(fit.tr, Data[train, ], type="class")
table(pred.tr, diagnosed[train])
mean(pred.tr != diagnosed[train])

pred.te = predict(fit.tr, Data[test, ], type="class")
table(pred.te, diagnosed[test])
mean(pred.te != diagnosed[test])

pred.te = predict(fit.tr, Data[test, ], type="class")
table(pred.te, diagnosed[test])
mean(pred.te != diagnosed[test])

## Example of Random Forest
library(ALL)
data(ALL)

ALLB123 = ALL[, ALL$BT %in% c("B1", "B2", "B3")]
names = featureNames(ALL)
ALLBTnames = ALLB123[names, ]
probeData = as.matrix(exprs(ALLBTnames))
fun = function(x) anova(lm(x ~ ALLB123$BT))$Pr[1]
anova.pValue = apply(exprs(ALLB123), 1, fun)
ww = anova.pValue < 0.00001
diagnosed = factor(ALLBTnames$BT)
Data = data.frame(t(probeData[ww, ]))

set.seed(123)
train = sample(1:78, 39, replace=FALSE)
test = setdiff(1:78, train)

install.packages("randomForest")
library(randomForest)

xtr = Data[train, ]
xte = Data[test, ]
ytr = diagnosed[train]
yte = diagnosed[test]

rf1 = randomForest(x=xtr, y=ytr, xtest=xte, ytest=yte, ntree=1000, mtry=1) # mtry : 각 노드 설정 시 설명변수 후보 개수
rf1$test$confusion
rf1.conf = rf1$test$confusion[1:3, 1:3]
1 - sum(diag(rf1.conf)) / sum(rf1.conf)

rf2 = randomForest(x=xtr, y=ytr, xtest=xte, ytest=yte, ntree=1000, mtry=3)
rf2$test$confusion
rf2.conf = rf2$test$confusion[1:3, 1:3]
1 - sum(diag(rf2.conf)) / sum(rf2.conf)

rf3 = randomForest(x=xtr, y=ytr, xtest=xte, ytest=yte, ntree=1000, mtry=3)
rf3$test$confusion
rf3.conf = rf3$test$confusion[1:3, 1:3]
1 - sum(diag(rf3.conf)) / sum(rf3.conf)
