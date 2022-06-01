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
