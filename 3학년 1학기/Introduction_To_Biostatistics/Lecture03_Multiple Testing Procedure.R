# Bonferroni Adjustment
## Bonferroni Adjustment는 대용량 데이터에서 FWER이 매우 커지는 문제를 방지하기 위해서 고안된 아이디어
data(golub, package="multtest")
golubFactor = factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))

pval = NULL
m = nrow(golub)
for (i in 1:m){
  pval[i] = t.test(golub[i, ] ~ golubFactor)$p.val
}

sum(pval < 0.05)    # Type 1 Error 가능성 다수 존재
sum(pval < 0.05/m)  # Type 1 Error 가능성 control

pj = p.adjust(pval, method="bonferroni")  # Bonferroni adjustment p-value
sum(pj < 0.05)  # = sum(pval < 0.05/m)

top = 20
oo = order(pval)
oot = oo[1:top]
data.frame(gene=golub.gnames[oot, 3], pvalue=pval[oot])

par(mfrow=c(1, 2))
hist(pval, col="orange", xlab="", main="Un-adjusted p-values")
hist(pj, col="purple", xlab="", main="Adjusted p-values")

par(mfrow=c(1, 1))
plot(-log10(pval), type="p", pch=20, col="red", xlab="gene")
abline(h=-log10(0.05/m), lty=2) # 스케일링을 통해 Type 1 error 가능성 확인

## Bonferroni Adjustment는 귀무가설의 기각에 있어 매우 보수적인 메소드라는 단점이 존재
## 매우 Significant 하지 않은 변수는 귀무가설을 기각하지 못한다 -> 유의미한 변수를 많이 끌어내기가 힘듬

# Holm's Step-Down Procedure
## Holm's method는 Bonferroni procedure 보다 덜 보수적 (무조건 Bonferroni가 덜 Reject H0)
## 단순한 a/m이 아닌 관찰된 p-value에 따라 threshold가 변화 (즉 flexible)
## Holm's method는 multiple test 에서 독립 가정을 만들지 않음

ph = p.adjust(pval, method="holm")
sum(ph < 0.05)

top = sum(ph < 0.05)
oo = order(pval)
oot = oo[1:top]
data.frame(gene=golub.gnames[oot, 3], pvalue=pval[oot],
           Bonferroni=pj[oot], Holm=ph[oot])

pval2 = NULL
for (i in 1:m){
  pval2[i] = wilcox.test(golub[i, ] ~ golubFactor)$p.val
}

pj2 = p.adjust(pval2, method="bonferroni")
ph2 = p.adjust(pval2, method="holm")
c(sum(pval2 < 0.05), sum(pj2 < 0.05), sum(ph2 < 0.05))
## 순수 pvalue는 매우 많이 기각, Bonferroni는 매우 덜 기각(보수적), Holm's는 적당히!

# Benjamini-Hochberg procedure
## Method to conntrol FDR
## Error를 컨트롤하는 것 보다는 Siginificant discovery에 집중!
## 굉장히 Classic 하며 한계가 조금은 존재하는 method, 하지만 이해하기에는 쉬움
data(golub, package="multtest")
golubFactor = factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))

pval = NULL
m = nrow(golub)
for (i in 1:m){
	pval[i] = t.test(golub[i, ] ~ golubFactor)$p.val
}

pj = p.adjust(pval, method="bonferroni")
pBH = p.adjust(pval, method="BH")		# Benjamini-Hochberg
c(sum(pj < 0.05), sum(pBH < 0.05))	# more reject H0

q = 0.05
poo = sort(pval)
wh = which(poo < q*(1:m)/m)
whh = 1:max(wh)

plot(poo, pch=20, ylab="P-values", xlab="Genes", main="")
points(whh, poo[whh], col=4, pch=20)
abline(a=0, b=q/m, col=2)
abline(h=q/m, col="darkgreen")

top = 1000
poot = poo[1:top]

plot(poot, pch=20, ylab="P-values", xlab="Genes", main="")
points(whh, poot[whh], col=4, pch=20)
abline(a=0, b=q/m, col=2)
abline(h=q/m, col="darkgreen")

# Permutation to Compute p-value
install.packages("ISLR2")
library(ISLR2)
data(Khan)

?Khan
str(Khan)
attach(Khan)

x = rbind(xtrain, xtest)
y = c(as.numeric(ytrain), as.numeric(ytest))
dim(x)
table(y)

x = as.matrix(x)
x1 = x[which(y==2), ]
x2 = x[which(y==4), ]
n1 = nrow(x1)
n2 = nrow(x2)