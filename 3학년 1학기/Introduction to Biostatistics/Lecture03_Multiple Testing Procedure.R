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

top = 20
oo = order(pval)
oot = oo[1:top]
data.frame(gene=golub.gnames[oot, 3], pvalue=pval[oot])


pj = p.adjust(pval, method="bonferroni")  # Bonferroni adjustment p-value
sum(pj < 0.05)  # = sum(pval < 0.05/m)

top = sum(pj < 0.05)
oo = order(pval)
oot = oo[1:top]
data.frame(gene=golub.gnames[oot, 3], pvalue=pval[oot], adj.pvalue=pj[oot])

par(mfrow=c(1, 2))
hist(pval, col="orange", xlab="", main="Un-adjusted p-values")
hist(pj, col="purple", xlab="", main="Adjusted p-values")

par(mfrow=c(1, 1))
plot(-log10(pval), type="p", pch=20, col="red", xlab="gene")
abline(h=-log10(0.05/m), lty=2) # 스케일링을 통해 Type 1 error 가능성 확인 (검은선 위로 Reject)

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
## install.packages("ISLR2")
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
x1 = x[which(y==2), ]	# target=2인 데이터로만
x2 = x[which(y==4), ]	# target=4인 데이터로만
n1 = nrow(x1)
n2 = nrow(x2)

k = 11 # 11번째 gene에 대해서 검정
shapiro.test(x1[, k])	# not reject H0 -> 정규성을 따른다 -> t.test 가능
shapiro.test(x2[, k])	# not reject H0

t.out = t.test(x1[, k], x2[, k], var.equal=TRUE)
t.out$statistic
t.out$p.value	# reject H0 -> 두 집단간의 차이가 존재한다 (p-value가 아슬아슬하긴 함 = 차이가 있지만 크진 않을수도?)

set.seed(1)
B = 10000
T = rep(NA, B)
for (j in 1:B) {	# permutation test
	data = sample(c(x1[, k], x2[, k]))
	T[j] = t.test(data[1:n1], data[(n1 + 1) : (n1 + n2)], 
	var.eqaul=TRUE)$statistic
}
mean((abs(T) >= abs(t.out$statistic)))		# t-test의 p-value값과 거의 비슷함! 사실 정규성을 따르므로 굳이 permuation 할 필요 없음

hist(T, breaks=100, xlim=c(-4.2, 4.2), main="", xlab="Null Distribution of Test Statistics", col=7)	# under H0에서의 t분포 시각화
x0 = seq(-4.2, 4.2, len=1000)
y0 = dt(seq(-4.2, 4.2, len=1000), df=(n1+n2-2))
lines(x0, y0*1000, col=2, lwd=3)	

TT = t.out$statistic
abline(v=-TT, col=4, lty=2, lwd=2)
abline(v=TT, col=4, lty=2, lwd=2)
text(TT-1, 350, paste("T = ", round(TT, 4), sep=""), col=4)


k = 877
shapiro.test(x1[, k])	# reject H0 -> 정규성 위반 -> t-test 하면 안됨!
shapiro.test(x2[, k])	# reject H0

par(mfrow=c(1, 2))
qqnorm(x1[, k], pch=19, col='red', main=expression(x[1]))	# Q-Q plot
qqline(x1[, k])
qqnorm(x2[, k], pch=19, col='red', main=expression(x[2]))
qqline(x2[, k])

T = rep(NA, B)
set.seed(2)
for (j in 1:B) {
	dat = sample(c(x1[, k], x2[, k]))
	T[j] = t.test(dat[1:n1], dat[(n1 + 1):(n1 + n2)], 
	var.equal=TRUE)$statistic
}

t.out = t.test(x1[, k], x2[, k], var.equal=TRUE)
t.out$p.value	# 정규성 만족하지 않으므로 좋지 않음
mean((abs(T) >= abs(t.out$statistic)))	# 정규성을 만족할 때와 비교했을 때 p-value 값이 상이함을 알 수 있음

hist(T, breaks=100, xlim=c(-2.9, 2.9), main="", xlab="Null Distribution of Test Statistic", col=7)	# bimodel(봉우리 두개 형태)
x0 = seq(-2.9, 2.9, len=1000)
y0 = dt(seq(-2.9, 2.9, len=1000), df=(n1 + n2 -2))
lines(x0, y0*1000, col=2, lwd=3)

TT = t.out$statistic
abline(v=-TT, col=4, lty=2, lwd=2)
abline(v=TT, col=4, lty=2, lwd=2)
text(TT-1, 200, paste("T = ", round(TT, 4), sep=""), col=4)

# Computation of Plug-in FDR
m = 200
B = 1000	# 시간관계상 1000번이지, 실제로는 매우 적은 횟수

set.seed(111)
index = sample(2308, m)
T = rep(NA, m)
T.star = matrix(NA, nrow=m, ncol=B)

for (j in 1:m){
	k = index[j]
	T[j] = t.test(x1[, k], x2[, k], var.equal=TRUE)$statistic
	for (b in 1:B) {
		dat = sample(c(x1[, k], x2[, k]))
		T.star[j, b] = t.test(dat[1:n1], dat[(n1 + n1) : (n1 + n2)], var.equal=TRUE)$statistic
	}
}

c = sort(abs(T))
FDR = R = V = rep(NA, m)
for (j in 1:m){
	R[j] = sum(abs(T) >= c[j])
	V[j] = sum(abs(T.star) >= c[j])/B
	FDR[j] = V[j]/R[j]
}

plot(R, FDR, xlab="Number of Rejections", type="l", ylab="False Discovery Rate", col=4, lwd=3)

oo = order(abs(T))
R[oo] = R		# ordering을 하지 않으면 index가 뒤죽박죽
FDR[oo] = FDR

alpha = 0.1
max(R[FDR <= alpha])
sort(index[FDR <= alpha])

pval = NULL
for (j in 1:m){
	k = index[j]
	pval[j] = t.test(x1[, k], x2[, k], var.equal=TRUE)$p.value
}

pBH = p.adjust(pval, method="BH")
data.frame(Resampling=round(FDR ,6), BH=round(pBH, 6))	# Resampling과 Benjamini procedure의 값이 거의 비슷함을 알 수 있음

oo = order(pBH)
plot(FDR[oo], pBH[oo], pch=20, xlab="Reampling FDR", ylab="BH procedure")
abline(0, 1, col="red", lty=2)	# 시각화 결과 y=x선상에 거의 겹침, 즉 값이 거의 일치

## sampling approches useful if
## 1. No theoretical null distribution is available
## 2. Sample size is samll & observations are not normally distributed

# Manhattan Plot
## A manhattan plot is specific type of scatter plot

## install.packages("qqman")
library(qqman)

str(gwasResults)
head(gwasResults)
tail(gwasResults)

as.data.frame(table(gwasResults$CHR))
manhattan(gwasResults, col=c("blue4", "orange3"), main="Manhattan Plot")

## 좀 더 예쁘게 만들어보자!
## install.packages("RColorBrewer")
library(RColorBrewer)

manhattan(gwasResults, suggestiveline=F, genow)
manhattan(gwasResults, suggestiveline=F, genomewideline=F, col=brewer.pal(5, "Set2"), cex=0.7)
legend("topright", c("Bonferroni", "Holm", "BH"), bty="n", cex=1.2, col=c("red", "blue", "orange"), lty=2)

alpha = 0.05
pval = gwasResults[, 4]

Bonf = p.adjust(pval, method="bonferroni")
bmax = max(pval[Bonf < alpha])
abline(h=-log10(pval[pval==bmax]), lty=2, col="red")

Holm = p.adjust(pval, method="holm")
hmax = max(pval[Holm < alpha])
abline(h=-log10(pval[pval==hmax]), lty=2, col="blue")

## line이 겹치는 이유는 bmax=hmax이기 때문

BHp = p.adjust(pval, method="BH")
ymax = max(pval[BHp < alpha])
abline(h=-log10(pval[pval==ymax]), lty=2, col="orange")

## FWER이 FDR에 비해 기각에 있어 보수적이기 때문에 Num of rejections가 더 적은것을 알 수 있음