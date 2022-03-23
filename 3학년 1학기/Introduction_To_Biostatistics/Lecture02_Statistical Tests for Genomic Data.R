# The Z-test
data(golub, package="multtest")
golubFactor = factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
x = golub[2058, golubFactor=="ALL"]
n = length(x)
sigma = 0.25 ; mu0 = 0 # 현실에서 알기는 거의 불가능

z.value = sqrt(n)*(mean(x)-mu0)/sigma
2*pnorm(-abs(z.value)) # Significance level 0.05 보다 훨씬 크기 때문에 Not reject H0


