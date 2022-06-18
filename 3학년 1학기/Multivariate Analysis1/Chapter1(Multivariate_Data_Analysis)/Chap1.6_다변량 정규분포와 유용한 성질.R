### CODE 1.6.1
# Bivariate normal distribution 그래프 3D 시각화 함수 구현
BVNpdf <- function(mu1,mu2,sig1,sig2,rho) {
par(mfrow=c(1,2))
s12 = sig1*sig2*rho
s11 = sig1^2
s22 = sig2^2
Sig <- matrix(c(s11,s12,s12,s22),ncol=2,nrow=2,byrow=T)
Sinv <- solve(Sig)
x1 <- seq(mu1 - 3.5*sig1,mu1+3.5*sig1,len=50)
fx1 <- seq(-3.5,3.5,len=50)
x2 <- seq(mu2 - 3.5*sig2,mu2+3.5*sig2,len=50)
fx2 <- seq(-3.5,3.5,len=50)
f <- function(x1,x2) {	# Bivariate normal distribution pdf
                cons <- ((2*pi)*det(Sig)^.5)^{-1}	# def() : 행렬식을 구하는 method
                cons*exp(-(.5*(1 - rho^2)^{-1})*(x1^2+x2^2-2*rho*x1*x2))
        }
f <- outer(fx1,fx2,f)	# outer() : outer product -> 두 벡터(열벡터)를 외적하여 행렬을 얻음
persp(x1,x2,f,theta = 30, expand=.50)	# persp() : creating 3D plots
title(main="Bivariate Normal pdf")
contour(x1,x2,f,lty="solid",drawlabels=F)	# contour() : creating contour plot
title(main="Contour Plot of BVN pdf")
BVNpdf(mu1,mu2,sig1,sig2,rho)
par(mfrow=c(1,1))
}

BVNpdf(0,0,1,1,0)
BVNpdf(0,0,1,1,0.8)
BVNpdf(0,0,1,1,-0.8)