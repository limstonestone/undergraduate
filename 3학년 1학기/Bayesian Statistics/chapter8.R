#### Figure 8.1
load("nels.RData") 
y1<-y.school1
y2<-y.school2


par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
boxplot(list(y1,y2),range=0,ylab="score",names=c("school 1","school 2"))

n1<-length(y1)
n2<-length(y2)
mean(y1)
mean(y2)
sd(c(y1,y2))
s2p<- ( var(y1)*(n1-1) + var(y2)*(n2-1) )/(n1+n2-2 )
tstat<- ( mean(y1)-mean(y2) ) /
   sqrt( s2p*(1/length(y1)+1/length(y2)))
t.test(y1,y2)


ts<-seq(-4,4,length=100)
plot(ts,dt(ts,n1+n2-1),type="l",xlab=expression(italic(t)),ylab="density")
abline(v=tstat,lwd=2,col="gray")




#### Two sample comparison

## data 
n1<-length(y1) ; n2<-length(y2)

## prior parameters
mu0<-50 ; g02<-625
del0<-0 ; t02<-625
s20<-100; nu0<-1

## starting values
mu<- ( mean(y1) + mean(y2) )/2
del<- ( mean(y1) - mean(y2) )/2

## Gibbs sampler
MU<-DEL<-S2<-NULL
Y12<-NULL
set.seed(1)
for(s in 1:5000) 
{

  ##update s2
  s2<-1/rgamma(1,(nu0+n1+n2)/2, 
        (nu0*s20+sum((y1-mu-del)^2)+sum((y2-mu+del)^2) )/2)
  ##

  ##update mu
  var.mu<-  1/(1/g02+ (n1+n2)/s2 )
  mean.mu<- var.mu*( mu0/g02 + sum(y1-del)/s2 + sum(y2+del)/s2 )
  mu<-rnorm(1,mean.mu,sqrt(var.mu))
  ##

  ##update del
  var.del<-  1/(1/t02+ (n1+n2)/s2 )
  mean.del<- var.del*( del0/t02 + sum(y1-mu)/s2 - sum(y2-mu)/s2 )
  del<-rnorm(1,mean.del,sqrt(var.del))
  ##

  ##save parameter values
  MU<-c(MU,mu) ; DEL<-c(DEL,del) ; S2<-c(S2,s2) 
  Y12<-rbind(Y12,c(rnorm(2,mu+c(1,-1)*del,sqrt(s2) ) ) )
}                 



#### Figure 
plot(MU+DEL,MU-DEL)
cor(MU+DEL,MU-DEL)


par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))

plot( density(MU,adj=2),xlim=c(mu0-sqrt(g02),mu0+sqrt(g02)), 
   main="",xlab=expression(mu),ylab="density",lwd=2 )
ds<-seq(mu0-sqrt(g02),mu0+sqrt(g02),length=100)
lines(ds,dnorm(ds,mu0,sqrt(g02)),lwd=2,col="gray" )
legend(22,.27,legend=c("prior","posterior"),lwd=c(2,2),col=c("black","gray"),
       bty="n")

plot( density(DEL,adj=2),xlim=c(-sqrt(t02),sqrt(t02)),
   main="",xlab=expression(delta),ylab="density",lwd=2 )
ds<-seq(-sqrt(t02),sqrt(t02),length=100)
lines(ds,dnorm(ds,0,sqrt(t02)),lwd=2,col="gray" )
legend(-28,.27,legend=c("prior","posterior"),lwd=c(2,2),col=c("black","gray"),
       bty="n")



## other posterior quantities  
quantile(DEL,c(.025,.5,.975))
quantile(DEL*2,c(.025,.5,.975))
mean(DEL>0)
mean(Y12[,1]>Y12[,2])





#### Put data into list form. This is different from in the book. 
Y<-list()
YM<-NULL
J<-max(Y.school.mathscore[,1])
n<-ybar<-ymed<-s2<-rep(0,J)
for(j in 1:J) {
  Y[[j]]<-Y.school.mathscore[ Y.school.mathscore[,1]==j,2] 
  ybar[j]<-mean(Y[[j]])
  ymed[j]<-median(Y[[j]])
  n[j]<-length(Y[[j]])
  s2[j]<-var(Y[[j]])
  YM<-rbind( YM, cbind( rep(j,n[j]), Y[[j]] ))
               }

## YM is like Y.school.mathscore in the book. 
colnames(YM)<-c("school","mathscore")



#### Figure 

par(mfrow=c(1,1))
plot(c(1,J),range(Y) ,type="n",ylab="math score",xlab="rank of  school-specific math score  average")

for(l in 1:J)  {
 j<-order(ybar)[l]
 points( rep(l,n[j]), Y[[j]],pch=16,cex=.6 )
 segments( l,min(Y[[j]]),l,max(Y[[j]]))
                }

abline(h=mean(ybar))




#### Figure 
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0)) 
hist(ybar,main="",xlab="sample mean")
plot(n,ybar,xlab="sample size",ylab="sample mean")






#### Figure 

par(mar=c(0,0,0,0),mgp=c(0,0,0))
plot(c(.15,.85),c(.15,.85),type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")

text( .5,.8 , expression(paste( mu ,",", tau^2 )) )

text(  c(.2,.3,.5,.7,.8),  rep(.6,5),
        expression( theta[1],theta[2],".....",theta[m-1],theta[m]) )

segments(  rep(.5,4),rep(.75,4), c(.2,.3,.7,.8),rep(.65,4) )

text( c(.2,.3,.5,.7,.8), rep(.4,5),
       expression( bar(y)[1], bar(y)[2],".....",bar(y)[m-1],bar(y)[m]) )

segments(  c(.2,.3,.7,.8),rep(.55,4), c(.2,.3,.7,.8),rep(.45,4) )


segments(  rep(.5,4),rep(.25,4), c(.2,.3,.7,.8),rep(.35,4) )

text( .5,.2,expression(sigma^2))




#### MCMC approximation to posterior for the hierarchical normal model

## weakly informative priors
nu0<-1  ; s20<-100
eta0<-1 ; t20<-100
mu0<-50 ; g20<-25

## starting values
m<-length(Y) 
n<-sv<-ybar<-rep(NA,m) 
for(j in 1:m) 
{ 
  ybar[j]<-mean(Y[[j]])
  sv[j]<-var(Y[[j]])
  n[j]<-length(Y[[j]]) 
}
theta<-ybar
sigma2<-mean(sv)
mu<-mean(theta)
tau2<-var(theta)

## setup MCMC
set.seed(1)
S<-5000
THETA<-matrix( nrow=S,ncol=m)
MST<-matrix( nrow=S,ncol=3)

## MCMC algorithm
for(s in 1:S) 
{

  # sample new values of the thetas
  for(j in 1:m) 
  {
    vtheta<-1/(n[j]/sigma2+1/tau2)
    etheta<-vtheta*(ybar[j]*n[j]/sigma2+mu/tau2)
    theta[j]<-rnorm(1,etheta,sqrt(vtheta))
   }

  #sample new value of sigma2
  nun<-nu0+sum(n)
  ss<-nu0*s20;for(j in 1:m){ss<-ss+sum((Y[[j]]-theta[j])^2)}
  sigma2<-1/rgamma(1,nun/2,ss/2)

  #sample a new value of mu
  vmu<- 1/(m/tau2+1/g20)
  emu<- vmu*(m*mean(theta)/tau2 + mu0/g20)
  mu<-rnorm(1,emu,sqrt(vmu)) 

  # sample a new value of tau2
  etam<-eta0+m
  ss<- eta0*t20 + sum( (theta-mu)^2 )
  tau2<-1/rgamma(1,etam/2,ss/2)

  #store results
  THETA[s,]<-theta
  MST[s,]<-c(mu,sigma2,tau2)

} 


mcmc1<-list(THETA=THETA,MST=MST)





#### Figure 


#### A plot for evaluating lack of convergence
stationarity.plot<-function(x,...){

S<-length(x)
scan<-1:S
ng<-min( round(S/100),10)
group<-S*ceiling( ng*scan/S) /ng

boxplot(x~group,...)               }



par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.7,.7,0))

stationarity.plot(MST[,1],xlab="iteration",ylab=expression(mu))
stationarity.plot(MST[,2],xlab="iteration",ylab=expression(sigma^2))
stationarity.plot(MST[,3],xlab="iteration",ylab=expression(tau^2))




#### More MCMC diagnostics
library(coda)
effectiveSize(MST)
par(mfrow=c(1,3))
acf(MST[,1]) 
acf(MST[,2]) 
acf(MST[,3]) 

effectiveSize(THETA) -> esTHETA

MCERR<-  apply(MST,2,sd)/sqrt( effectiveSize(MST) )
apply(MST,2,mean)

TMCERR<-  apply(THETA,2,sd)/sqrt( effectiveSize(THETA) )



#### Figure 

par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.7,.7,0))
plot(density(MST[,1],adj=2),xlab=expression(mu),main="",lwd=2,
ylab=expression(paste(italic("p("),mu,"|",italic(y[1]),"...",italic(y[m]),")")))
abline( v=quantile(MST[,1],c(.025,.5,.975)),col="gray",lty=c(3,2,3) )
plot(density(MST[,2],adj=2),xlab=expression(sigma^2),main="", lwd=2,
ylab=expression(paste(italic("p("),sigma^2,"|",italic(y[1]),"...",italic(y[m]),")")))
abline( v=quantile(MST[,2],c(.025,.5,.975)),col="gray",lty=c(3,2,3) )
plot(density(MST[,3],adj=2),xlab=expression(tau^2),main="",lwd=2,
ylab=expression(paste(italic("p("),tau^2,"|",italic(y[1]),"...",italic(y[m]),")")))
abline( v=quantile(MST[,3],c(.025,.5,.975)),col="gray",lty=c(3,2,3) )

mean((MST[,1]))
mean(sqrt(MST[,2]))
mean(sqrt(MST[,3]))



#### Figure 

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

theta.hat<-apply(THETA,2,mean)
plot(ybar,theta.hat,xlab=expression(bar(italic(y))),ylab=expression(hat(theta)))
abline(0,1)
plot(n,ybar-theta.hat,ylab=expression( bar(italic(y))-hat(theta) ),xlab="sample size")
abline(h=0)




#### Figure 

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

theta.order<-order(theta.hat)
theta.order[1:20]

idx<-c(46,82)

ybar.order<-order(ybar)
ybar.order[1:20]

ybar[c(46,82)]
n[c(46,82)]
theta.hat[c(46,82)]

mean(THETA[,46]<THETA[,82])

par(mfrow=c(1,1))
plot(density(THETA[,46],adj=2),col="black",xlim=
  range(c(Y[[46]],Y[[82]],THETA[,c(46,82)])),lwd=2,
        main="",xlab="math score",ylim=c(-.05,.2),ylab="",yaxt="n")
axis(side=2,at=c(0,0.10,0.20) )
lines(density(THETA[,82],adj=2),col="gray",lwd=2)
abline(h=0)

points( Y[[46]],rep(-0.01666,n[46]), col="black",pch=16)
points( ybar[46],-.01666,col="black",pch=16 ,cex=1.5)
abline( h=-.01666,col="black")

points( Y[[82]],rep(-0.0333,n[82]), col="gray",pch=16)
points( ybar[82],-.0333,col="gray",pch=16 ,cex=1.5)
abline( h=-.0333,col="gray")

segments(mean(MST[,1]), 0,mean(MST[,1]),1,lwd=2,lty=2 )

legend(52.5,.15,legend=c("school 46","school 82",
     expression(paste("E[", mu,"|",italic(y[1]),"...",italic(y[m]),"]"))),
       lwd=c(2,2),lty=c(1,1,2),col=c("black","gray"),bty="n")




