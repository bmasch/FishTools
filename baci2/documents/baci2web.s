#Program to estimate power of a baci design
#when the variance-covariance matrix is unknown. Variance is estimated
#along with the treatment effect and the control population mean
#The estimated variance-covariance matrix has the
#form of an intraclass covariance matrix (a common variance and a common
#covariance)
#Baci code using Monte Carlo estimates of power
#AUTHOR: Richard A. Hinrichsen
#CONTACT: rich@hinrichsenenvironmental.com
#DATE REVISED: 10/14/2013
#Nsim number of Monte Carlo simulations
#s2 is variances (assumed equal for all populations)
#rho is correlation between each pair of populations
#n1 number of before years
#n2 number of after years
#k1 number of control populations
#k2 number of treatment populations
#me measurement error
#alpha probability of a type I error (rejecting null hypothesis when true)
#delta true treatment effect representing difference in natural log survival rate, #ln(Streatment/Scontrol)
#the intraclass covariance matrix structure is assumed.
#outputs
#ngood is the number of simulations that result in a valid estimate
#se standard error
#cv coefficient of variation
#power is the probability of rejecting the null hypothesis of no effect when it is false
library(MASS)
baci2 <- function(Nsim=1000,s2=1,rho=.9,n1=5,n2=5,k1=1,k2=1,me=log(1.10),alpha=0.05,
delta=log(1.50)){
check.inputs(Nsim,s2,rho,n1,n2,k1,k2,me,alpha,delta)
k <- k1+k2
SIG2<-matrix(s2*rho,ncol=k,nrow=k)
diag(SIG2)<-s2+me*me
INVSIG2<-solve(SIG2)
deltas<-rep(NA,Nsim)
ses<-rep(NA,Nsim)
#Do Monte Carlo simulations to get replications of delta and se
for(ii in 1:Nsim){
bres<-baci.estimates(s2=s2,rho=rho,n1=n1,n2=n2,k1=k1,k2=k2,me=me,
alpha=alpha,delta=delta)
if(!is.null(bres)){
deltas[ii]<-bres$par[2]
ses[ii]<-get.se(bres$SIG2,n1=n1,n2=n2,k1=k1)
}
}
se<-sqrt(var(deltas,na.rm=T))
#get critical value of distribution under null hypothesis
crit2<-quantile(x=(deltas-delta)/ses,probs=c(alpha/2,1-alpha/2),na.rm=T)
crit<-mean(abs(crit2))
powerx<-abs(deltas/ses)>crit
power<-mean(powerx,na.rm=T)
ngood<-sum(!is.na(deltas/ses))
return(list(Nsim=Nsim,ngood=ngood,s2=s2,rho=rho,n1=n1,n2=n2,k1=k1,k2=k2,me=me,
alpha=alpha,delta=delta,se=se,cv=se/delta,power=power))
}
#make sure the inputs make sense
check.inputs<-function(Nsim,s2,rho,n1,n2,k1,k2,me,alpha,delta){
 if(Nsim<10)stop("Number of simulations, Nsim, must be at least 10")
 if(round(Nsim)!=Nsim)stop("Number of simulations, Nsim, must be an integer")
 if(s2<0)stop("Variance, s2, must be nonnegative")
 if(me<0)stop("Measurement error standard deviation, me, must be nonnegative")
 if((s2+me*me)<=0)stop("Total variance, s2+me*me, must be positive")
 if(abs(rho)>1)stop("Correlation coefficient, rho, must be between -1 and 1")
 if(n1<=0)stop("Number of before years,n1, must be positive")
 if(round(n1)!=n1)stop("Number of before years,n1, must be an integer")
 if(n2<=0)stop("Number of after years, n2, must be positive")
 if(round(n2)!=n2)stop("Number of after years, n2, must be an integer")
 if(k1<=0)stop("Number of control populations, k1, must be positive")
 if(round(k1)!=k1)stop("Number of control populations, k1, must be an integer")
 if(k2<=0)stop("Number of treatment populations, k2, must be positive")
 if(round(k2)!=k2)stop("Number of treatment populations, k2, must be an integer")
 if((alpha<=0)|(alpha>=1))stop("Prob. of type I error, alpha, must be between 0 and 1")
 if(!is.double(delta))stop("The treatment effect, delta, must be a real number")
 return(NULL)
}
baci.estimates<-function(s2=1,rho=.9,n1=5,n2=5,k1=1,k2=1,me=log(1.10),alpha=0.05,delta=log(1.50)){
n<-n1+n2
k<-k1+k2
par<-c(log(.10),delta)
SIG2<-matrix(s2*rho,ncol=k,nrow=k)
diag(SIG2)<- s2+me*me
xmat1<-mvrnorm(n=n1,mu=rep(par[1],k),Sigma=SIG2)
xmat2<-mvrnorm(n=n2,mu=c(rep(par[1],k1),rep(par[1]+par[2],k2)),Sigma=SIG2)
xmat<-cbind(t(xmat1),t(xmat2))
res2<-myoptim2(xmat=xmat,s2=s2,rho=rho,me=me,n1=n1,k1=k1)
return(res2)
}
#Iterate until maximum likelihood estimates are obtained
#solving the estimating equations which were
#determined by setting the partial derivatives of the
#likelihood function to zero.
myoptim2<-function(xmat,s2,rho,me,n1,k1){
k<-dim(xmat)[1]
n<-dim(xmat)[2]
#begin with OLS regression estimates of theta parameters
SIG2<-diag(1,k)
par<-get.pars(xmat,SIG2,n1,k1)
SIG2<-get.SIG2(par,xmat,n1,k1)
#check condition number of SIG2
cn<-kappa(SIG2)
if((1/cn)<=1.e-15){
warning("SIG2 is computationally singular in myoptim2")
return(NULL)}
lf1<-lf(par=par,x=xmat,n1=n1,k1=k1,SIG2)
etol<-1.e-5
err<-2.*etol*(abs(lf1)+etol)
iter<-0
#look for relative likelihood function convergence
while(err>etol*(abs(lf1)+etol)){
par<-get.pars(xmat,SIG2,n1,k1)
SIG2<-get.SIG2(par,xmat,n1,k1)
#check condition number of SIG2
cn<-kappa(SIG2)
if((1/cn)<=1.e-15){
warning("SIG2 is computationally singular in myoptim2")
return(NULL)}
lf2<-lf(par=par,x=xmat,n1=n1,k1=k1,SIG2)
err<-abs(lf2-lf1)
lf1<-lf2
iter<-iter+1
if(iter>1000){
warning("too many iterations in myoptim2")
return(NULL)}
}
return(list(par=par,SIG2=SIG2))
}
#Use estimating equations to solve for parameter values
#Returns control mean (mu) and treatment effect (delta)
get.pars<-function(xmat,SIG2,n1,k1){
n<-dim(xmat)[2]
n2<-n-n1
k<-dim(xmat)[1]
E<-rep(1,k)
E1<-c(rep(1,k1),rep(0,k-k1))
E2<-c(rep(0,k1),rep(1,k-k1))
xbar2<-apply(xmat[,(n1+1):n],c(1),mean)
xbar<-apply(xmat,c(1),mean)
INVSIG2<-solve(SIG2)
delta<-(t(E2)%*%INVSIG2%*%xbar2)*(t(E)%*%INVSIG2%*%E)-t(E)%*%INVSIG2%*%xbar*(t(E2)%*%INVSIG2%*%E)
den<-(t(E)%*%INVSIG2%*%E)*(t(E2)%*%INVSIG2%*%E2)-(n2/n)*(t(E2)%*%INVSIG2%*%E)^2
delta<-delta/den
mu<-t(E2)%*%INVSIG2%*%xbar2-(t(E2)%*%INVSIG2%*%E2)*delta
den<-t(E2)%*%INVSIG2%*%E
mu<-mu/den
return(c(mu,delta))
}
#log-likelihood function
lf<-function(par,x,n1,k1,SIG2){
INVSIG2<-solve(SIG2)
n<-dim(x)[2]
k<-dim(x)[1]
z<-x
like<--k*.5*n*log(2*pi)-.5*n*log(det(SIG2))
for(ii in 1:n1){
z[,ii]<-x[,ii]-rep(par[1],k)
like<-like-.5*t(z[,ii])%*%INVSIG2%*%z[,ii]
}
for(ii in (n1+1):n){
z[,ii]<-x[,ii]-c(rep(par[1],k1),rep(par[1]+par[2],k-k1))
like<-like-.5*t(z[,ii])%*%INVSIG2%*%z[,ii]
}
return(like)
}
#Get the estimate variance-covariance matrix
#This is based on the estimating equations
#variance is unknown and has the
#form of an intraclass covariance matrix
get.SIG2<-function(par,x,n1,k1){
iform<-1
n<-dim(x)[2]
k<-dim(x)[1]
z<-x
SIG2<-matrix(0,ncol=k,nrow=k)
for(ii in 1:n1){
z[,ii]<-x[,ii]-rep(par[1],k)
SIG2<-SIG2+z[,ii]%*%t(z[,ii])/n
}
for(ii in (n1+1):n){
z[,ii]<-x[,ii]-c(rep(par[1],k1),rep(par[1]+par[2],k-k1))
SIG2<-SIG2+z[,ii]%*%t(z[,ii])/n
}
if(iform==1){
s2<-mean(diag(SIG2))
s12<-(sum(SIG2)-sum(diag(SIG2)))/(k*k-k)
SIG2<-matrix(s12,ncol=k,nrow=k)
diag(SIG2)<-s2
}
return(SIG2)
}
#Return se estimate based on SIG2
#This is a theortical formula
#derived in the paper
get.se<-function(SIG2,n1=5,n2=5,k1=1){
k<-dim(SIG2)[1]
k2<-k-k1
INVSIG2<-solve(SIG2)
e<-rep(1,k)
se<-(n1+n2)*t(e)%*%INVSIG2%*%e
e1<-c(rep(1,k1),rep(0,k2))
e2<-c(rep(0,k1),rep(1,k2))
det<-n1*t(e)%*%INVSIG2%*%e+n2*t(e1)%*%INVSIG2%*%e1
det<-det*n2*t(e2)%*%INVSIG2%*%e2-n2*n2*(t(e2)%*%INVSIG2%*%e1)^2
se<-sqrt(se/det)
return(se)
}
