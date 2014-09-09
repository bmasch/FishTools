#Program to calculate properties of phos estimates using Monte Carlo simulation and theoretical results
#This code treats the general case of inputs from several hatcheries with potentially different marking fractions
#and different fraction of marked fish given a coded-wire tag. 

#Variables and parameters used in the analysis
#inputs
#Nsims = total number of bootstrap replications
#Nhos = true hatchery origin spawning escapement escapement (hatchery-specific)
#Nnos = true natural origin spawning escapement
#theta = sampling fraction 
#lambda = marking rate (lambda)  (hatchery-specific)
#pcwt=fraction of marked fish that are also coded-wire tagged (hatchery-specific)
#
#
#intermediate variables
#phos = fraction of escapement that is of hatchery origin
#nhatch=number of hatcheries supplying spawners in the wild
#Ehatchsampled = Replications of number of hatchery-origin fish that are sampled
#Enatsampled = Replications of number of natural-origin fish that are sampled
#Em = Replications of number of marked spawners (hatchery-specific)
#Emcwt = Replications of number of marked and coded-wire tagged spawners (hatchery-specific)
#Eu = Replications of number of unmarked spawners
#Emtot= Replications of the total number of marked fish (summing over hatcheries)
#Nhoshat = Replications estimate of Nhos
#Ntothat = Replications of estimate of Ntot (totally number of spawners)


#output variables
#phos (true value) calculated from Nhos and Nnos
#phosi (true values) calculated from Nhos and Nhos (sum(phosi)=phos)
#SE.Nhoshat = standard error (SE) of Nhoshat
#CV.Nhoshat = Coefficient of variation of Nhoshat
#SE.Nnoshat = standard error (SE) of Nnoshat
#CV.Nnnoshat = Coefficient of variation of Nnoshat
#SE.phoshat = standard error (SE)
#CV.phoshat = Coefficient of variation
#BIAS.phoshat = relative bias
#the following use theoretical formulas
#SE2.Nhoshat = standard error (SE) of Nhoshat
#CV2.Nhoshat = Coefficient of variation of Nhoshat
#SE2.Nnoshat = standard error (SE) of Nnoshat
#CV2.Nnnoshat = Coefficient of variation of Nnoshat
#SE2.phoshat = standard error (SE) 
#CV2.phoshat = Coefficient of variation 

#uses Monte Carlo simulation for multiple hatcheries
#uses cwt ratios to help esimate fractions of
#unmarked fish from hatchery i

#Use Monte Carlo simulation for results
phos.mhatch.estimates1<-function(Nsims=10000,Nhos=c(100,100),Nnos=200,theta=0.25,
  lambda=c(0.75,.25),pcwt=c(.5,.9)){

#check dimension of inputs
k1<-length(Nhos);k2<-length(lambda);k3<-length(pcwt)
mytest<-abs(k1-k2)+abs(k2-k3)
if(mytest>0) stop("dimensions of Nhos, lambda, and pcwt must match")
nhatch<-length(Nhos)
#check lambdas (if they are all the same, the analysis simplifies)
if(nhatch==1){mytest==TRUE}
if(nhatch>1){mytest<-var(lambda)<1.e-10}
if(mytest){
#phis don’t matter at all – it’s as if there were a single hatchery
 res<-phos.estimates1(Nsims,Nhos=sum(Nhos),Nnos=Nnos,theta=theta,lambda=mean(lambda))
 phos=sum(Nhos)/(sum(Nhos)+Nnos)
 myres<-list(Nsims=Nsims,
		Nhos=Nhos,
		Nnos=Nnos,
		theta=theta,
		lambda=lambda,
		pcwt=pcwt,  
                phos=phos,
                SE.Nhoshat=res$SE.Nhoshat,
                CV.Nhoshat=res$CV.Nhoshat,
                SE.Nnoshat=res$SE.Nnoshat,
                CV.Nnoshat=res$CV.Nnoshat,              
		SE.phoshat=res$SE.phoshat,
		CV.phoshat=res$CV.phoshat,
		BIAS.phoshat=res$BIAS.phoshat)

 return(myres)
}

#check phis (must all exceed zero)
if(sum(pcwt==0))stop("phis must all be greater than zero")
phitest<-FALSE
if(sum(pcwt==1)==nhatch)phitest<-TRUE

 phos<-sum(Nhos)/(sum(Nhos)+Nnos)
#generate synthetic data sets
 Ehatchsampled<-matrix(NA,nrow=Nsims,ncol=nhatch)
 for(jj in 1:nhatch){
  Ehatchsampled[,jj] <-rbinom(Nsims,size=Nhos[jj],prob=theta)
 }
 Enatsampled <-rbinom(Nsims,size=Nnos,prob=theta)
 Em<-matrix(NA,nrow=Nsims,ncol=nhatch)
 Emcwt<-matrix(NA,nrow=Nsims,ncol=nhatch)

 for(ii in 1:Nsims){
  for(jj in 1:nhatch){
  Em[ii,jj]<-rbinom(1,size=Ehatchsampled[ii,jj],prob=lambda[jj])
  Emcwt[ii,jj]<-rbinom(1,size=Em[ii,jj],prob=pcwt[jj])
 }}

#total unmarked fish (summing over all hatcheries)
 Emtot<-apply(Em,c(1),sum)
 Eu<-apply(Ehatchsampled,c(1),sum)-Emtot+Enatsampled
 
 Nhoshat<-rep(NA,Nsims)
#Replications of estimates
if(!phitest){
 for(ii in 1:Nsims){
  Nhoshat[ii]<-get.nhoshat(x2=sum(Em[ii,]-Emcwt[ii,]),x1=Emcwt[ii,],theta,lambda,phi=pcwt)
}}else{ 
 for(ii in 1:Nsims){
  Nhoshat[ii]<- sum(Emcwt[ii,]/(theta*lambda))
}}
 
 Ntothat<-Eu*(1/theta)+Emtot*(1/theta)
 Nnoshat<-Ntothat-Nhoshat
 phoshat<-Nhoshat/Ntothat

#properties of phos estimator
SE.Nhoshat<-sqrt(var(Nhoshat,na.rm=T))
CV.Nhoshat<-SE.Nhoshat/sum(Nhos)
SE.Nnoshat<-sqrt(var(Nnoshat,na.rm=T))
CV.Nnoshat<-SE.Nnoshat/Nnos
SE.phoshat<-sqrt(var(phoshat,na.rm=T))
CV.phoshat<-SE.phoshat/phos
BIAS.phoshat<-(mean(phoshat,na.rm=T)-phos)/phos

myres<-list(Nsims=Nsims,
                   Nhos=Nhos,
                   Nnos=Nnos,
                   theta=theta,
                   lambda=lambda,
                   pcwt=pcwt,
                   phos=phos,
                   SE.Nhoshat=SE.Nhoshat,
                   CV.Nhoshat=CV.Nhoshat,
                   SE.Nnoshat=SE.Nnoshat,
                   CV.Nnoshat=CV.Nnoshat,
                   SE.phoshat=SE.phoshat,
                   CV.phoshat=CV.phoshat,
                   BIAS.phoshat=BIAS.phoshat)
 return(myres)
}

#Theoretical results
phos.mhatch.estimates2<-function(Nhos=c(100,100),Nnos=200,theta=0.25,
  lambda=c(0.75,.25),pcwt=c(.5,.9)){

#check dimension of inputs
k1<-length(Nhos);k2<-length(lambda);k3<-length(pcwt)
mytest<-abs(k1-k2)+abs(k2-k3)
if(mytest>0) stop("dimensions of Nhos, lambda, and pcwt must match")
nhatch<-length(Nhos)
#check lambdas (if they are all the same, the analysis simplifies)
if(nhatch==1){mytest==TRUE}
if(nhatch>1){mytest<-var(lambda)<1.e-10}
if(mytest){
#phis don’t matter at all – it’s as if there were a single hatchery
 res<-phos.estimates2(Nhos=sum(Nhos),
			Nnos=Nnos,
			theta=theta,
			lambda=mean(lambda))
 phos=sum(Nhos)/(sum(Nhos)+Nnos)
 myres<-list(Nhos=Nhos,
		Nnos=Nnos,
		theta=theta,
		lambda=lambda,
		pcwt=pcwt,
                phos=phos,
                SE2.Nhoshat=res$SE2.Nhoshat,
                CV2.Nhoshat=res$CV2.Nhoshat,
                SE2.Nnoshat=res$SE2.Nnoshat,
                CV2.Nnoshat=res$CV2.Nnoshat,
                SE2.phoshat=res$SE2.phoshat,
		CV2.phoshat=res$CV2.phoshat)
 return(myres)
}#mytest

#check phis (must all exceed zero)
if(sum(pcwt==0))stop("phis must all be greater than zero")
phitest<-FALSE
if(sum(pcwt==1)==nhatch)phitest<-TRUE
 phos<-sum(Nhos)/(sum(Nhos)+Nnos)

#theoretical formula for variance of phoshat
Ntot<-sum(Nhos)+Nnos
phosi<-Nhos/Ntot
if(!phitest){
 sum1<-sum(phosi*(1-theta*lambda*pcwt)/(theta*lambda*pcwt))
 sum2<-sum(phosi*(1-pcwt)/pcwt)
 sum3<-sum(phosi*(1-pcwt)*theta*lambda/pcwt)
 phos.var<-(1/Ntot)*(sum1-sum2*sum2/sum3-phos*phos*(1-theta)/theta)
 sum1<-sum(Nhos*(1-theta*lambda*pcwt)/(theta*lambda*pcwt))
 sum2<-sum(Nhos*(1-pcwt)/pcwt)
 sum3<-sum(Nhos*(1-pcwt)*theta*lambda/pcwt)
 Nhos.var<-sum1-sum2*sum2/sum3
 Nnos.var<-Ntot*(1-theta)/theta+Nhos.var-2*(1-theta)*sum(Nhos)/theta
}else{
 sum1<-sum(phosi*(1-theta*lambda)/(theta*lambda))
 phos.var<-(1/Ntot)*(sum1-phos*phos*(1-theta)/theta)
 Nhos.var<-sum(Nhos*(1-theta*lambda)/(theta*lambda))
 Nnos.var<-Ntot*(1-theta)/theta+Nhos.var-2*(1-theta)*sum(Nhos)/theta
}
 SE2.phoshat<-sqrt(phos.var)
 CV2.phoshat<-SE2.phoshat/phos
 SE2.Nhoshat<-sqrt(Nhos.var)
 CV2.Nhoshat<-SE2.Nhoshat/sum(Nhos)
 SE2.Nnoshat<-sqrt(Nnos.var)
 CV2.Nnoshat<-SE2.Nnoshat/Nnos

myres<-list(Nhos=Nhos,
                   Nnos=Nnos,
                   theta=theta,
                   lambda=lambda,
                   pcwt=pcwt,
                   phos=phos,
                   SE2.Nhoshat=SE2.Nhoshat,
                   CV2.Nhoshat=CV2.Nhoshat,
                   SE2.Nnoshat=SE2.Nnoshat,
                   CV2.Nnoshat=CV2.Nnoshat,
                   SE2.phoshat=SE2.phoshat,
                   CV2.phoshat=CV2.phoshat)
 return(myres)
}


#use iteration in x=g(x) method
#in general the estimate depends on the true values
#of escapement, so use iteration until the estimate converges
get.nhoshat<-function(x2,x1,theta,lambda,phi){
  etol<-1.e-5
  nhatch<-length(x1)
  Nhos0<-x1/(theta*lambda*phi)
  if(sum(c(x1,x2))<1.e-10)return(0.0)
  if((sum(x1)<1.e-10)&(x2>0))return(NA)
  run1<-sum(x1*(1-phi)/phi)
#initial guess
  Nhos<- Nhos0
  mynorm1<-sqrt(sum(Nhos*Nhos))
  err<-2.*etol*(mynorm1+etol)
  iter<-0
  while(err>etol*(mynorm1+etol)){
   rise<-Nhos*(1-phi)/(phi*theta)
   run<-sum(lambda*Nhos*(1-phi)/phi)
   Nhos<-Nhos0+(rise/run)*(x2-run1)
   mynorm2<-sqrt(sum(Nhos*Nhos))
   err<-abs(mynorm2-mynorm1)
   mynorm1<-mynorm2
   iter<-iter+1
   if(iter>100)stop("too many iterations in get.nhoshat")
  }
#  print(iter)
  return(sum(Nhos))
}


#special case where all lambdas are the same (Monte Carlo Results)
phos.estimates1<-function(Nsims=10000,Nhos=100,Nnos=100,theta=0.25,lambda=0.75)
{
 Ntot<-Nhos+Nnos
 phos<-Nhos/Ntot
 Ehatchsampled <-rbinom(Nsims,size=Nhos,prob=theta)
 Enatsampled <-rbinom(Nsims,size=Nnos,prob=theta)
 Em<-rep(NA,Nsims)
 for(ii in 1:Nsims){
  Em[ii]<-rbinom(1,size=Ehatchsampled[ii],prob=lambda)
 }
 Eu<-Ehatchsampled-Em+Enatsampled

 Nhoshat<-Em*(1/theta)*(1/lambda)
 Ntothat<-Eu*(1/theta)+Em*(1/theta)
 Nnoshat<-Ntothat-Nhoshat
 phoshat<-Nhoshat/Ntothat
 SE.Nhoshat<-sqrt(var(Nhoshat,na.rm=T))
 CV.Nhoshat<-SE.Nhoshat/Nhos
 SE.Nnoshat<-sqrt(var(Nnoshat,na.rm=T))
 CV.Nnoshat<-SE.Nnoshat/Nnos
 SE.phoshat<-sqrt(var(phoshat,na.rm=T))
 CV.phoshat<-SE.phoshat/phos
 BIAS.phoshat<-(mean(phoshat,na.rm=T)-phos)/phos

 myres<-list(Nsims=Nsims,
                   Nhos=Nhos,
                   Nnos=Nnos,
                   theta=theta,
                   lambda=lambda,
                   phos=phos,
                   SE.Nhoshat=SE.Nhoshat,
                   CV.Nhoshat=CV.Nhoshat,
                   SE.Nnoshat=SE.Nnoshat,
                   CV.Nnoshat=CV.Nnoshat,
                   SE.phoshat=SE.phoshat,
                   CV.phoshat=CV.phoshat,
                   BIAS.phoshat=BIAS.phoshat)
		   
 return(myres)
 }

#special case where all lambdas are the same (theoretical results)
phos.estimates2<-function(Nhos=100,Nnos=100,theta=0.25,lambda=0.75)
{
 Ntot<-Nhos+Nnos
 phos<-Nhos/Ntot
 var.Nhoshat<-Nhos*(1-lambda*theta)/(lambda*theta)
 var.Nnoshat<-Nnos*(1-theta)/theta+Nhos*(1-lambda)/(theta*lambda)
 var.phos<-(phos/Ntot)*((1-lambda*theta)/(lambda*theta)-phos*(1-theta)/theta)
 SE2.Nhoshat<-sqrt(var.Nhoshat)
 CV2.Nhoshat<-SE2.Nhoshat/Nhos
 SE2.Nnoshat<-sqrt(var.Nnoshat)
 CV2.Nnoshat<-SE2.Nnoshat/Nnos
 SE2.phoshat<-sqrt(var.phos)
 CV2.phoshat<-SE2.phoshat/phos

 myres<-list(Nhos=Nhos,
                   Nnos=Nnos,
                   theta=theta,
                   lambda=lambda,
                   phos=phos,
                   SE2.Nhoshat=SE2.Nhoshat,
                   CV2.Nhoshat=CV2.Nhoshat,
                   SE2.Nnoshat=SE2.Nnoshat,
                   CV2.Nnoshat=CV2.Nnoshat,
                   SE2.phoshat=SE2.phoshat,
                   CV2.phoshat=CV2.phoshat)
		   
 return(myres)
}