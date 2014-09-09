#HATCHMARK2E Program to estimate proportion of hatchery-origin escapement using
#generalized least squares (Hinrichsen et al. 2011). Precision results are obtained using
#Bootstrapping or theoretical results
#
#This code treats the general case of inputs from several source hatcheries with potentially 
#different visual marking fractions (VM fractions). 

#Variables and parameters used in the analysis
#inputs
#Nsims = total number of bootstrap replications
#x2 = visibly marked and not coded-wire tagged observation
#x1 = visibly marked and coded-wire tagged observations (hatchery-specific)
#Eu = Number of spawners in the sample that are not visibly marked
#theta = sampling fraction 
#lambda = marking rate (lambda)  (hatchery-specific)
#phi=fraction of marked fish that are also coded-wire tagged (hatchery-specific)
#
#output variables
#Nnos = GLSE estimate of natural origin spawning escapement
#Nhos = GLSE estimate of hatchery origin spawning escapement escapement (hatchery-specific)
#phos = GLSE estimate of the proportion of hatchery-origin spawning escapement
#SE.Nnoshat = standard error (SE) of Nnoshat
#CV.Nnoshat = Coefficient of variation of Nnoshat
#SE.Nhoshat = standard error (SE) of Nhoshat
#CV.Nhoshat = Coefficient of variation of Nhoshat
#SE.phoshat = standard error (SE) of the phos estimator
#CV.phoshat = Coefficient of variation of the phos estimator
#BIAS.phoshat = relative bias of the phos estimator

#the following use theoretical formulas

#SE2.Nnoshat = standard error (SE) of Nnoshat
#CV2.Nnoshat = Coefficient of variation of Nnoshat
#SE2.Nhoshat = standard error (SE) of Nhoshat
#CV2.Nhoshat = Coefficient of variation of Nhoshat
#SE2.phoshat = standard error (SE) 
#CV2.phoshat = Coefficient of variation 

#use bootstrapping for variance and bias
phos.mhatch.main1<-function(Nsims=10000,x1=c(40,23),x2=40,Eu=200,theta=0.25,
lambda=c(0.75,0.25),phi=c(.5,.9)){

#check inputs
k1<-length(x1);k2<-length(lambda);k3<-length(phi)
mytest<-abs(k1-k2)+abs(k2-k3)
if(mytest>0) stop("dimensions of x1, lambda, and phi must match")
nhatch<-length(x1)

if(theta>1)stop("theta must be less than or equal to one")
if(theta<=0)stop("theta must be greater than zero")
if(sum(lambda>1))stop("lambdas must all be less than one")
if(sum(lambda<=0))stop("lambdas must all be greater than zero")
if(sum(phi>1))stop("phis must all be less than one")
if(sum(phi<=0))stop("phis must all be greater than zero")


#check lambdas (if they are all the same, the analysis simplifies)
lambdatest<-FALSE
if(nhatch==1){lambdatest==TRUE}
if(nhatch>1){lambdatest<-var(lambda)<1.e-10}

if(lambdatest){
  Nhos<-(sum(x1)+x2)/(theta*lambda[1])
  Nnos<-(sum(x1)+x2+Eu)/theta - Nhos
  res1<-phos.estimates1(Nsims=Nsims, Nnos=Nnos,Nhos=Nhos,theta=theta,lambda=lambda[1])
}else{
if((sum(abs(x1))<1.e-10)&(x2>0))stop("Nhos unestimable because lambdas differ and x1=0 and x2>0")
phitest<-FALSE
if(sum(phi==1)==nhatch)phitest<-TRUE
if(!phitest){
Nhos<-get.nhoshat.all(x1=x1,x2=x2,theta=theta,lambda=lambda,phi=phi)
}else{
 if(x2>0)stop("Error: All phis are one and x2 is greater than zero")
 Nhos<-x1/(theta*lambda)
}
Nnos<-(sum(x1)+x2+Eu)/theta - sum(Nhos)

res1<-phos.mhatch.estimates1(Nsims=Nsims,Nnos=Nnos,Nhos=Nhos,theta=theta,
	lambda=lambda,phi=phi)
}

return(list(Nsims=res1$Nsims,
              x1=x1,
	      x2=x2,
	      Eu=Eu,
              theta=theta,
	      lambda=lambda,
	      phi=phi,
	      Nnos=res1$Nnos,
	      Nhos=sum(res1$Nhos),
	      phos=res1$phos,
              SE.Nnoshat=res1$SE.Nnoshat,
              CV.Nnoshat=res1$CV.Nnoshat,
              SE.Nhoshat=res1$SE.Nhoshat,
              CV.Nhoshat=res1$CV.Nhoshat,
              SE.phoshat=res1$SE.phoshat,
              CV.phoshat=res1$CV.phoshat,
              BIAS.phoshat=res1$BIAS.phoshat))

}

#use theoretical variances formulas
phos.mhatch.main2<-function(x1=c(40,23),x2=40,Eu=200,theta=0.25,
lambda=c(0.75,0.25),phi=c(.5,.9)){

#check inputs
k1<-length(x1);k2<-length(lambda);k3<-length(phi)
mytest<-abs(k1-k2)+abs(k2-k3)
if(mytest>0) stop("dimensions of x1, lambda, and phi must match")
nhatch<-length(x1)

if(theta>1)stop("theta must be less than or equal to one")
if(theta<=0)stop("theta must be greater than zero")
if(sum(lambda>1))stop("lambdas must all be less than one")
if(sum(lambda<=0))stop("lambdas must all be greater than zero")
if(sum(phi>1))stop("phis must all be less than one")
if(sum(phi<=0))stop("phis must all be greater than zero")


#check lambdas (if they are all the same, the analysis simplifies)
lambdatest<-FALSE
if(nhatch==1){lambdatest==TRUE}
if(nhatch>1){lambdatest<-var(lambda)<1.e-10}

if(lambdatest){
  Nhos<-(sum(x1)+x2)/(theta*lambda[1])
  Nnos<-(sum(x1)+x2+Eu)/theta - Nhos
  res2<-phos.estimates2(Nnos=Nnos,Nhos=Nhos,theta=theta,lambda=lambda[1])
}else{
if((sum(abs(x1))<1.e-10)&(x2>0))stop("Nhos unestimable because lambdas differ and x1=0 and x2>0")
phitest<-FALSE
if(sum(phi==1)==nhatch)phitest<-TRUE
if(!phitest){
Nhos<-get.nhoshat.all(x1=x1,x2=x2,theta=theta,lambda=lambda,phi=phi)
}else{
 if(x2>0)stop("Error: All phis are one and x2 is greater than zero")
 Nhos<-x1/(theta*lambda)
}

Nnos<-(sum(x1)+x2+Eu)/theta - sum(Nhos)
res2<-phos.mhatch.estimates2(Nnos=Nnos,Nhos=Nhos,theta=theta,
	lambda=lambda,phi=phi)
}

return(list(Nsims=NA,
            x1=x1,
	    x2=x2,
            Eu=Eu,
            theta=theta,
	    lambda=lambda,
            phi=phi,   
	    Nnos=res2$Nnos,
            Nhos=sum(res2$Nhos),
	    phos=res2$phos,
            SE2.Nnoshat=res2$SE2.Nnoshat,
            CV2.Nnoshat=res2$CV2.Nnoshat,
            SE2.Nhoshat=res2$SE2.Nhoshat,
            CV2.Nhoshat=res2$CV2.Nhoshat,
            SE2.phoshat=res2$SE2.phoshat,
            CV2.phoshat=res2$CV2.phoshat,
            BIAS2.phoshat=NA))
                
}

#uses Bootstrapping for multiple hatcheries
#uses cwt ratios to help esimate fractions of
#unmarked fish from hatchery i

#Use Bootstrapping for results
phos.mhatch.estimates1<-function(Nsims=10000,Nnos=200,Nhos=c(100,100),theta=0.25,
  lambda=c(0.75,.25),phi=c(.5,.9)){

#check dimension of inputs
k1<-length(Nhos);k2<-length(lambda);k3<-length(phi)
mytest<-abs(k1-k2)+abs(k2-k3)
if(mytest>0) stop("dimensions of Nhos, lambda, and phi must match")
nhatch<-length(Nhos)

#check inputs
if(sum(Nhos<0))stop("An Nhos estimate is negative")
if(Nnos<0)stop("Nnos estimate is negative")

#check lambdas (if they are all the same, the analysis simplifies)
mytest<-FALSE
if(nhatch==1){mytest==TRUE}
if(nhatch>1){mytest<-var(lambda)<1.e-10}
if(mytest){
#phis don’t matter at all – it’s as if there were a single hatchery
 res<-phos.estimates1(Nsims,Nnos=Nnos,Nhos=sum(Nhos),theta=theta,lambda=mean(lambda))
 phos=sum(Nhos)/(sum(Nhos)+Nnos)
 myres<-list(Nsims=Nsims,
		Nnos=Nnos,
		Nhos=Nhos,
		theta=theta,
		lambda=lambda,
		phi=phi,  
                phos=phos,
                SE.Nnoshat=res$SE.Nnoshat,
                CV.Nnoshat=res$CV.Nnoshat,
                SE.Nhoshat=res$SE.Nhoshat,
                CV.Nhoshat=res$CV.Nhoshat,              
		SE.phoshat=res$SE.phoshat,
		CV.phoshat=res$CV.phoshat,
		BIAS.phoshat=res$BIAS.phoshat)

 return(myres)
}

#check phis (must all exceed zero)
if(sum(phi==0))stop("phis must all be greater than zero")
phitest<-FALSE
if(sum(phi==1)==nhatch)phitest<-TRUE

 phos<-sum(Nhos)/(sum(Nhos)+Nnos)
#generate synthetic data sets
 Ehatchsampled<-matrix(NA,nrow=Nsims,ncol=nhatch)
 for(jj in 1:nhatch){
  Ehatchsampled[,jj] <-rbinom(Nsims,size=round(Nhos[jj]),prob=theta)
 }
 Enatsampled <-rbinom(Nsims,size=round(Nnos),prob=theta)
 Em<-matrix(NA,nrow=Nsims,ncol=nhatch)
 Emcwt<-matrix(NA,nrow=Nsims,ncol=nhatch)

 for(ii in 1:Nsims){
  for(jj in 1:nhatch){
  Em[ii,jj]<-rbinom(1,size=Ehatchsampled[ii,jj],prob=lambda[jj])
  Emcwt[ii,jj]<-rbinom(1,size=Em[ii,jj],prob=phi[jj])
 }}

#total unmarked fish (summing over all hatcheries)
 Emtot<-apply(Em,c(1),sum)
 Eu<-apply(Ehatchsampled,c(1),sum)-Emtot+Enatsampled
 
 Nhoshat<-rep(NA,Nsims)
#Replications of estimates
if(!phitest){
 for(ii in 1:Nsims){
  Nhoshat[ii]<-get.nhoshat(x1=Emcwt[ii,],x2=sum(Em[ii,]-Emcwt[ii,]),theta,lambda,phi=phi)
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
                   Nnos=Nnos,
                   Nhos=Nhos,
                   theta=theta,
                   lambda=lambda,
                   phi=phi,
                   phos=phos,
                   SE.Nnoshat=SE.Nnoshat,
                   CV.Nnoshat=CV.Nnoshat,
                   SE.Nhoshat=SE.Nhoshat,
                   CV.Nhoshat=CV.Nhoshat,
                   SE.phoshat=SE.phoshat,
                   CV.phoshat=CV.phoshat,
                   BIAS.phoshat=BIAS.phoshat)
 return(myres)
}

#Theoretical results
phos.mhatch.estimates2<-function(Nnos=200,Nhos=c(100,100),theta=0.25,
  lambda=c(0.75,.25),phi=c(.5,.9)){

#check dimension of inputs
k1<-length(Nhos);k2<-length(lambda);k3<-length(phi)
mytest<-abs(k1-k2)+abs(k2-k3)
if(mytest>0) stop("dimensions of Nhos, lambda, and phi must match")
nhatch<-length(Nhos)

#check inputs
if(sum(Nhos<0))stop("An Nhos estimate is negative")
if(Nnos<0)stop("Nnos estimate is negative")

#check lambdas (if they are all the same, the analysis simplifies)
if(nhatch==1){mytest==TRUE}
if(nhatch>1){mytest<-var(lambda)<1.e-10}
if(mytest){
#phis don’t matter at all – it’s as if there were a single hatchery
 res<-phos.estimates2(Nnos=Nnos,
                        Nhos=sum(Nhos),
			theta=theta,
			lambda=mean(lambda))
 phos=sum(Nhos)/(sum(Nhos)+Nnos)
 myres<-list(Nnos=Nnos,
             Nhos=Nhos,
		theta=theta,
		lambda=lambda,
		phi=phi,
                phos=phos,
                SE2.Nnoshat=res$SE2.Nnoshat,
                CV2.Nnoshat=res$CV2.Nnoshat,
                SE2.Nhoshat=res$SE2.Nhoshat,
                CV2.Nhoshat=res$CV2.Nhoshat,
                SE2.phoshat=res$SE2.phoshat,
		CV2.phoshat=res$CV2.phoshat)
 return(myres)
}#mytest

#check phis (must all exceed zero)
if(sum(phi==0))stop("phis must all be greater than zero")
phitest<-FALSE
if(sum(phi==1)==nhatch)phitest<-TRUE
 phos<-sum(Nhos)/(sum(Nhos)+Nnos)

#theoretical formula for variance of phoshat
Ntot<-sum(Nhos)+Nnos
phosi<-Nhos/Ntot
if(!phitest){
 sum1<-sum(phosi*(1-theta*lambda*phi)/(theta*lambda*phi))
 sum2<-sum(phosi*(1-phi)/phi)
 sum3<-sum(phosi*(1-phi)*theta*lambda/phi)
 phos.var<-(1/Ntot)*(sum1-sum2*sum2/sum3-phos*phos*(1-theta)/theta)
 sum1<-sum(Nhos*(1-theta*lambda*phi)/(theta*lambda*phi))
 sum2<-sum(Nhos*(1-phi)/phi)
 sum3<-sum(Nhos*(1-phi)*theta*lambda/phi)
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

myres<-list(Nnos=Nnos,
                   Nhos=Nhos,
                   theta=theta,
                   lambda=lambda,
                   phi=phi,
                   phos=phos,
                   SE2.Nnoshat=SE2.Nnoshat,
                   CV2.Nnoshat=CV2.Nnoshat,
                   SE2.Nhoshat=SE2.Nhoshat,
                   CV2.Nhoshat=CV2.Nhoshat,
                   SE2.phoshat=SE2.phoshat,
                   CV2.phoshat=CV2.phoshat)
 return(myres)
}





#special case where all lambdas are the same (Bootstrapping Results)
phos.estimates1<-function(Nsims=10000,Nnos=100,Nhos=100,theta=0.25,lambda=0.75)
{
 Ntot<-Nhos+Nnos
 phos<-Nhos/Ntot
 Ehatchsampled <-rbinom(Nsims,size=round(Nhos),prob=theta)
 Enatsampled <-rbinom(Nsims,size=round(Nnos),prob=theta)
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
                   Nnos=Nnos,
                   Nhos=Nhos,
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
phos.estimates2<-function(Nnos=100,Nhos=100,theta=0.25,lambda=0.75)
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

 myres<-list(Nnos=Nnos,
                   Nhos=Nhos,
                   theta=theta,
                   lambda=lambda,
                   phos=phos,
                   SE2.Nnoshat=SE2.Nnoshat,
                   CV2.Nnoshat=CV2.Nnoshat,
                   SE2.Nhoshat=SE2.Nhoshat,
                   CV2.Nhoshat=CV2.Nhoshat,
                   SE2.phoshat=SE2.phoshat,
                   CV2.phoshat=CV2.phoshat)
		   
 return(myres)
}


#In general the estimate depends on the true values
#of escapement, so use iteration until the estimate converges
#Use fixed point iteration to get the GLSE
get.nhoshat<-function(x1,x2,theta,lambda,phi){
  etol<-1.e-10
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

#
get.nhoshat.all<-function(x1,x2,theta,lambda,phi){
  etol<-1.e-10
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
  return(Nhos)
}