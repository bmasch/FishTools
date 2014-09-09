#Program to calculate power of an experiment aimed at estimating the relative reproductive success (RRS)
#of hatchery-origin spawners
#AUTHOR: Richard A. Hinrichsen, Ph.D.
#DATE: 3-6-2014
#FILE: rrs-3-6-2014.s
#input variables
#Sw number of wild-origin spawning females
#Sh number of hatchery-origin spawning females
#n is the sample size of progeny
#delta is log(RRS)

#top level function
rrs.main<-function(Sw=200,Sh=200,n=800,alpha=0.05,delta=log(.8),MONTE=FALSE,NSIM=1000){
check.inputs(Sw,Sh,n,alpha,delta,MONTE,NSIM)
if(!MONTE){res<-rrs(Sw=Sw,Sh=Sh,n=n,alpha=alpha,delta=delta)}
if(MONTE){res<-rrs2(Sw=Sw,Sh=Sh,n=n,alpha=alpha,delta=delta,NSIM)}
final.res<-list(MONTE=res$MONTE,
 NSIM=res$NSIM,
 Sw=res$Sw,
 Sh=res$Sh,
 n=res$n,
 alpha=res$alpha,
 delta=res$delta,
 SE.delta=res$SE.delta,
 CV.delta=res$CV.delta,
 BIAS.delta=res$BIAS.delta,
 power=res$power)
return(final.res)
}

#check that inputs are valid
check.inputs<-function(Sw,Sh,n,alpha,delta,MONTE,NSIM){
 if(!is.logical(MONTE))stop("MONTE must be TRUE or FALSE")
 if(MONTE){
  if(floor(NSIM)!=NSIM){stop("NSIM must be a positive integer")}
  if(NSIM<=0){stop("NSIM must be a positive integer")}}
 if(floor(n)!=n){stop("n must be a positive integer")}
 if(n<=0){stop("n must be a positive integer")}
 if(floor(Sw)!=Sw){stop("Sw must be a positive integer")}
 if(Sw<=0){stop("Sw must be a positive integer")}
 if(floor(Sh)!=Sh){stop("Sh must be a positive integer")}
 if(Sh<=0){stop("Sh must be a positive integer")}
 if(alpha<=0){stop("alpha must be between zero and 1.0")}
 if(alpha>=1){stop("alpha must be between zero and 1.0")}
 if(!is.double(alpha)){stop("alpha must be a double")}
 return(NULL)
}

#This uses theoretical formulas from Hinrichsen (2003)
rrs<-function(Sw=200,Sh=200,n=800,alpha=0.05,delta=log(.8)){
 theta<-exp(delta)
 q<-qnorm(1-alpha/2)
 thetavar<-(theta*(Sw+Sh*theta)^2)/(n*Sh*Sw)
 deltavar<-thetavar/(theta*theta)
 se<-sqrt(deltavar)
 power<-(1-pnorm(q*se,mean=delta,sd=se))+pnorm(-q*se,mean=delta,sd=se)
 myres<-list(MONTE=FALSE,
  NSIM=NA,
  Sw=Sw,
  Sh=Sh,
  n=n,
  alpha=alpha,
  delta=delta,
  SE.delta=se,
  CV.delta=se/delta,
  BIAS.delta=NA,
  power=power)
 return(myres)
}

#return MLE of delta and its SE
get.estimate<-function(Sw,Sh,n,Rw){
 theta<-Sw*(n-Rw)/(Rw*Sh)
 delta<-log(theta)
 thetavar<-(theta*(Sw+Sh*theta)^2)/(n*Sh*Sw)
 deltavar<-thetavar/(theta*theta)
 se<-sqrt(deltavar)

 return(list(delta=delta,se=se))
}

#calculate SE and statistical power using Monte Carlo simulation
rrs2<-function(NSIM=1000,Sw=200,Sh=200,n=800,alpha=0.05,delta=log(.8)){
 theta<-exp(delta)
 q<-qnorm(1-alpha/2)
 prob<-Sw/(Sw+Sh*theta)
 Rw<-rbinom(n=NSIM,size=n,prob=prob)
 res<-get.estimate(Sw,Sh,n,Rw)
 deltas<-res$delta
 ses<-res$se
 power<-abs(deltas/ses)>q
 power<-sum(power)/NSIM
 se<-sqrt(var(deltas,na.rm=T))
 mymean<-mean(deltas,na.rm=T)
 BIAS.delta<-(mymean-delta)/delta
 myres<-list(MONTE=TRUE,
  NSIM=NSIM,
  Sw=Sw,
  Sh=Sh,
  n=n,
  alpha=alpha,
  delta=delta,
  SE.delta=se,
  CV.delta=se/delta,
  BIAS.delta=BIAS.delta,
  power=power)
 return(myres)
}