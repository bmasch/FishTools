#Program to estimate the relative reproductive success (RRS)
#of hatchery-origin spawners
#AUTHOR: Richard A. Hinrichsen, Ph.D.
#DATE: 9-29-2014
#FILE: rrse-9-29-2014.s
#input variables
#Sw number of wild-origin spawning females
#Sh number of hatchery-origin spawning females
#nw is the number of progeny sampled that have a wild-origin female parent
#nh is the number of progeny sampled that have a hatchery-origin female parent
#delta is log(RRS)

#top level function
rrse.main<-function(Sw=200,Sh=200,nw=444,nh=356,BOOT=FALSE,NSIM=1000){
check.inputs(Sw,Sh,nw,nh,BOOT,NSIM)
if(!BOOT){res<-rrse(Sw=Sw,Sh=Sh,nw=nw,nh=nh)}
if(BOOT){res<-rrse2(Sw=Sw,Sh=Sh,nw=nw,nh=nh,NSIM)}
final.res<-list(BOOT=res$BOOT,
 NSIM=res$NSIM,
 Sw=res$Sw,
 Sh=res$Sh,
 nw=res$nw,
 nh=res$nh,
 delta=res$delta,
 SE.delta=res$SE.delta,
 CV.delta=res$CV.delta,
 BIAS.delta=res$BIAS.delta)
return(final.res)
}

#check that inputs are valid
check.inputs<-function(Sw,Sh,nw,nh,BOOT,NSIM){
 if(!is.logical(BOOT))stop("BOOT must be TRUE or FALSE")
 if(BOOT){
  if(floor(NSIM)!=NSIM){stop("NSIM must be a positive integer")}
  if(NSIM<=0){stop("NSIM must be a positive integer")}}
 if(floor(nw)!=nw){stop("nw must be a positive integer")}
 if(nw<=0){stop("nw must be a positive integer")}
 if(floor(nh)!=nh){stop("nh must be a positive integer")}
 if(nh<=0){stop("nh must be a positive integer")}
 if(floor(Sw)!=Sw){stop("Sw must be a positive integer")}
 if(Sw<=0){stop("Sw must be a positive integer")}
 if(floor(Sh)!=Sh){stop("Sh must be a positive integer")}
 if(Sh<=0){stop("Sh must be a positive integer")}
 return(NULL)
}

#This uses theoretical formulas from Hinrichsen (2003)
rrse<-function(Sw=200,Sh=200,nw=444,nh=356){
 theta<-nh*Sw/(Sh*nw)
 n<-nh+nw
 thetavar<-(theta*(Sw+Sh*theta)^2)/(n*Sh*Sw)
 deltavar<-thetavar/(theta*theta)
 se<-sqrt(deltavar)
 delta<-log(theta)
 myres<-list(BOOT=FALSE,
  NSIM=NA,
  Sw=Sw,
  Sh=Sh,
  nw=nw,
  nh=nh,
  delta=delta,
  SE.delta=se,
  CV.delta=se/delta,
  BIAS.delta=NA)
 return(myres)
}

#return MLE of delta and its SE
get.estimate<-function(Sw,Sh,nw,nh){
 n<-nw+nh
 theta<-Sw*nh/(nw*Sh)
 delta<-log(theta)
 thetavar<-(theta*(Sw+Sh*theta)^2)/(n*Sh*Sw)
 deltavar<-thetavar/(theta*theta)
 se<-sqrt(deltavar)

 return(list(delta=delta,se=se))
}

#calculate SE using Bootstrap simulation
rrse2<-function(NSIM=1000,Sw=200,Sh=200,nw=444,nh=356){
 n<-nh+nw
 theta<-Sw*nh/(nw*Sh)
 delta<-log(theta)
 prob<-Sw/(Sw+Sh*theta)
 Rw<-rbinom(n=NSIM,size=n,prob=prob)
 Rh<-n-Rw
 res<-get.estimate(Sw,Sh,Rw,Rh)
 deltas<-res$delta
 ses<-res$se
 se<-sqrt(var(deltas,na.rm=T))
 mymean<-mean(deltas,na.rm=T)
 BIAS.delta<-(mymean-delta)/delta
 myres<-list(BOOT=TRUE,
  NSIM=NSIM,
  Sw=Sw,
  Sh=Sh,
  nw=nw,
  nh=nh,
  delta=delta,
  SE.delta=se,
  CV.delta=se/delta,
  BIAS.delta=BIAS.delta)
 return(myres)
}