#Program to estimate the relative reproductive success (RRS)
#of hatchery-origin spawners using multiple brood years.
#AUTHOR: Richard A. Hinrichsen, Ph.D.
#DATE: 9-29-2014
#FILE: rrs2e-9-29-2014.s
#input variables
#Sw number of wild-origin spawning females (one for each year)
#Sh number of hatchery-origin spawning females (one for each year)
#nw is the number of sampled progeny with a wild-born mother (one for each year)
#nh is the number of sampled progeny with a hatchery-born mother (one for each year)
#delta is log(RRS)

#top level function
rrs2e.main<-function(Sw=c(200,200),Sh=c(200,200),nw=c(444,111),nh=c(356,89),BOOT=FALSE,NSIM=1000){
check.inputs(Sw,Sh,nw,nh,BOOT,NSIM)
if(!BOOT){res<-rrs2e(Sw=Sw,Sh=Sh,nw=nw,nh=nh)}
if(BOOT){res<-rrs2e.boot(Sw=Sw,Sh=Sh,nw=nw,nh=nh,NSIM)}
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
#check dimension of inputs
 k1<-length(Sw);k2<-length(Sh);k3<-length(nw);k4<-length(nh)
 mytest<-abs(k1-k2)+abs(k2-k3)+abs(k3-k4)
 if(mytest>0) stop("dimensions of Sw, Sh, nw, and nh must match")
 if(!is.numeric(Sw)){stop("Sw must be a number")}
 if(!is.numeric(Sh)){stop("Sh must be a number")}
 if(!is.numeric(nw)){stop("nw must be a number")}
 if(!is.numeric(nh)){stop("nh must be a number")}
 if(sum(floor(nw)-nw)){stop("Each nw must be a positive integer")}
 if(sum(nw<=0)){stop("Each nw must be a positive integer")}
 if(sum(floor(nh)-nh)){stop("Each nh must be a positive integer")}
 if(sum(nh<=0)){stop("Each nh must be a positive integer")}
 if(sum(floor(Sw)-Sw)){stop("Each Sw must be a positive integer")}
 if(sum(Sw<=0)){stop("Each Sw must be a positive integer")}
 if(sum(floor(Sh)-Sh)){stop("Each Sh must be a positive integer")}
 if(sum(Sh<=0)){stop("Each Sh must be a positive integer")}
 return(NULL)
}

#This uses theoretical formulas from Hinrichsen (2003)
rrs2e<-function(Sw,Sh,nw,nh){
 n<-nw+nh
 res<-get.estimate(Sw,Sh,nw,nh)
 delta<-res$delta
 theta<-exp(delta)
 INF<-sum((n*Sh*Sw)/(theta*(Sw+Sh*theta)^2))
 thetavar<-1/INF
 deltavar<-thetavar/(theta*theta)
 se<-sqrt(deltavar)
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
#use Fisher's Scoring Method
get.estimate<-function(Sw,Sh,nw,nh){
 NTRIAL<-100
 Rw<-nw
 n<-nw+nh
 theta<-mean(Sw*(n-Rw)/(Rw*Sh))
 tolx<-1.e-5

 for(ii in 1:NTRIAL){
   INF<-sum((n*Sh*Sw)/(theta*(Sw+Sh*theta)^2))
   df<-sum((n-Rw)/theta-n*Sh/(Sw+Sh*theta))
   delx<-(1/INF)*df
   theta<-theta+delx
   errx<-sum(abs(delx))/abs(theta)
   if(errx<=tolx)break
 }
 if(ii==NTRIAL){
  warning("maximum number of iterations was reached")
  return(list(delta=NA,se=NA))
 }
 delta<-log(theta)
 thetavar<-1/INF
 deltavar<-thetavar/(theta*theta)
 se<-sqrt(deltavar)
 return(list(delta=delta,se=se))
}


#calculate SE using bootstrap simulation
rrs2e.boot<-function(NSIM,Sw,Sh,nw,nh){
 n<-nw+nh
 res<-get.estimate(Sw,Sh,nw,nh)
 delta<-res$delta
 theta<-exp(delta)
 prob<-Sw/(Sw+Sh*theta)
 nprob<-length(prob)
 Rw<-rep(NA,nprob)
 deltas<-rep(NA,NSIM)
 ses<-rep(NA,NSIM)

 for(ii in 1:NSIM){
   Rw<-rep(NA,nprob)
   for(jj in 1:nprob){
    Rw[jj]<-rbinom(n=1,size=n[jj],prob=prob[jj])
   }
  res<-get.estimate(Sw,Sh,Rw,n-Rw)
  deltas[ii]<-res$delta
  ses[ii]<-res$se
 }
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