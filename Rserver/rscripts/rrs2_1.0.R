library(RJSONIO)
library(RMySQL)


Sw=as.numeric(unlist(strsplit(GET$Sw,"\n")))
Sh=as.numeric(unlist(strsplit(GET$Sh,"\n")))
n=as.numeric(unlist(strsplit(GET$n,"\n")))
alpha=as.numeric(GET$alpha)
delta=as.numeric(GET$delta)
type=GET$type
NSIM=as.numeric(GET$NSIM)
time1=proc.time()[3]
warn=""

#needed for logging
getBrowserIndex = function(){
	nm = names(SERVER$headers_in)
	index=0
	for(i in 1:length(nm)){
		if(nm[i]=="User-Agent")index=i
	}
	return(index)
}

doLog = function(log.string,process.time,was.successful,log.message){
}

#Program to calculate power of an experiment aimed at estimating the relative reproductive success (RRS)
#of hatchery-origin spawners using multiple brood years.
#AUTHOR: Richard A. Hinrichsen, Ph.D.
#DATE: 6-21-2014
#FILE: rrs2-6-21-2014.s
#input variables
#Sw number of wild-origin spawning females (one for each year)
#Sh number of hatchery-origin spawning females (one for each year)
#n is the sample size of progeny (one for each year)
#delta is log(RRS)

#top level function
rrs2.main<-function(Sw=c(200,200),Sh=c(200,200),n=c(800,200),alpha=0.05,delta=log(.8),MONTE=FALSE,NSIM=1000){
check.inputs(Sw,Sh,n,alpha,delta,MONTE,NSIM)
if(!MONTE){res<-rrs2(Sw=Sw,Sh=Sh,n=n,alpha=alpha,delta=delta)}
if(MONTE){res<-rrs2.monte(Sw=Sw,Sh=Sh,n=n,alpha=alpha,delta=delta,NSIM)}
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
#check dimension of inputs
 k1<-length(Sw);k2<-length(Sh);k3<-length(n)
 mytest<-abs(k1-k2)+abs(k2-k3)
 if(mytest>0) stop("dimensions of Sw, Sh, and n must match")
 if(!is.numeric(Sw)){stop("Sw must be a number")}
 if(!is.numeric(Sh)){stop("Sh must be a number")}
 if(!is.numeric(n)){stop("n must be a number")}
 if(sum(floor(n)-n)){stop("Each n must be a positive integer")}
 if(sum(n<=0)){stop("Each n must be a positive integer")}
 if(sum(floor(Sw)-Sw)){stop("Each Sw must be a positive integer")}
 if(sum(Sw<=0)){stop("Each Sw must be a positive integer")}
 if(sum(floor(Sh)-Sh)){stop("Each Sh must be a positive integer")}
 if(sum(Sh<=0)){stop("Each Sh must be a positive integer")}
 if(alpha<=0){stop("alpha must be between zero and 1.0")}
 if(alpha>=1){stop("alpha must be between zero and 1.0")}
 if(!is.double(alpha)){stop("alpha must be a double")}
 if(!is.double(delta)){stop("delta must be a double")}
 return(NULL)
}

#This uses theoretical formulas from Hinrichsen (2003)
rrs2<-function(Sw,Sh,n,alpha,delta){
 theta<-exp(delta)
 q<-qnorm(1-alpha/2)
 INF<-sum((n*Sh*Sw)/(theta*(Sw+Sh*theta)^2))
 thetavar<-1/INF
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
#use Fisher's Scoring Method
get.estimate<-function(Sw,Sh,n,Rw){
 NTRIAL<-100
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


#calculate SE and statistical power using Monte Carlo simulation
rrs2.monte<-function(NSIM,Sw,Sh,n,alpha,delta){
 theta<-exp(delta)
 q<-qnorm(1-alpha/2)
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
  res<-get.estimate(Sw,Sh,n,Rw)
  deltas[ii]<-res$delta
  ses[ii]<-res$se
 }

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

#set seed on server date variable
reSeed = function(rt){
	time1=strsplit(rt," ")[[1]][2]
	time2=strsplit(time1,":")
	h=as.numeric(time2[[1]][1])
	m=as.numeric(time2[[1]][2])
	s=as.numeric(time2[[1]][3])
	set.seed(num=h*3600+m*60+s)
}

#overrides system warning function
warning = function(text){
	warn <<- text
}

test=function(){
	str(GET)
}

doCalc= function(){

  if(type=="A"){
	dat <- rrs2.main(Sw=Sw,Sh=Sh,n=n,alpha=alpha,delta=delta,MONTE=FALSE,NSIM=NSIM)
	tmp <- list(Sw=dat$Sw,
	Sh=dat$Sh,
	n=dat$n,
	alpha=dat$alpha,
	delta=dat$delta,
	type="A",
	NSIM=NA,
	SE_delta=dat$SE.delta,
	CV_delta=dat$CV.delta,
	BIAS_delta=NA,
	power=dat$power,
	warning = warn)
	time=proc.time()[3]-time1
	doLog(toJSON(GET),time,'TRUE',warn)	
	cat(toJSON(tmp))	
  }
  else{
	dat <- rrs2.main(Sw=Sw,Sh=Sh,n=n,alpha=alpha,delta=delta,MONTE=TRUE,NSIM=NSIM)
	tmp <- list(Sw=dat$Sw,
	Sh=dat$Sh,
	n=dat$n,
	alpha=dat$alpha,
	delta=dat$delta,
	type="MC",
	NSIM=dat$NSIM,
	SE_delta=dat$SE.delta,
	CV_delta=dat$CV.delta,
	BIAS_delta=dat$BIAS.delta,
	power=dat$power,
	warning = warn)
	time=proc.time()[3]-time1
	doLog(toJSON(GET),time,'TRUE',warn)	
	cat(toJSON(tmp))	
  }
}

# Output starts here
setHeader(header='Expires',value='-1')
setHeader(header='Pragma',value='no-cache')
setContentType("text/html")

res=tryCatch(doCalc(),
#res=tryCatch(test(),
 warning = function(war) {
}, error = function(err) {
  # error handler picks up where error was generated
  	time=proc.time()[3]-time1
	doLog(toJSON(GET),time,'FALSE',err$message)  
	cat(err$message)
}, finally = {

}) # END tryCatch
DONE