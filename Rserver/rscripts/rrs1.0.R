library(RJSONIO)
library(RMySQL)
Sw=as.numeric(GET$Sw)
Sh=as.numeric(GET$Sh)
n=as.numeric(GET$n)
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

#Put R code here---------------------------------------------------------------------

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
	if(OPT)cat("true")
	else cat("false")
}

doCalc= function(){

  if(type=="A"){
    
	dat <- rrs.main(Sw=Sw,Sh=Sh,n=n,alpha=alpha,delta=delta,MONTE=FALSE,NSIM=1000)
	#dat <- phos.pbt.estimates()
	
	tmp <- list(Sw=dat$Sw,
	Sh=dat$Sh,
	n=n,
	alpha=dat$alpha,
	delta=dat$delta,
	type="A",
	NSIM=NA,
	SE_delta=dat$SE.delta,
	CV_delta=dat$CV.delta,
	BIAS_delta=NA,
	power=dat$power,
	warning = warn)
	cat(toJSON(tmp))	
  }
  else{
	dat <- rrs.main(Sw=Sw,Sh=Sh,n=n,alpha=alpha,delta=delta,MONTE=TRUE,NSIM=NSIM)
	tmp <- list(Sw=dat$Sw,
	Sh=dat$Sh,
	n=n,
	alpha=dat$alpha,
	delta=dat$delta,
	type="MC",
	NSIM=dat$NSIM,
	SE_delta=dat$SE.delta,
	CV_delta=dat$CV.delta,
	BIAS_delta=dat$BIAS.delta,
	power=dat$power,
	warning = warn)
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
	#uncomment this when logging works again
	#doLog(toJSON(GET),time,'FALSE',err$message)  
	cat(err$message)
}, finally = {

}) # END tryCatch
DONE