library(RJSONIO)
library(RMySQL)
Sw=as.numeric(GET$Sw)
Sh=as.numeric(GET$Sh)
nw=as.numeric(GET$nw)
nh=as.numeric(GET$nh)
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
#Program to estimate the relative reproductive success (RRS)
#of hatchery-origin spawners
#AUTHOR: Richard A. Hinrichsen, Ph.D.
#DATE: 9-28-2014
#FILE: rrse-9-28-2014.s
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
    
	dat <- rrse.main(Sw=Sw,Sh=Sh,nw=nw,nh=nh,BOOT=FALSE,NSIM=1000)
	#dat <- phos.pbt.estimates()
	
	tmp <- list(Sw=dat$Sw,
	Sh=dat$Sh,
	nh=nh,
	nw=nw,
	type="A",
	NSIM=NA,
	delta=dat$delta,
	SE_delta=dat$SE.delta,
	CV_delta=dat$CV.delta,
	BIAS_delta=NA,
	warning = warn)
	cat(toJSON(tmp))	
  }
  else{
	dat <- rrse.main(Sw=Sw,Sh=Sh,nw=nw,nh=nh,BOOT=TRUE,NSIM=NSIM)
	tmp <- list(Sw=dat$Sw,
	Sh=dat$Sh,
	nh=nh,
	nw=nw,
	type="B",
	NSIM=dat$NSIM,
	delta=dat$delta,
	SE_delta=dat$SE.delta,
	CV_delta=dat$CV.delta,
	BIAS_delta=dat$BIAS.delta,
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