library(RJSONIO)
library(RMySQL)

N=as.numeric(GET$N)
SART=as.numeric(GET$SART)
SARM=as.numeric(GET$SARM)
alpha=as.numeric(GET$alpha)

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

#Put R Functions here
#R-code used to estimate the optimal allocation of tagged smolts between transported
#and in-river groups.
#inputs
#N -- the total number of tagged smolts
#SART -- SAR of transported juveniles
#SARM -- SAR of in-river juvenile migrants
tmdesign<-function(N=1000,SART=.01 ,SARM=.02,alpha=0.05){
if(N<=0){
warning("The number of juveniles released must exceed zero")
return(NULL)}
if((SART>1)|(SARM<0)){
warning("The SARs are survival rates and so must be less than one")
return(NULL)}
if((SARM>1)|(SARM<0)){
warning("The SARs are survival rates and so must be less than one")
return(NULL)}
s1<-sqrt((1-SART)/SART)
s2<-sqrt((1-SARM)/SARM)
ft<-s1/(s1+s2)
NT<-ft*N
NM<-N-NT
delta<-log(SART/SARM)
q<-qnorm(1-alpha/2)
tmvar<-(1-SART)/(SART*NT)+(1-SARM)/(SARM*NM)
se<-sqrt(tmvar)
power<-(1-pnorm(q*se,mean=delta,sd=se))+pnorm(-q*se,mean=delta,sd=se)
return(list(N=N,SART=SART, SARM=SARM ,alpha=alpha, NT=NT, NM=NM, delta=delta,se=se,cv=se/delta,power=power))
}
#outputs
#NT -- the optimal number of tagged transported juveniles
#NM – the optimal number of tagged in-river juvenile migrants
#delta is the true log(SART/SARM)
#se is the standard error of the estimate
#cv is the CV of the estimate
#power is the probability of rejecting the null hypothesis of delta=0.

#end R code paste

#overrides system warning function
warning = function(text){
	warn <<- text
}

test=function(){
str(GET)
}

doCalc= function(){
    
	dat <- tmdesign(N=N,SART=SART,SARM=SARM,alpha=alpha)
	#dat <- phos.pbt.estimates()
	
	tmp <- list(N=dat$N,
	SART=dat$SART,
	SARM=dat$SARM,
	alpha=dat$alpha,
	NT=dat$NT,
	NM=dat$NM,
	delta=dat$delta,
	StdErr=dat$se,
	CoeffVar=dat$cv,
	Power=dat$power,
	warning = warn)
	time=proc.time()[3]-time1
	doLog(toJSON(GET),time,'TRUE',warn)	
	cat(toJSON(tmp))	
  
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