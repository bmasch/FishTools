library(RJSONIO)
library(RMySQL)
NT=as.numeric(GET$NT)
NM=as.numeric(GET$NM)
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

#R-code used to estimate the power of an experiment
#to detect a significant difference between the
#smolt-to-adult SARs of transported juveniles
#and the smolt-to-adult survival rate of juveniles
#migrating in-river.
#inputs
#SART -- SAR of transported juveniles
#NT -- the number of tagged transported juveniles
#SARM -- SAR of in-river juvenile migrants
#NM -- number of tagged in-river juvenile migrants
tmpower<-function(SART=.01,NT=1000,SARM=.02,NM=1000,alpha=0.05){
if((SART>1)|(SARM<0)){
warning("The SARs are survival rates and so must be less than one")
return(NULL)}
if((SARM>1)|(SARM<0)){
warning("The SARs are survival rates and so must be less than one")
return(NULL)}
if(NM<=0){
warning("The number of juveniles released must exceed zero")
return(NULL)}
if(NT<=0){
warning("The number of juveniles released must exceed zero")
return(NULL)}
if((alpha>1)|(alpha<0)){
warning("The significance level (alpha) must be between zero and one")
return(NULL)}
if((NT*SART<5)|(NM*SARM<5)){
warning("An expected cell count is less than 5" )
return(NULL)}
delta<-log(SART/SARM)
q<-qnorm(1-alpha/2)
tmvar<-(1-SART)/(SART*NT)+(1-SARM)/(SARM*NM)
se<-sqrt(tmvar)
power<-(1-pnorm(q*se,mean=delta,sd=se))+pnorm(-q*se,mean=delta,sd=se)
return(list(SART=SART,NT=NT,SARM=SARM,NM=NM,alpha=alpha,delta=delta,se=se,
cv=se/delta,power=power))
}
#outputs
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
    
	dat <- tmpower(SART=SART,NT=NT,SARM=SARM,NM=NM,alpha=alpha)
	tmp <- list(SART=dat$SART,
	NT=dat$NT,
	SARM=dat$SARM,
	NM=dat$NM,
	alpha=dat$alpha,
	NT=dat$NT,
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