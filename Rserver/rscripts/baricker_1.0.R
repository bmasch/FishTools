library(RJSONIO)
library(RMySQL)

varE=as.numeric(GET$varE)
S1=as.numeric(GET$S1)
S2=as.numeric(GET$S1)
varS1=as.numeric(GET$varS1)
varS2=as.numeric(GET$varS1)
n1=as.numeric(GET$n1)
n2=as.numeric(GET$n2)
alpha=as.numeric(GET$alpha)
theta=as.numeric(GET$theta)

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
# Program to implement a power of a BA study design aimed
# at detecting a shift in the Ricker-a as in Hinrichsen (2001).
#Input definitions
#var is the error variance
#S1 is the sample mean of the spawner counts during the Before period
#S2 is the sample mean of the spawner counts during the After period
#varS1 is the sample variance of the spawner counts during the Before period
#varS2 is the sample variance of the spawner counts during the After period
#n1 is the number of Before years
#n2 is the number of After years
#theta is the true effect size
#alpha is the probability of a Type I error (assuming a two-sided alternative hypothesis)
#
baricker<-function(var=0.25,S1=100,S2=100,varS1=10,varS2=10,n1=10,n2=10,theta=log(2.0),alpha=0.05){
se<-var*(1/n1+1/n2+(S1-S2)*(S1-S2)/(n1*varS1+n2*varS2))
se<-sqrt(se)
cv<-se/theta
delta<-theta
N<-n1+n2
q<-qt(p=1-alpha/2,df=N-3)
power<-1-pt(q,ncp=delta/se,df=N-3)+pt(-q,ncp=delta/se,df=N-3)
return(list(var=var,S1=S1,S2=S2,varS1=varS1,varS2=varS2,n1=n1,n2=n2,theta=theta, alpha=alpha,se=se,cv=cv,power=power))
}
#outputs
#se -- standard error
#cv -- coefficient of variation
#power -- probability of rejecting the null hypothesis of no effect

#end R code paste

#overrides system warning function
warning = function(text){
	warn <<- text
}

test=function(){
str(GET)
}

doCalc= function(){
    
	dat <- baricker(var=varE,S1=S1,S2=S2,varS1=varS1,varS2=varS2,n1=n1,n2=n2,theta=theta,alpha=alpha)
	#dat <- phos.pbt.estimates()
	
	tmp <- list(varE=dat$var,
	S1=dat$S1,
	S2=dat$S2,
	varS1=dat$varS1,
	varS2=dat$varS2,	
	n1=dat$n1,
	n2=dat$n2,
	theta=dat$theta,
	alpha=dat$alpha,
	SE=dat$se[1],
	CV=dat$cv[1],
	Power=dat$power[1],
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