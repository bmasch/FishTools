library(RJSONIO)
library(RMySQL)

vartime=as.numeric(GET$vartime)
varsite=as.numeric(GET$varsite)
n1=as.numeric(GET$n1)
n2=as.numeric(GET$n2)
N=as.numeric(GET$N)
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
# Program to implement a power analysis of a paired experiment
# as in Liermann and Roni (2008)
#N is the number of sites
#n1 is the number of Before years
#n2 is the number of After years
#theta is the true effect size which is the true shift in the difference
# in treatment and control log(smolts) between the Before and After periods.
#
lr<-function(vartime=1,varsite=1,N=20,n1=2,n2=8,theta=.02,alpha=0.05){
sd<-vartime*(1/n1+1/n2)+varsite
sd<-sqrt(sd)
se<-sd/sqrt(N)
cv<-se/theta
delta=theta
q<-qt(p=1-alpha/2,df=N-1)
power<-1-pt(q,ncp=delta/se,df=N-1)+pt(-q,ncp=delta/se,df=N-1)
return(list(vartime=vartime,varsite=varsite,N=N,n1=n1,n2=n2,theta=theta,alpha=alpha,
se=se,cv=cv,power=power))
}
#outputs
#se -- standard error
#cv -- coefficient of variation
#power -- probability of rejecting the null hypothesis of no treatment effect

#end R code paste

#overrides system warning function
warning = function(text){
	warn <<- text
}

test=function(){
cat(GET$vartime)
cat(GET$rho)
cat(GET$n1)
cat(GET$n2)
cat(GET$k1)
cat(GET$k2)
cat(GET$me)
cat(GET$alpha)
cat(GET$delta)
}

doCalc= function(){
    
	dat <- lr(vartime=vartime,varsite=varsite,N=N,n1=n1,n2=n2,theta=theta,alpha=alpha)
	#dat <- phos.pbt.estimates()
	
	tmp <- list(vartime=dat$vartime,
	varsite=dat$varsite,
	N=dat$N,
	n1=dat$n1,
	n2=dat$n2,
	theta=dat$theta,
	alpha=dat$alpha,
	StdErr=dat$se[1],
	CoeffVar=dat$cv[1],
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