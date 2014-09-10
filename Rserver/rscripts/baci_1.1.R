library(RJSONIO)
library(RMySQL)

s2=as.numeric(GET$s2)
rho=as.numeric(GET$rho)
n1=as.numeric(GET$n1)
n2=as.numeric(GET$n2)
k1=as.numeric(GET$k1)
k2=as.numeric(GET$k2)
me=as.numeric(GET$me)
alpha=as.numeric(GET$alpha)
delta=as.numeric(GET$delta)

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
#Program to estimate standard errors and power in a BACI-type design
#This R-code uses the added assumptions of a common variance and common correlation 
#terms (the intraclass covariance structure studied by R. A. Fisher (1925). This assumption may 
#be relaxed by specifying SIG2 as an input to the function baci in place of the input variables s2 #and rho
#inputs
#s2 is year-to-year variance (assumed equal for all populations)
#rho is the correlation of survivals between each pair of populations
#n1 number of Before years
#n2 number of After years
#k1 number of control populations
#k2 number of treatment populations
#me measurement error
#alpha -- prob. type I error (Probability of rejecting null hypothesis when true.) 
#delta -- true treatment effect representing difference in natural log survival #ln(Streatment/Scontrol)
#outputs
#se -- standard error
#cv -- coefficient of variation
#power -- probability of rejecting the null hypothesis of no effect when it is false
baci<-function(s2=1,rho=.9,n1=5,n2=5,k1=1,k2=1,me=log(1.10),alpha=0.05,delta=log(1.50)){
 check.inputs(s2,rho,n1,n2,k1,k2,me,alpha,delta)
 k<-k1+k2
 n<-n1+n2
 SIG2<-matrix(s2*rho,ncol=k,nrow=k)
 diag(SIG2)<-s2+me*me
 INVSIG2<-solve(SIG2)
 e<-rep(1,k)
 se<-n*t(e)%*%INVSIG2%*%e
 e2<-c(rep(0,k1),rep(1,k2))
 det<-n*t(e)%*%INVSIG2%*%e
 det<-det*n2*t(e2)%*%INVSIG2%*%e2-n2*n2*(t(e2)%*%INVSIG2%*%e)^2
 se<-sqrt(se/det)
#rule--reject when estimate exceeds q ses in absolute value (two-sided)
 q<-qnorm(1-alpha/2)
 power<-(1-pnorm(q*se,mean=delta,sd=se))+pnorm(-q*se,mean=delta,sd=se)
 return(list(s2=s2,rho=rho,n1=n1,n2=n2,k1=k1,k2=k2,me=me,
  alpha=alpha,delta=delta, se=se,cv=se/delta,power=power))
}

#make sure the inputs make sense
check.inputs<-function(s2,rho,n1,n2,k1,k2,me,alpha,delta){
 if(s2<0)stop("Variance, s2, must be nonnegative")
 if(me<0)stop("Measurement error standard deviation, me, must be nonnegative")
 if((s2+me*me)<=0)stop("Total variance, s2+me*me, must be positive")
 if(abs(rho)>1)stop("Correlation coefficient, rho, must be between -1 and 1")
 if(n1<=0)stop("Number of before years,n1, must be positive")
 if(round(n1)!=n1)stop("Number of before years,n1, must be an integer")
 if(n2<=0)stop("Number of after years, n2, must be positive")
 if(round(n2)!=n2)stop("Number of after years, n2, must be an integer")
 if(k1<=0)stop("Number of control populations, k1, must be positive")
 if(round(k1)!=k1)stop("Number of control populations, k1, must be an integer")
 if(k2<=0)stop("Number of treatment populations, k2, must be positive")
 if(round(k2)!=k2)stop("Number of treatment populations, k2, must be an integer")
 if((alpha<=0)|(alpha>=1))stop("Prob. of type I error, alpha, must be between 0 and 1")
 if(!is.double(delta))stop("The treatment effect, delta, must be a real number")
 return(NULL)
}
#overrides system warning function
warning = function(text){
	warn <<- text
}

test=function(){
cat(GET$s2)
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
    
	dat <- baci(s2=s2,rho=rho,n1=n1,n2=n2,k1=k1,k2=k2,me=me,alpha=alpha,delta=delta)
	#dat <- phos.pbt.estimates()
	tmp <- list(s2=dat$s2,
	rho=dat$rho,
	n1=dat$n1,
	n2=dat$n2,
	k1=dat$k1,
	k2=dat$k2,
	me=dat$me,
	alpha=dat$alpha,
	delta=dat$delta,
	se=dat$se[1],
	cv=dat$cv[1],
	power=dat$power[1],
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