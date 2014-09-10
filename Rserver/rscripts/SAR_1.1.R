library(RJSONIO)
library(RMySQL)
Nr=as.numeric(GET$Nr)
Nd=as.numeric(GET$Nd)

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

# Program to estimate standard error and CV
# of a smolt-to-adult ratio (SAR). Adult recoveries
# are assumed to be binomially distributed
#input parameters
#Nr number of juveniles released
#Nd number of adults detected
SAR<-function(Nr,Nd){
if(Nd>Nr){
warning("The number of adults detected (Nd) must be no greater than the number of juveniles released (Nr)")
return(NULL)}
sar<-Nd/Nr
se<-sar*sqrt(1/Nd-1/Nr)
cv<-sqrt(1/Nd-1/Nr)
return(list(Nr=Nr,Nd=Nd,sar=sar,se=se,cv=cv))
}
#outputs
#sar survival rate estimate (SAR)
#se standard error
#cv coefficient of variation

#end R code paste

#overrides system warning function
warning = function(text){
	warn <<- text
}

test=function(){
str(GET)
}

doCalc= function(){
    
	dat <- SAR(Nr=Nr,Nd=Nd)
	#dat <- phos.pbt.estimates()
	
	tmp <- list(Nr=dat$Nr,
	Nd=dat$Nd,
	SAR=dat$sar,
	SE=dat$se,
	CV=dat$cv,
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