#Program to calculate properties of spawner estimates
#when not all hatchery-origin spawners are visibly marked (VM)
#based on a known sampling rate and VM fraction
#AUTHOR: Richard A. Hinrichsen
#CONTACT: rich@hinrichsenenvironmental.com
#DATE MODIFIED: 10/18/2013

#Variables and parameters used in the analysis
#inputs
#MONTE = logical variable when TRUE, Monte Carlo simulations are used
#Nsims = total number of bootstrap replications
#Nhos = true number of hatchery-origin spawners
#Nnos = true number of wild-origin spawners
#theta = sample rate
#lambda = VM fraction
#Em = number of sampled spawners that are VM 
#Eu = number sampled spawners that are non-VM
#
#
#intermediate variables
#phos = fraction of spawnerst that is of hatchery origin
#Ehatchsampled = Replications of number of hatchery-origin spawners that are sampled
#Enatsampled = Replications of number of wild-origin spawners that are sampled
#Em = Relications of number of VM spawners
#Eu = Replications of number of non-VM spawners
#Nhoshat = Replications estimate of Nhos
#Nnoshat = Replications of estimate of Nnos
#
#output variables
#phos (true value) calculated from Nhos and Nnos
#SE.***** = standard error (SE)
#CV.*** = Coefficient of variation
#BIAS.phoshat (relative bias of the estimator phoshat)


main<-function(MONTE=FALSE,Nsims=NA,Nhos=100,Nnos=100,theta=.25,lambda=.75){
check.inputs(MONTE,Nsims,Nhos,Nnos,theta,lambda)
 if(MONTE){res<-phos.estimates1(Nsims=Nsims,Nhos=Nhos,Nnos=Nnos,theta=theta,lambda=lambda)}
 else{res<-phos.estimates2(Nhos=Nhos,Nnos=Nnos,theta=theta,lambda=lambda)}
return(res)
}

#uses Monte Carlo simulation
phos.estimates1<-function(Nsims=10000,Nhos=100,Nnos=100,theta=0.25,lambda=0.75)
{
phos<-Nhos/(Nhos+Nnos)
#generate synthetic data sets
Ehatchsampled <-rbinom(Nsims,size=Nhos,prob=theta)
Enatsampled <-rbinom(Nsims,size=Nnos,prob=theta)
Em<-rep(NA,Nsims)
for(ii in 1:Nsims){
Em[ii]<-rbinom(1,size=Ehatchsampled[ii],prob=lambda)
}
Eu<-Ehatchsampled-Em+Enatsampled

#Replications of estimates
Nhoshat<-Em*(1/theta)*(1/lambda)
Nnoshat<-Eu*(1/theta)+Em*(1/theta)-Em*(1/lambda)*(1/theta)
phoshat<-Nhoshat/(Nhoshat+Nnoshat)
#properties of estimators
SE.Nhoshat<-sqrt(var(Nhoshat,na.rm=T))
SE.Nnoshat<-sqrt(var(Nnoshat,na.rm=T))
SE.phoshat<-sqrt(var(phoshat,na.rm=T))
CV.Nhoshat<-SE.Nhoshat/Nhos
CV.Nnoshat<-SE.Nnoshat/Nnos
CV.phoshat<-SE.phoshat/phos
BIAS.phoshat<-(mean(phoshat, na.rm=T)-phos)/phos
myres<-list(MONTE=TRUE,Nsims=Nsims,Nhos=Nhos,Nnos=Nnos,theta=theta,lambda=lambda,phos=phos,
SE.Nhoshat=SE.Nhoshat,CV.Nhoshat=CV.Nhoshat,
SE.Nnoshat=SE.Nnoshat,CV.Nnoshat=CV.Nnoshat,
SE.phoshat=SE.phoshat,CV.phoshat=CV.phoshat, BIAS.phoshat=BIAS.phoshat)
return(myres)
}
#Uses theoretical variance calculations
#the estimate of standard error of phos is unreliable when Nhos and Nnos are small
#because it is based on a first order Taylor series expansion about the spawner estimates.
#standard errors for spawner estimates are exact
phos.estimates2<-function(Nhos=100,Nnos=100,theta=0.25,lambda=0.75)
{
N<-Nhos+Nnos
phos<-Nhos/(Nhos+Nnos)
#properties of estimators
var.Nhoshat<-Nhos*(1-lambda*theta)/(lambda*theta)
var.Nnoshat<-Nnos*(1-theta)/theta+Nhos*(1-lambda)/(theta*lambda)
var.phoshat<-(Nhos/(lambda*theta*N^4))*(Nnos*Nnos*(1-lambda*theta)+Nhos*Nnos*(2-lambda-lambda*theta)+Nhos*Nhos*(1-lambda))
SE.Nhoshat<-sqrt(var.Nhoshat)
SE.Nnoshat<-sqrt(var.Nnoshat)
SE.phoshat<-sqrt(var.phoshat)
CV.Nhoshat<-SE.Nhoshat/Nhos
CV.Nnoshat<-SE.Nnoshat/Nnos
CV.phoshat<-SE.phoshat/phos
myres<-list(MONTE=FALSE,Nsims=NA,Nhos=Nhos,Nnos=Nnos,theta=theta,lambda=lambda,phos=phos,
SE.Nhoshat=SE.Nhoshat,CV.Nhoshat=CV.Nhoshat,
SE.Nnoshat=SE.Nnoshat,CV.Nnoshat=CV.Nnoshat,
SE.phoshat=SE.phoshat,CV.phoshat=CV.phoshat, BIAS.phoshat=NA)
return(myres)
}

#make sure inputs make sense
check.inputs<-function(MONTE,Nsims,Nhos,Nnos,theta,lambda){
if(!is.logical(MONTE)){stop("MONTE must be a logical value")}
if(MONTE){
 if(!is.numeric(Nsims))stop("In Monte Carlo model, Nsims must be an integer")
 if(!(Nsims==round(Nsims)))stop("In Monte Carlo mode, Nsims must be an integer")
}
if(Nhos!=round(Nhos))stop("Nhos must be an integer")
if(Nhos<0)stop("Nhos must be nonnegative")
if(Nnos!=round(Nnos))stop("Nnos must be an integer")
if(Nnos<0)stop("Nnos must be nonnegative")
if(theta>1)stop("Sample rate, theta, must be less than or equal to one")
if(theta<=0)stop("Sample rate, theta, must be greater than or equal to zero")
if(lambda>1)stop("VM fraction, lambda, must be less than or equal to one")
if(lambda<=0)stop("VM fraction, lambda, must be greater than or equal to zero")
return(NULL)
}


