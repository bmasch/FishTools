#Program to calculate properties of phos estimates using Monte Carlo simulation and theoretical results
#This code treats the general case of inputs from several hatcheries with potentially different marking fractions
#and different fraction of marked fish given a parentage-based tag. 
#FILE: pbt-web-11-16-2012.s
#AUTHOR: Richard A. Hinrichsen, 16 November 2012
#REVISION: Used n as an input instead of n2 (n2 is calculated as n2=n-n1)
#REVISION: allow program to optimally choose n1 given a fixed n. See flag OPT

#Variables and parameters used in the analysis
#inputs
#phosi = true proportions of hatchery origin spawners (hatchery-specific)
#Nsamp = total number of spawners sampled on spawning grounds
#n = total number of spawners sent to the lab and checked for PBT
#n1 = number of marked spawners sent to the lab and checked for PBT (when OPT=FALSE)
#lambda = marking rate (lambda)  (hatchery-specific)
#ppbt=fraction of marked fish that are also parentage-based tagged (hatchery-specific)
#OPT = FALSE when n1 is user input, TRUE when program to select an optimal value of n1
#MONTE = FALSE for theoretical results, TRUE for Monte Carlo results
#NSIM = number of Monte Carlo simulations (needed if MONTE=TRUE)
#
#
#Select intermediate variables
#nhatch=number of hatcheries supplying spawners in the wild
#n2 = n - n1
#I = Fisher Information Matrix
#I2 = Fisher Information Matrix giving minimum SE for phos (occurs when n=N)

#Results
#phos = true proportion of hatchery-origin spawners
#Ex1 = expected number of VM spawners (summing over hatcheries)
#Ex2 = expected number of not VM spawners (summing over hatcheries)
#SE_MIN.phoshat = standard error (SE) when ALL sampled fish genetically tested
#CV_MIN.phoshat = Coefficient of variation when ALL sampled fish genetically tested
#SE.phoshat = standard error (SE) 
#CV.phoshat = Coefficient of variation 
#BIAS.phoshat = relative bias estimate (NA if MONTE=FALSE)
#n1 = an optimal number of marked spawners sent to the lab and checked for PBT (when OPT=TRUE)

#top level function
phos.pbt.main <- function(phosi=.1*c(1/20,1/20,9/20,9/20),
Nsamp=1000,n=200,n1=50,lambda=c(1,.95,.5,.5),ppbt=c(.95,.95,.95,.95),OPT=FALSE,
MONTE=FALSE,NSIM=1000){
        check.inputs(phosi,Nsamp,n,n1,lambda,ppbt,OPT,MONTE,NSIM)
        if(!OPT){
        if(!MONTE){res <- phos.pbt.estimates(phosi=phosi,Nsamp=Nsamp,n=n,n1=n1,lambda=lambda,ppbt=ppbt)}
        if(MONTE){res <- phos.pbt.estimates2(NSIM=NSIM,phosi=phosi,Nsamp=Nsamp,n=n,n1=n1,lambda=lambda,ppbt=ppbt)}}
        if(OPT){res <- optimize(phosi=phosi,Nsamp=Nsamp,n=n,lambda=lambda,ppbt=ppbt)}
        return(res)
}

#Determine an optimal value of n1
optimize <- function(phosi,Nsamp,n,lambda,ppbt){
 #loop over all feasible values of n1
 #note that n1 might be a non-integer because the constraints are not necessarily integers
 Ex1 <- Nsamp*sum(lambda*phosi)
 min.n1 <- max(Ex1-(Nsamp-n),0)
 max.n1 <- min(Ex1,n)
 min.int <- ceiling(min.n1)
 max.int <- floor(max.n1)
 if(min.int>=max.int){n1s <- min.n1}
 if(min.int<max.int){n1s <- c(min.n1,min.int:max.int,max.n1)}
 n1s <- unique(n1s)
 nfeas <- length(n1s)
 cv <- 1.e10
 for(ii in 1:nfeas){
  res.new <- phos.pbt.estimates(phosi=phosi,Nsamp=Nsamp,n=n,n1=n1s[ii],lambda=lambda,ppbt=ppbt)
  if(res.new$CV.phoshat<cv){res <- res.new;n1 <- res.new$n1;cv <- res.new$CV.phoshat}
#  print(c(res.new$n1,res.new$CV.phoshat))
 }
 return(res)
}

#optimize(phosi=c(.1,.2),Nsamp=1000,n=100,lambda=c(1.0,.5),ppbt=c(.95,.90))

check.inputs <- function(phosi,Nsamp,n,n1,lambda,ppbt,OPT,MONTE,NSIM){
 if(!is.logical(OPT))stop("OPT must be TRUE or FALSE")
 if(!is.logical(MONTE))stop("MONTE must be TRUE or FALSE")
 if(OPT){
 if(MONTE){stop("For the optimization option, the program cannot be run in Monte Carlo mode")}}
 if(MONTE){
 if(floor(NSIM)!=NSIM){stop("NSIM must be a positive integer")}
 if(NSIM<=0){stop("NSIM must be a positive integer")}}

 if(floor(Nsamp)!=Nsamp){stop("Nsamp must be a positive integer")}
 if(Nsamp<=0){stop("Nsamp must be a positive integer")}

#check dimension of inputs
 k1 <- length(phosi);k2 <- length(lambda);k3 <- length(ppbt)
 mytest <- abs(k1-k2)+abs(k2-k3)
 if(mytest>0) stop("dimensions of phosi, lambda, and ppbt must match")
 nhatch <- length(phosi)
#check that the subsample size is less than sample size
 if(n>Nsamp)stop(paste("n must be less than or equal to Nsamp=",Nsamp))
#check ppbts (must all exceed zero)
 if(sum(ppbt<=0))stop("ppbts must all be greater than zero")
#check that each ppbt is less than or equal to one
 if(sum(ppbt>1))stop("ppbts must all be less than or equal to 1.0")
#check that all lambdas are between zero and 1.0
 if(sum(lambda<0))stop("lambdas must all be greater than or equal to zero")
 if(sum(lambda>1))stop("lambdas must all be less than or equal to one")
#check that all phosi are between zero and 1.0
 if(sum(phosi<=0))stop("phosi must all be greater than zero")
 if(sum(phosi>1))stop("phosi must all be less than or equal to one")
#fail if n=0 and lambdas not constant
 if((n==0)&(sum(lambda==mean(lambda))< nhatch)){
 stop("Error: subsample sizes are zero and lambdas vary among hatcheries. Try setting n1>0 or n>n1")
 }
 if(sum(lambda==0)&n==n1){
 stop("Error: Some lambdas are zero and n = n1. Try setting n > n1.")
}

#get expected values of observations
 Ex1 <- Nsamp*sum(lambda*phosi)
 Ex2 <- Nsamp-Ex1
#check subsample
 if(OPT==FALSE){
 if(n1<0)stop("n1 must be nonnegative")
 if(n<n1)stop("n must be greater than or equal to n1")
 if(n1>Ex1)stop(paste("n1 must not exceed the expected number of VM spawners in sample=",Ex1))
 if(n1<(n-Ex2))stop(paste("n1 must be >= n minus the expected number of ~VM spawners in sample=",n-Ex2))}
 return(NULL)
}


#Theoretical results
phos.pbt.estimates <- function(phosi=.1*c(1/20,1/20,9/20,9/20),
Nsamp=1000,n=200,n1=50,lambda=c(1,.95,.5,.5),ppbt=c(.95,.95,.95,.95)){

 check.inputs(phosi=phosi,Nsamp=Nsamp,n=n,n1=n1,lambda=lambda,ppbt=ppbt,OPT=FALSE,MONTE=FALSE,NSIM=NA)
 n2 <- n-n1
 nhatch <- length(lambda)
#get expected values of observations
 Ex1 <- Nsamp*sum(lambda*phosi)
 Ex2 <- Nsamp-Ex1
 phos <- sum(phosi)
#check case of n1=0,n-n1=0 and constant lambdas
if((n==0)&(sum(lambda==mean(lambda))==nhatch)){
 res <- onehatchery.estimates(phos=phos,lambda=mean(lambda),Nsamp=Nsamp)
 res2 <- phos.pbt.estimates(phosi=phosi,Nsamp=Nsamp,n=Nsamp,n1=Ex1,lambda=lambda,ppbt=ppbt)
 myres <- list(MONTE=FALSE,
                   NSIM=NA,
                   phosi=phosi,
                   Nsamp=Nsamp,
                   n=n,
		   n1=n1,
                   lambda=lambda,
                   ppbt=ppbt,
                   phos=phos,
                   Ex1=Ex1,
		   Ex2=Ex2,
                   SE_MIN.phoshat=res2$SE_MIN.phoshat,
                   CV_MIN.phoshat=res2$CV_MIN.phoshat,
                   SE.phoshat=res$SE.phoshat,
                   CV.phoshat=res$CV.phoshat,
                   BIAS.phoshat=NA)
 return(myres)
}

#combine cells with lambda=1 into a single cell and try again!
if((sum(lambda==1)>1)&(n1==0)){
 warning("collapsing cells with lambda=1 into a single cell since n1=0")
 iii <- lambda==1
 lambda1.new <- 1.0
 ppbt1.new <- sum(ppbt[iii]*phosi[iii])/sum(phosi[iii])
 phosi1.new <- sum(phosi[iii])
 lambda.new <- c(lambda1.new,lambda[!iii])
 ppbt.new <- c(ppbt1.new,ppbt[!iii])
 phosi.new <- c(phosi1.new,phosi[!iii])
 res <- phos.pbt.estimates(phosi=phosi.new,Nsamp=Nsamp,n=n,n1=n1,lambda=lambda.new,ppbt=ppbt.new)
 myres <- list(MONTE=FALSE,
                   NSIM=NA,
                   phosi=phosi,
                   Nsamp=Nsamp,
                   n=n,
		   n1=n1,
                   lambda=lambda,
                   ppbt=ppbt,
                   phos=phos,
                   Ex1=Ex1,
		   Ex2=Ex2,
                   SE_MIN.phoshat=res$SE_MIN.phoshat,
                   CV_MIN.phoshat=res$CV_MIN.phoshat,
                   SE.phoshat=res$SE.phoshat,
                   CV.phoshat=res$CV.phoshat,
                   BIAS.phoshat=NA)
 return(myres)
}


#check to see if all pbts are one (special case)
pbttest <- FALSE
if(sum(ppbt==1)==nhatch)pbttest <- TRUE
#check to see if all lambdas are zero (special case)
lambdatest <- FALSE
if(sum(lambda==0)==nhatch)lambdatest <- TRUE


#get Fisher Information Matrix
if(lambdatest){
  I <- getI3(Nsamp,n1,n2,lambda,ppbt,phosi)
  I2 <- getI3(Nsamp,Ex1,Ex2,lambda,ppbt,phosi)
}
else{

if(pbttest){
  I <- getI2(Nsamp,n1,n2,lambda,ppbt,phosi)
  I2 <- getI2(Nsamp,Ex1,Ex2,lambda,ppbt,phosi)
}
else{
  I <- getI(Nsamp,n1,n2,lambda,ppbt,phosi)
  I2 <- getI(Nsamp,Ex1,Ex2,lambda,ppbt,phosi)
}
}
#check to see if Fisher Information Matrix is computationally singular
if(is.na(rcond(I)))stop("Condition number of I is NA")
if(rcond(I)<1.e-16)stop("Fisher Information matrix I is computationally singular")
varmat <- solve(I)
e <- rep(1,nhatch)
phos.var <- t(e)%*%varmat%*%e

if(is.na(rcond(I2)))stop("Condition number of I2 is NA")
#get var when all sampled spawners sent to the lab
if(rcond(I2)<1.e-16)stop("Fisher Information matrix I2 is computationally singular")
varmat2 <- solve(I2)
e <- rep(1,nhatch)
phos2.var <- t(e)%*%varmat2%*%e

SE.phoshat <- sqrt(phos.var)
CV.phoshat <- SE.phoshat/phos
SE_MIN.phoshat <- sqrt(phos2.var)
CV_MIN.phoshat <- SE_MIN.phoshat/phos
SE_MIN.phoshat <- as.numeric(SE_MIN.phoshat)
CV_MIN.phoshat <- as.numeric(CV_MIN.phoshat)
SE.phoshat <- as.numeric(SE.phoshat)
CV.phoshat <- as.numeric(CV.phoshat)

myres <- list(MONTE=FALSE,
                   NSIM=NA,
                   phosi=phosi,
                   Nsamp=Nsamp,
                   n=n,
		   n1=n1,
                   lambda=lambda,
                   ppbt=ppbt,
                   phos=phos,
                   Ex1=Ex1,
		   Ex2=Ex2,
                   SE_MIN.phoshat=SE_MIN.phoshat,
                   CV_MIN.phoshat=CV_MIN.phoshat,
                   SE.phoshat=SE.phoshat,
                   CV.phoshat=CV.phoshat,
                   BIAS.phoshat=NA)
 return(myres)
}

#get SE and CV in single hatchery case (lambdas constant and n1=n2=0)
onehatchery.estimates <- function(phos,Nsamp,lambda){
SE.phoshat <- phos*(1-phos*lambda)/(Nsamp*lambda)
SE.phoshat <- sqrt(SE.phoshat)
CV.phoshat <- SE.phoshat/phos
return(list(SE.phoshat=SE.phoshat,CV.phoshat=CV.phoshat))
}

#Fisher Information Matrix
getI <- function(Nsamp,n1,n2,lambda,ppbt,phosi){
 Ex1 <- Nsamp*sum(lambda*phosi)
 Ex2 <- Nsamp-Ex1
 theta1 <- n1/Ex1
 theta2 <- n2/Ex2

 v <- lambda
 I <-  v%*%t(v)*Nsamp*(1-theta1)/sum(v*phosi)
 I <- I +v%*%t(v)*Nsamp*(1-theta2)/(1-sum(v*phosi))

 v <- (1-ppbt)*lambda
 I <- I+v%*%t(v)*Nsamp*theta1/sum(v*phosi)

 v <- lambda*(1-ppbt)+ppbt
 I <- I+v%*%t(v)*Nsamp*theta2/(1-sum(v*phosi))

#fix diagonal
 mydiag <- diag(I)+Nsamp*ppbt*(theta1*lambda+theta2*(1-lambda))/phosi
 diag(I) <- mydiag
 return(I)
}

#Fisher Information matrix (used when all pbts are one)
getI2 <- function(Nsamp,n1,n2,lambda,ppbt,phosi){
 Ex1 <- Nsamp*sum(lambda*phosi)
 Ex2 <- Nsamp-Ex1
 theta1 <- n1/Ex1
 theta2 <- n2/Ex2

 v <- lambda
 I <-  v%*%t(v)*Nsamp*(1-theta1)/sum(v*phosi)
 I <- I +v%*%t(v)*Nsamp*(1-theta2)/(1-sum(v*phosi))
 I <- I+Nsamp*theta2/(1-sum(phosi))

#fix diagonal
 mydiag <- diag(I)+Nsamp*(theta1*lambda+theta2*(1-lambda))/phosi
 diag(I) <- mydiag
 return(I)
}

#Fisher Information matrix (used when all lambdas are zero)
getI3 <- function(Nsamp,n1,n2,lambda,ppbt,phosi){
 Ex2 <- Nsamp
 theta2 <- n2/Ex2
 v <- ppbt
 I <- v%*%t(v)*Nsamp*theta2/(1-sum(v*phosi))
#fix diagonal
 mydiag <- diag(I)+Nsamp*ppbt*theta2/phosi
 diag(I) <- mydiag
 return(I)
}

#Monte Carlo estimates of standard error and bias
phos.pbt.estimates2 <- function(NSIM=1000,phosi=.1*c(1/20,1/20,9/20,9/20),
 Nsamp=1000,n=n,n1=50,lambda=c(1,.95,.5,.5),ppbt=c(.95,.95,.95,.95)){

 check.inputs(phosi=phosi,Nsamp=Nsamp,n=n,n1=n1,lambda=lambda,ppbt=ppbt,OPT=FALSE,MONTE=TRUE,NSIM=NSIM)
 n2 <- n-n1
 nhatch <- length(phosi)

#get expected values of observations
 Ex1 <- Nsamp*sum(lambda*phosi)
 Ex2 <- Nsamp-Ex1

 phos <- sum(phosi)
#check for singularity. Combine cells with lambda=1 into a single cell and try again!
if((sum(lambda==1)>1)&(sum(lambda==1)< nhatch)&(n1==0)){
 warning("collapsing cells with lambda=1 into a single cell since n1=0")
 iii <- lambda==1
 lambda1.new <- 1.0
 ppbt1.new <- sum(ppbt[iii]*phosi[iii])/sum(phosi[iii])
 phosi1.new <- sum(phosi[iii])
 lambda.new <- c(lambda1.new,lambda[!iii])
 ppbt.new <- c(ppbt1.new,ppbt[!iii])
 phosi.new <- c(phosi1.new,phosi[!iii])
 res <- phos.pbt.estimates2(NSIM=NSIM,phosi=phosi.new,Nsamp=Nsamp,n=n,n1=n1,lambda=lambda.new,ppbt=ppbt.new)
 myres <- list(MONTE=TRUE,
                   NSIM=NSIM,
                   phosi=phosi,
                   Nsamp=Nsamp,
                   n=n,
		   n1=n1,
                   lambda=lambda,
                   ppbt=ppbt,
                   phos=phos,
                   Ex1=Ex1,
		   Ex2=Ex2,
                   SE_MIN.phoshat=res$SE_MIN.phoshat,
                   CV_MIN.phoshat=res$CV_MIN.phoshat,
                   SE.phoshat=res$SE.phoshat,
                   CV.phoshat=res$CV.phoshat,
                   BIAS.phoshat=res$BIAS.phoshat)
 return(myres)
}


 phos.sim <- rep(NA,NSIM)
 for(ii in 1:NSIM){
  phos.sim[ii] <- get.estimates2(phosi=phosi,Nsamp=Nsamp,n1=n1,n2=n2,lambda=lambda,ppbt=ppbt)
 }

 SE.phoshat <- sqrt(var(phos.sim,na.rm=T))
 CV.phoshat <- SE.phoshat/phos
 mymean <- mean(phos.sim,na.rm=T)
 BIAS.phoshat <- (mymean-phos)/phos

#Now get minimum variance and CV
 phos.sim <- rep(NA,NSIM)
 for(ii in 1:NSIM){
  phos.sim[ii] <- get.estimates2(phosi=phosi,Nsamp=Nsamp,n1=Ex1,n2=Ex2,lambda=lambda,ppbt=ppbt)
 }
 SE_MIN.phoshat <- sqrt(var(phos.sim,na.rm=T))
 CV_MIN.phoshat <- SE_MIN.phoshat/phos

 myres <- list(MONTE=TRUE,
                   NSIM=NSIM,
                   phosi=phosi,
                   Nsamp=Nsamp,
                   n=n,
		   n1=n1,
                   lambda=lambda,
                   ppbt=ppbt,
                   phos=phos,
                   Ex1=Ex1,
		   Ex2=Ex2,
                   SE_MIN.phoshat=SE_MIN.phoshat,
                   CV_MIN.phoshat=CV_MIN.phoshat,
                   SE.phoshat=SE.phoshat,
                   CV.phoshat=CV.phoshat,
                   BIAS.phoshat=BIAS.phoshat)
 return(myres)
}

#simulate data
pbtsim <- function(phosi,Nsamp,n1,n2,lambda,ppbt){
 m <- length(lambda)
#first get binomial sample of fish marked and unmarked
 P <- sum(phosi*lambda)
 x1 <- rbinom(n=1,size=Nsamp,prob=P)
 x2 <- Nsamp-x1
#next use multinomial distribution to simulate
#how many fish are pbt and how many are not pbt
 py <- ppbt*phosi*lambda/P
 pz <- ppbt*phosi*(1-lambda)/(1-P)
 y <- rmultinom(n=1,size=min(n1,x1),prob=c(py,1-sum(py)))
 z <- rmultinom(n=1,size=min(n2,x2),prob=c(pz,1-sum(pz)))

 return(list(Nsamp=Nsamp,n1=n1,n2=n2,x1=x1,x2=x2,y=y[1:m],z=z[1:m]))
}


#Use Fisher's scoring algorithm to estimate phosi
get.estimates2 <- function(phosi,Nsamp,n1,n2,lambda,ppbt){
 NTRIAL <- 100
 tolx <- 1.e-5
#simulate data
 res <- pbtsim(phosi=phosi,Nsamp=Nsamp,n1=n1,n2=n2,lambda=lambda,ppbt=ppbt)
 x1 <- res$x1
 x2 <- res$x2
 y <- res$y
 z <- res$z
 nhatch <- length(phosi)

 w <- y+z
 iii <- (w>0)

if(sum(iii)==0){
  if(sum(lambda==mean(lambda))==nhatch){
    if(mean(lambda)>0)return((x1/Nsamp)/mean(lambda))
  }
 return(NA)
}

#reduce the dimension if zeroes present. In this case phosis associated with zeroes
#are estimated to be zero.

 y <- y[iii]
 z <- z[iii]
 ppbt <- ppbt[iii]
 lambda <- lambda[iii]
 phosi <- phosi[iii]
 nhatch=length(y)


#check for special cases
#check to see if all pbts are one (special case)
pbttest <- FALSE
if(sum(ppbt==1)==nhatch)pbttest <- TRUE
#check to see if all lambdas are zero (special case)
lambdatest <- FALSE
if(sum(lambda==0)==nhatch)lambdatest <- TRUE

#get the right Fisher Information Matrix
 if(lambdatest){
  my.getI <- getI3
 }
 else{

 if(pbttest){
  my.getI <- getI2
 }
else{
  my.getI <- getI
 }
 }

#use Fisher's scoring algorithm method to find where the partial derivatives of the 
#log-likelihood are zero. Use Fisher Information matrix in Newton's method to approx -Hessian
#set the initial guess equal to the true phosi
 phosi.hat <- phosi
 errf <- 0.0
 alpha <- 0.9
 for(ii in 1:NTRIAL){

  I <- my.getI(Nsamp,n1,n2,lambda,ppbt,phosi.hat)
  df <- dlike(phosi.hat,x1,x2,y,z,Nsamp,n1,n2,lambda,ppbt)
  if(is.na(rcond(I))){warning("condition number of I is NA");return(NA)}
  if(rcond(I)<1.e-15){warning("singular information matrix");return(NA)}
  delx <- solve(I)%*%df
  phosi.hat <- phosi.hat+delx*(1-alpha)
  errx <- sum(abs(delx))/sum(abs(phosi))
  alpha <- alpha*alpha
  if(errx<=tolx)break
 }
 if(ii==NTRIAL){warning("maximum number of iterations was reached");return(NA)}

 return(sum(phosi.hat))
}

#get gradient of the log likelihood function
#phosi is the current best estimate of phosi
dlike <- function(phosi,x1,x2,y,z,Nsamp, n1,n2,lambda,ppbt){
#estimate phosi
 sum1 <- sum(lambda*phosi)
 sum2 <- sum((1-ppbt)*lambda*phosi)
 res <- lambda*(x1-n1)/sum1-lambda*(Nsamp-x1-n2)/(1-sum1)+y/phosi
 res <- res+(n1-sum(y))*(1-ppbt)*lambda/sum2+z/phosi
 res <- res-(n2-sum(z))*(lambda*(1-ppbt)+ppbt)/(1-sum1-sum2)
 return(res)
}
