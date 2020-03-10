# For One period portfolio problemo


# Logistic Functions
L<-function(x,l=0,h=1){
	# logistic
	y<- l + (h-l)*(1/(1+exp(-x)))
	return(y)
}

Linv<-function(y,l=0,h=1){
	# invert logistic
	y<- (y-l)/(h-l) 
	x<- -log(1-y) + log(y)
	return(x)
}



Rmarket <- function(Eer){
	# defaults are globals; but we can override if needed)
	# Rf and sigma are GLOBALS (so we do not have to re-do quad all the time
	# for now theta only matters for implications of Erp 
	# maybe later for var etc.?

	rbar <- log(Rf + Eer ) - 0.5* r.sigma^2 
	N<-max(length(Eer),1)  # passing scalar sometimes crashes

	if (N == 1){
			R.m <- exp(rbar +  r.sigma*(epsilon$nodes))
	} else {
			R.m <- exp( matrix(rep(rbar, times=epsilon$n),ncol= epsilon$n) +  r.sigma*t(matrix(rep(epsilon$nodes,times=N),ncol= N)))	
	}
	# FYI
	# r.mean <- Rmarket %*% t(epsilon$weights)
		
	return(R.m)
}




SEU<-function(w,Eer,person){
	# w is length 2 here
	# (alpha^-1) * E_theta [ E[ (Rf+w(R(y)-Rf))^alpha | x ]
	#  Vector of possible Erp Tx1 ; and a vector of possible w Tx1
	# and then we can optimize all at once for a full path

	R.m <-Rmarket(Eer)  # RM comes back a N x epsilon$n

	w.all <- matrix(rep(w, times=epsilon$n),ncol= epsilon$n) 
	
	W<- Rf + w * (R.m-Rf)	
	
	U <- person$alpha^-1 * W^(person$alpha)  %*% epsilon$weights # 
	
	return(U)
}


SEU.min <-function(par, Eer, person ) {
	# Theta
	w<-L(c(par),l=wLOW,h=wHIGH)
	U <- SEU(w,Eer,person)
	score<- (-1.0)  * sum(U)   # optim does a MIN ; this assumes we have opnly >0 util 
	return(score)
 }
 
 
 wstar<-function(Eer, person){
 	w0<- Linv(0.5,l=wLOW,h=wHIGH)
	wst<-optim(par = w0, # space is in -inf..+inf
					SEU.min, 
					Eer=Eer,
        					person=person, 
        					control=list(maxit=1000),
        					method="Brent",lower=-15,upper=+15 # L(-15:15,l=wLOW,h=wHIGH) gets close 
 					  )
 					  
	w.star<-L(wst$par,l=wLOW,h=wHIGH)
 	return(w.star)
 }
 
 
 LogReturn.p <- function(w,r) {
 	# end of period wealth for a w and r VECTORS
 	# below we use apply
 	
 	Rp= Rf+ w * (exp(r)-Rf)
 	
 	Rp[which(Rp<=0)] <- 0  # bankrupt!
 	
 	logRp<- log(Rp)
 	
 	return(logRp)
 }
 
 Performance.p <- function( w , t=NULL) {
 	# do this for annual holding period
 	# assume you update your portfolio 1/12 each month ... sort of a rolling thing
 	r<-shift( Data$eR_Market.A ,n=+12)
 	
 	logRp <- apply(w,2, LogReturn.p , r=r)   #fix for bankrupt....

	logW <- apply(logRp ,2, function(x)  {cumsum(x) / 12})  # annual->monthly that is why/12

	logW.test<-logW
	if (!is.null(t)){
			logW.test<-apply(logW,2,function(x) {x/x[max(t)]})
	}
 	
 	return(
 		list(
 			logRp=logRp,
 			logW=logW,
 			logW.test=logW.test,
 			date=Data$date # not sure if needed; just in case
 		)
 	)	
 }
 
Performance.Long <- function(w,P,model.sum){
	

	
	z<- data.frame(cbind(Data$year,Data$m,w))
	colnames(z)<-c("year","month",models)
	z<-melt(z,id.vars=c("year","month"),variable.name="model",value.name="weight")

	Z<-z
	z<- data.frame(cbind(Data$year,Data$m,P$logRp))
	colnames(z)<-c("year","month",models)
	z<-melt(z,id.vars=c("year","month"),variable.name="model",value.name="Rp")
	Z<-cbind(Z,z)

	z<- data.frame(cbind(Data$year,Data$m,P$logW))
	colnames(z)<-c("year","month",models)
	z<-melt(z,id.vars=c("year","month"),variable.name="model",value.name="logW")
	Z<-cbind(Z,z)

	z<- data.frame(cbind(Data$year,Data$m,P$logW.test))
	colnames(z)<-c("year","month",models)
	z<-melt(z,id.vars=c("year","month"),variable.name="model",value.name="logW.test")
	Z<-cbind(Z,z)
	
	N<-length(Data$year)
	
	# keep track of meta data (duplicate but handy)
	
	z<- data.frame(cbind(Data$year,Data$m, rep.row(model.sum["Model.a",],N)))
	colnames(z)<-c("year","month",models)
	z<-melt(z,id.vars=c("year","month"),variable.name="model",value.name="Model.a")
	Z<-cbind(Z,z)

	z<- data.frame(cbind(Data$year,Data$m, rep.row(model.sum["Model.logb",],N)))
	colnames(z)<-c("year","month",models)
	z<-melt(z,id.vars=c("year","month"),variable.name="model",value.name="Model.logb")
	Z<-cbind(Z,z)	

	z<- data.frame(cbind(Data$year,Data$m, rep.row(model.sum["nzero",],N)))
	colnames(z)<-c("year","month",models)
	z<-melt(z,id.vars=c("year","month"),variable.name="model",value.name="nzero")
	Z<-cbind(Z,z)

	Z$t <- Z$year + (Z$month-0.5)/12

	Z<-Z[,c("t","year","month","model","Model.a","Model.logb","nzero","weight","Rp","logW","logW.test")]
	
	return(Z)
}
 
 

 