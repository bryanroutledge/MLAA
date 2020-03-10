# Data lib

# Time series
# example
# year / month / er eR LAG / er LEAD
# cbind(Data$year,Data$month,Data$eR_Market,shift(Data$eR_Market,-12),shift(Data$eR_Market,+12))
shift <- function(x, n){
	if (n<0){
		# n<0 .... lag
		n <-  (-1)*n
		x <- c(rep(NA,n), head(x, -n)) 	
	} else if (n>0)  {
		# n>0 .... lead
		x <- c( tail(x, -n), rep(NA,n))
	}
	return(x)
}

# check
# cbind(Data$year,Data$month,Data$eR_Market,rolling(Data$eR_Market,n=1),rolling(Data$eR_Market,n=12))
rolling <- function(x, n){
	# rolling 12 month return
	# n=1 should return x
	A=cumsum(x)
	A.n=shift(A, -1.0*n)
	
	x<- A - A.n
	
	return(x)
	
}

closest <- function(x , t){
	# x = vector ; t = scalar
	# find which one has x == t or close to it
	m<-abs(x - t)
	i<-which(m == min(m))	
	return(list(i=i,x=x[i]))
}


Sample.Stats <- function(x, t=NULL, name=NULL) {
	# columns to take a mean sd of
	# do: ALL train=t and test=-t 

	if (!is.null(name)){
		Names=c(paste(c(name,".full"),collapse=""),
				paste(c(name,".train"),collapse=""),
				paste(c(name,".test"),collapse="")
				)
	}else{
		Names=rep("x.",times=3)
	}
	

	m.all <- colMeans(x, na.rm=TRUE)	
	m.train   <- colMeans(x[t,], na.rm=TRUE)		
	m.test   <- colMeans(x[-t,], na.rm=TRUE)	
	mean<-rbind(m.all,m.train,m.test)	

	N<-unlist(lapply(Names,function(x) {paste(c(x,".mean"),collapse="")}))
	rownames(mean)<-N
	

	sd.all <- colSds(x, na.rm=TRUE)
	sd.train <- colSds(x[t,], na.rm=TRUE)
	sd.test <- colSds(x[-t,], na.rm=TRUE)
	sd<-rbind(sd.all,sd.train,sd.test)

	N<-unlist(lapply(Names,function(x) {paste(c(x,".sd"),collapse="")}))	
	rownames(sd)<-N	
	
	return(list(mean=mean,sd=sd))
}


# http://www.r-bloggers.com/a-quick-way-to-do-row-repeat-and-col-repeat-rep-row-rep-col/
rep.row<-function(x,n){
   matrix(rep(x,each=n),nrow=n)
}

rep.col<-function(x,n){
   matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

norm.sparse<-function(x){
	# make mean 0 var 1 on those that are not zero
	x.nz<-x[which(x!=0)]
	m<-mean(x.nz)
	s<-sd(x.nz)
	x[x!=0] <- (x[x!=0] - m)/s 
	return(x)
}




