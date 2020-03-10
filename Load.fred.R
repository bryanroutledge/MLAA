# Load numerical x data
# all global


X.Data <- read.table(Data.File,
	header=TRUE,
	stringsAsFactors = FALSE,  # make strings real strings
	sep="\t",
	#nrows=400  # to load part of file
	)	

# Pick the X variables before creating the y
Use.not.names=c(
			"date","year","month",
			"eR_Market",    # let's not use this.
			"NPPTTL_level_log_dif_z"   # not available until 2001
		)
		
Use.not<-unlist(lapply(Use.not.names, function(x) {which(colnames(X.Data)==x)}))
x.index <-c(1:length(colnames(X.Data)))
x.index <- x.index[-Use.not]

#prune early
X.Data<-X.Data[which(X.Data$year>=TRAIN.b),]


# Trim the x to mathc the y  HACK!
if (dim(X.Data)[1]>length(y)){
	print("TOO MUCH X DATA")
	X.Data <- X.Data[seq(1,length(y)),]
}

Split<-rep("TRAIN",times=N)
Split[which(X.Data$year>TRAIN.e)]<-"TEST"
N<-length(X.Data$date)
index<-c(1:N)
x.Data.index<-data.frame(cbind(index,
			X.Data[,c("date","year","month")],
		Split))

x=as.matrix(cbind(X.Data[,x.index]))