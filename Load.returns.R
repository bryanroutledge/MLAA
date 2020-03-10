# Load the Numerical (ECON) Data
# THE y data (returns)
# everything is global 

Returns.Data <- read.table(Returns.File,
	header=TRUE,
	stringsAsFactors = FALSE,  # make strings real strings
	sep="\t",
	#nrows=400  # to load part of file
)
# Prune EARLY data
Returns.Data<-Returns.Data[which(Returns.Data$year>=TRAIN.b),]
N<-length(Returns.Data$date)

Split<-rep("TRAIN",times=N)
Split[which(Returns.Data$year>TRAIN.e)]<-"TEST"


# LEAD 	(see shift)
### FORECAST THE RETURN _in_ the MONTH 12 months from now 
Returns.Data$eR_Market.plus12 <- shift(Returns.Data$eR_Market,n=12)

#Forecast the ONE YEAR return from today
Returns.Data$eR_Market.A <- rolling(Returns.Data$eR_Market,n=12)  
Returns.Data$eR_Market.A.plus12 <- shift(Returns.Data$eR_Market.A,n=12)


if (FORECAST.annual){
	y.index<-which( colnames(Returns.Data)=="eR_Market.A.plus12" )
	H.adj<-1
} else {
	y.index<-which( colnames(Returns.Data)=="eR_Market.plus12" )
	H.adj<-1
}

# Date index info
index<-c(1:N)
y.Data.index<-data.frame(cbind(index,Returns.Data,Split))

# THE KEY ITEM .... 
y=cbind(Returns.Data[,y.index])




