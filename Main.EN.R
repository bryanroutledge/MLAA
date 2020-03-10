# Elastic Net regerssion and then portfolio
# routledge 2015
# key input is glmnet package 
# NOTE: scaled by number of observtions so becareful looking across data of diff sizes
#



## HEADER FOR SAVING THINGS
NEW  <- TRUE  # clear the ALL.P and such
SAVE <- TRUE  # Dump the R.workspace for each run
Run.dir <- ""  # Leave blank to create new with date

# remove most things.
if ( NEW ) {
	#  CLEAR THE BUNDLES
	ALL.DATA.P<-c()
	ALL.SUM<-c()  
	
	Run.date<- Sys.time()	
	Run.date<-format(Run.date, format="%Y-%m-%d-%H-%M-%S")
	Run.dir <- paste(c("RUN.",Run.date),collapse="")	
	dir.create(Run.dir)
	print(paste(c("RESULTS SAVED IN: ", Run.dir),collapse=""))
}


require("glmnet")
require("lars") # a lasso package
# require("zoo")  # for time series dates and such

require("statmod")       # for Quadrature
# require("quantmod")
require("ggplot2")
#require("grid")  # should be now preinstalled? May not be needed
library("broom")  # tidy(glmnet model) into a df
library("xtable") # For print out data frames to latex table
library("matrixStats") # for colSds 
library("reshape2")    # for melt "wide" to "long" http://seananderson.ca/2013/10/19/reshape.html
library("wordcloud")  
#wordcloud(words=c('a','b'),freq=c(1,2),colors=c('red','green'))


source("/Users/rout/Desktop/Dropbox/R_lib/bryan_lib.R")
source("data.lib.R")
source("oneperiod.lib.R")
source("oneperiod.plot.lib.R")



# ECONOMY PARAMETERS
# log R ~ N ( Rf + r.premium(y), r.sigma)   
# Some algebra ....  
# E[R-Rf] = 0.06 --> E[exp(y+s eps)]=.06+Rf 
#    --> exp(y+.5s^2)=.06+Rf -> y=log(.06+Rf) - 0.5s^2
# check Wealth if w=1 should be 7% ave rate of return
# r.premium(y) <- from the forecast model and data x

Rf<-1.0 
r.sigma    =  0.20
# r.mean SEE FUNCTION in lib file

er.min= -0.9 #goofy low  -90% expected! return
er.max= +0.9  # goofy high  

# Get the epsilons (use ODD so you get one near zero)
epsilon.quad<-5
epsilon<-gauss.quad.prob(epsilon.quad,dist="normal",mu=0,sigma=1)
epsilon$n<-epsilon.quad



person<-list(alpha=-3.0)
wLOW  <- -0.90 # DO NOT MAKE this zero; ZERO need to be reachable with L(x)
wHIGH <- 1.75

# RUN parameters
# replace na->0 == OVERLAP FALSE 
OVERLAP<-FALSE
TRAIN.b<-1952
TRAIN.e<-2005


TRY.a <- 5
TRY.b <- 5  # how many models for b (lambda) to look at

FORECAST.annual=TRUE;



pawse()


### RUN MANY DATA SETS AND BUNDLE THINGS..... 

# THE y DATA
Returns.File <- "Data/PriceDiv.ReturnsData.txt"

# hacj out buy hold
# make data const.... Does not work need a epslion 
# or just save some one?

# THE x DATA
Data.Type<-"fred"  
Data.File<-"Data/PriceDiv.Data.txt"
Data.Name<-"Bob"

print(paste("RUNNING:",Data.Name,collapse=""))
source("Main.OneDataRun.R")
source("Main.GatherRun.R")


Data.Type<-"fred"  
Data.File<-"Data/FredLudvigsonNg.Data.txt"
Data.Name<-"Fred"

print(paste("RUNNING:",Data.Name,collapse=""))
source("Main.OneDataRun.R")
source("Main.GatherRun.R")

print("NOTE!! ONLY RUNNING A")
if (FALSE){
Data.Type<-"text"  
Text.Vocab.File <- "vocab.index.txt"
Text.Data.File  <- "BB.Text.Data.txt"
Text.Index.File  <- "Date.Index.txt"
Text.NORMALIZE <- TRUE
Data.Name<-"Alan.1000"

print(paste("RUNNING:",Data.Name,collapse=""))
source("Main.OneDataRun.R")
source("Main.GatherRun.R")


Data.Type<-"text"  
Text.Vocab.File <- "vocab.index.5000.txt"
Text.Data.File  <- "BB.Text.Data.5000.txt"
Text.Index.File  <- "Date.Index.txt"
Text.NORMALIZE <- TRUE
Data.Name<-"Alan.5000"


print(paste("RUNNING:",Data.Name,collapse=""))
source("Main.OneDataRun.R")
source("Main.GatherRun.R")
}

Data.Type<-"text"  
Text.Vocab.File <- "vocab.index.10000.txt"
Text.Data.File  <- "BB.Text.Data.10000.txt"
Text.Index.File  <- "Date.Index.txt"
Text.NORMALIZE <- TRUE
Data.Name<-"Alan.10000"


print(paste("RUNNING:",Data.Name,collapse=""))
source("Main.OneDataRun.R")
source("Main.GatherRun.R")


# Need a hack for BUY and HOLD in there.... 
print("A hack to get buy and hold... largest log.b")
Buy.Hold<-ALL.DATA.P[which(ALL.DATA.P$Model.logb==max(ALL.DATA.P$Model.logb)),]
print(paste(c(length(Buy.Hold$weight),mean(Buy.Hold$weight),sd(Buy.Hold$weight)),collapse=" "))

Buy.Hold$Dataset<-"z.Buy+Hold"
ALL.DATA.P <- rbind(ALL.DATA.P,Buy.Hold)  # NOTE R BIND

# NEED TO MOVE SOMETHING OVER TO ALL.SUM 

###

print("DONE WITH most things....")

# Some interesting plots .... 
run.this<-paste(c("./Fix.xtabletex.sh ",Run.dir),collapse="")
print(paste(c("RUNNING PERL: ",run.this),collapse=""))

run.this<-paste(c("./Gather.WordPlots.sh ",Run.dir),collapse="")
print(paste(c("Run this to get plots ",run.this),collapse=""))
