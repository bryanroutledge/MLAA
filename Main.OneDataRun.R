#
#
#
#   MAIN LOOP TO SOLVE FOR ONE SET OF X DATA
#   EVERYTHING IS GLOBAL
#
source("Load.returns.R")

if (Data.Type == "fred"){
	source("Load.fred.R")
} else if (Data.Type == "text"){
	source("Load.textdata.R")	
} else {
	print("NO DATA TYPE.... boom")
}


# Check to make sure indexs line up
if (all(x.Data.index$date == y.Data.index$date)){
	print("Dates match OK")
	Data<-y.Data.index 
} else {
	print("DATES NOT MATCH !!!!!!!!")	
	stop("DATES NOT MATCH !!!!!!!!")
	
}

# drop things if you like?


# Data defined when loading returns 
t.train <- which(Data$year>=TRAIN.b & Data$year<=TRAIN.e)
print(paste(c("DATA:",length(Data$year),
			  "(",min(Data$year),"-",max(Data$year),")",
			" TRAIN:",length(t.train),
			  "(",TRAIN.b,"-",TRAIN.e,")"			
			),collapse=""))



# need to fix this .... for x and y data ...
if (OVERLAP){
	# 	find overlap
	#Data<-Data[rowSums(is.na(Data[,c(y.index,x.index)])) == 0,]  # non-overlapping
	#head(Data)
	print("OVERLAP CHECKING NOT WORKING")
} else {
	x[is.na(x)] <- 0
	y[is.na(y)] <- 0	
}



ALL.sum<-c()
ALL.P  <-data.frame(NULL)
ALL.theta<-c()

for (a in seq(0,1.0, length.out=TRY.a)){
	# ALPHA=1 -->LASSO  ALPHA=0-->ridge l2
	# CV seems to give you a bit better ?
	fit=cv.glmnet(
		x=x[t.train,],    
		y=y[t.train,], 
		alpha=a , family="gaussian")


	# This has fit for whole path of lambda (b in my paper); which ones to use?


	# SUMMARIZE THE MODEL
	# using broom 
	results <- tidy(fit)                      # mse for all lambda etc.
	results.g <-glance(fit) 	 			  # gives min lambda and min mse 	
	results.g$lambda.1se.left <- 2*results.g$lambda.min - results.g$lambda.1se 
	results.coef <-  tidy(fit$glmnet.fit)      # All the coefs of all the models


	# which models are most interesting are the ones that have sensible 
	# predictions for w.  So lets calc for all w and then
	# pick models that do not hit min/max bounds for w*
	# that is arbitrary but seens a good screen
	# to be fair, screan ONLY on TRAIN DATA.

	# want to solve for optimal portfolio one at time 
	# since theta(t) does not matter for theta(~t) it is takint to long to get x-derivatives
	
	# PREDITION	
	hat<-predict(fit,newx=x,s=results$lambda) 
	
	# Make Annual
	# H.adj depends in if returns are already 12 months or not
	Eer <- hat  * H.adj

	# Cap the stoopid ones ... should not matter; these are not interesing people
	Eer[which(Eer< er.min)]<- er.min
	Eer[which(Eer> er.max)]<- er.max
	
	# to find reasonable people, I can look juat at the min and max
	Eer.bound<-colMins(Eer[t.train,])
	Eer.bound<-rbind(Eer.bound,colMaxs(Eer[t.train,]))

	w<-lapply(Eer.bound,wstar,person=person)    # list lapply since one at time (not by col or by row)
	w<-matrix(as.numeric(w),ncol=length(results$lambda)) # as.numeric to get rid of strings

	interesting.i <- which(colMins(w) > 0.99* wLOW  & colMins(w) < 0.99* wHIGH)
	if (length(interesting.i)>TRY.b){
		interesting.i<-unique(round(seq(min(interesting.i), max(interesting.i), length.out = TRY.b)))		
	}

	best.i <- closest(results$lambda, results.g$lambda.min)$i # keep index	
	interesting.i <- unique(c(best.i,interesting.i))		     
			       
	interesting <- results$lambda[interesting.i]
	
	# name the examples	
	models<-unlist(lapply(interesting,function(x) {
				paste(c("a=",sprintf("%.4f",a),"_logb=",sprintf("%.4f",log(x))),collapse="")
				}
			))	

	# USING TIDY... HANDY FOR ONE MODEL .... 
	results.interesting.row<-results[interesting.i,]
	
	results.interesting <- t(results.interesting.row)
	colnames(results.interesting)<-models

	# also store the "a" and "log b" in the 
	Model.a <-rep(a,times=length(interesting))
	Model.logb<-log(interesting)
	results.interesting <- rbind(Model.a,Model.logb,results.interesting)
	

	# TIDYING MYSELF ... keeps all together
	# all three model COEFS as one Matrix
	theta <- coef(fit,s=interesting)
	theta<-as.data.frame(as.matrix(theta))

	# name cols for keep across runs
	colnames(theta)<-models
	
	k<-which(rowSums(abs(theta))==0)
	if (length(k)>0){
		theta.sparse<-theta[-k,]  # if you keep all you can merge across models of alpha?
	} else {
		theta.sparse<-theta
	}

	# PREDITION	
	hat <- hat[,interesting.i] 	
	Eer <- Eer[,interesting.i]  # carve down to just the 5 or so models

	w<-lapply(Eer,wstar,person=person)    # list lapply since one at time (not by col or by row)
	w<-matrix(as.numeric(w),ncol=length(interesting)) # as.numeric to get rid of strings


	P<-Performance.p(w ,t.train )  # wealth given w; returns etc.	
	
	P.df <- Performance.Long(w,P, results.interesting)   #convert to long format data.frame
	ALL.P<-rbind(ALL.P,P.df)  # NOTE rows

	# Summary Stats (do on hat... Then adjust!)
	s<- Sample.Stats(hat,t=t.train,name="Eer") 
	results.interesting<-rbind(results.interesting,s$mean,s$sd)


	s<- Sample.Stats(w,t=t.train,name="weight.in.Rm") 
	results.interesting<-rbind(results.interesting,s$mean,s$sd)

	s<- Sample.Stats(P$logRp,t=t.train,name="Rp") 
	results.interesting<-rbind(results.interesting,s$mean,s$sd)


	# Plot the weights and wealths ....




	# FOR PRINT OUT 
	theta.terms<-length(theta[,1])
	show<-c(1:min(theta.terms,10))
		
	
	print(paste(c("a=",a),collapse=""))
	SHOW<- rbind(results.interesting,
				theta.sparse[order(-theta.sparse[,1]),][show,], # biggest	
				theta.sparse[order( theta.sparse[,1]),][show,] # smallest		
		)

	print(format(SHOW,digits = 3,scientific = 2))
	
	
	# SAVE THINGS FOR LATEX
	# (1) SHOW -> table
	# (2) Stats -> tabular only
	# (3) coefs -> tabulr only 
	# xtable outputs for LATEX
	# Keep in Data Run.  One of these for each a=
	ftex.stub <-paste(c(Run.dir,"/",Data.Name,".a",a),collapse="")
	
	ftex <-paste(c(ftex.stub,".tex"),collapse="")
	print(xtable(SHOW,floating=FALSE,digits=5),file=ftex)
	
	ftex <-paste(c(ftex.stub,".stats.tex"),collapse="")
	print(xtable(results.interesting,floating=FALSE,digits=5),file=ftex,only.contents=TRUE)
	
	ftex <-paste(c(ftex.stub,".big.coef.tex"),collapse="")
	print(xtable(theta.sparse[order(-theta.sparse[,1]),][show,],floating=FALSE,digits=5),file=ftex, only.contents=TRUE)
	ftex <-paste(c(ftex.stub,".small.coef.tex"),collapse="")
	print(xtable(theta.sparse[order(-theta.sparse[,1]),][show,],floating=FALSE,digits=5),file=ftex,only.contents=TRUE)
	
	
	show<-c(1:min(theta.terms,50))
	ftex <-paste(c(ftex.stub,".50big.coef.tex"),collapse="")
	print(xtable(theta.sparse[order(-theta.sparse[,1]),][show,],floating=FALSE,digits=5),file=ftex, only.contents=TRUE)
	ftex <-paste(c(ftex.stub,".50small.coef.tex"),collapse="")
	print(xtable(theta.sparse[order(theta.sparse[,1]),][show,],floating=FALSE,digits=5),file=ftex,only.contents=TRUE)
		
	print(paste(c("TEX saved to:",ftex.stub),collapse=""))	
	
	
	#Save all
	ALL.theta <-cbind(ALL.theta,as.matrix(theta)) 

	Dataset<-rep(Data.Name,times=dim(results.interesting)[2])
	sum<-data.frame(t(results.interesting))  # note the t
	sum<-cbind(Dataset,sum)

	ALL.sum <- rbind(ALL.sum,sum)

}	




# was converting numbers to stings!
#z<-colnames(ALL.sum) #stop the abbrevitions
#ALL.sum<-data.frame(ALL.sum)  # it was a matrix to this point	
#colnames(ALL.sum)<-z  
#ALL.theta<-data.frame(ALL.theta)
#colnames(ALL.theta)<-z  


# Add the dataset names
ALL.P$Dataset<-Data.Name
Dataset<-rep(Data.Name,times=dim(ALL.sum)[2])

#ALL.sum <- rbind(Dataset,ALL.sum)
#rownames(ALL.sum)[1]<-"Dataset"



#print(format(ALL.sum,digits = 3,scientific = 2))		
# Run the glmnet regression
# do some predicitons out of sample
# solve for the portfolios

