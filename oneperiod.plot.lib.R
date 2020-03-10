# Plot things


# Most of the data sets are GLOBAL
# just the parms to plot we can pass in ....

f.pdf<-function(
	file,
	dir="Graphics"
	)
	#  Gets you back FRED.PDF even if you gave it FRED.PDF or FRED
	{
		f<-paste(c(dir,"/",file,".pdf"),collapse="")
		f<-sub("(pdf.)+pdf", "pdf", f, ignore.case = TRUE, perl = TRUE)
		print(paste(c("TO FILE:",f),collapse=""))
		return(f)
	}

gg_color_hue <- function(n,alpha=1.0) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100, alpha=alpha)[1:n]
}


Plot.w <- function
	(
	 P=ALL.P,     
	 yplot="weight", 
	 a.lim=c(0,1),
	 logb.lim=c(-Inf,Inf),
	 nzero.lim=c(0,Inf),
	 year.lim=c(1900,2100),
	 Dataset="ALL",
	 HideModel="FALSE",
	 file="",
	 dir="Graphics"
	 )
{



	P <- P[which(P$Model.a >= min(a.lim) & P$Model.a <= max(a.lim)),]
	P <- P[which(P$Model.logb >= min(logb.lim) & P$Model.logb <= max(logb.lim)),]
	P <- P[which(P$nzero >= min(nzero.lim) & P$nzero <= max(nzero.lim)),]
	P <- P[which(P$year >= min(year.lim) & P$year <= max(year.lim)),]


	

	print(dim(P))

	print("MODELS TO PLOT:")
	models<-unique(P$model)
	models.n<-length(models)
	print(models)
	print(models.n)
	
	model.plot=unlist(lapply(models,function(x) {which(colnames(ALL.sum)==x)}))
	if (length(model.plot)==1){
		model.plot=c(model.plot,model.plot) #makes things print nice
	}
	print(model.plot)

	print(ALL.sum[,model.plot])
	



	

	
		p<- qplot(
				x= t, y = eval(parse(text=yplot))  , data = P, 
				geom="line",
				color=Dataset,
				linetype=model
				
				)
			p<-p+labs(x="Date", y=yplot)
			p<-p+labs(x="Date", y=yplot)


		if (HideModel){	
			p<-p+scale_linetype_manual(values=rep("solid",times=models.n))
			p<-p+ guides(linetype=FALSE)
		#	p<-p+ guides(color=FALSE)
		}	
		
		Palette <- gg_color_hue(4)
		Palette <- c( "#E69F00", "#56B4E9", "#009E73", "#000000","#F0E442", "#0072B2", "#D55E00", "#CC79A7")		
		if (Dataset =="z.Buy+Hold"){
			Palette <- c( "#E69F0000", "#56B4E900", "#009E7300", "#000000")				
		}
		if (Dataset =="Alan.10000"){
			Palette <- c( "#E69F00", "#56B4E900", "#009E7300", "#00000000")				
		}	
		if (Dataset =="Bob"){
			Palette <- c( "#E69F0000", "#56B4E9", "#009E7300", "#00000000")				
		}	
		if (Dataset =="Fred"){
			Palette <- c( "#E69F0000", "#56B4E900", "#009E73", "#00000000")				
		}				
		p<-p+ scale_colour_manual(values=Palette)


		#	p<-p+labs(title=title)		

		
	print(p)
	
	if (file!=""){

		file<-paste(c(file,".",Dataset),collapse="")
	
		f<-f.pdf(file,dir=dir)
		pdf(f) 
		print(p)
		dev.off()
	}
	
	return(p)

}

## PLOT SUMMARIES ...
Plot.SUM <- function(
	 P=ALL.SUM,     
	 yplot="nzero", 
	 file="",
	 dir="Graphics"
	 ){

	p<- qplot(
		x= Model.logb, y = eval(parse(text=yplot))  , data = P,
		linetype=Dataset,
		size=Model.a,
		color=Dataset
		)
	p<-p+labs(x="log b", y=yplot)	
	p<-p+scale_size(range = c(5, 10))


	if (file==""){
		print(p)
	}else{
		f<-paste(c(file,".",yplot),collapse="")
		f<-f.pdf(f,dir=dir)
		pdf(f) 
		print(p)
		dev.off()	
		print(p)
	}
}

## PLOT WORD CLOUDS OF THETAS

Fred.Describe<-function(
		w
	)
{	
	# build some word clouds for FRED?  need real descriptions
	#./fred.makeindex.pl >Fred.index.txt
	# could load as global if this happens to be too slow
	Fred.Index <- read.table("Fred.index.txt",
      	  					header=TRUE,
        						stringsAsFactors = FALSE,  # make strings real strings
        						sep="\t",
        						#nrows=4  # to load part of file
        					) 
	
	for (j in (1:length(w))){
			d <- which(Fred.Index[,1]==w[j])
			if (length(d)>=1){
				w[j] <- Fred.Index[d,2]
			}
	}
	return(w)
}


Plot.Word.Cloud <- function(
	Theta.Many,    # Send in many of them 
	col.to.plot,
	Dataset="Alan.10000",
	scale = c(4, 0.4),
	file="",
	dir="Graphics",
	Er.bigger.color='red',  #RED is HOT   ?
	Er.smaller.color='blue' #BLUE is COOL ?
){
	print(dim(Theta.Many))
	print(col.to.plot)
	theta<- Theta.Many[,col.to.plot]
	names(theta)<-rownames(Theta.Many)
	model<-names(Theta.Many)[col.to.plot]
	title.f <- paste(c(Dataset,".",model),collapse="")

	theta<-theta[which(theta!=0)]
	theta<-theta[which(abs(theta)> 1.0e-10)]	
	theta<-theta[which(names(theta)!="(Intercept)")]

	F<- floor(abs(theta) * 10000000)
	title <- paste(c(title.f," nzero=",length(F)),collapse="")	
	print(title)
	
	if (length(F)>=2){
		words<-names(theta)
		
		
		if (Dataset=="Fred" || Dataset=="Bob"){
			print("adding desriptions")
			words <- Fred.Describe(words)
		}

		word.color<-rep(Er.smaller.color,times=length(F))
		word.color[which(theta>0)] <- Er.bigger.color
				
		
		if (file==""){
			print(title)
		}else{
			f<-paste(c(file,".",title.f),collapse="")
			f<-f.pdf(f,dir=dir)
			pdf(f) 
		}
			
		wordcloud(words=words,freq=F,colors=word.color,scale = c(4, 0.4))
		text(x=0.5, y=1.0, title)

		if (file==""){
		}else{
			dev.off()
		}
		

		
	}
	
	
}






