# SAVe stuff
#
ALL.DATA.P <- rbind(ALL.DATA.P,ALL.P)  # NOTE R BIND
ALL.SUM <- rbind(ALL.SUM,ALL.sum)


f<-paste(c(Run.dir,"/",Data.Name,".Rdata"),collapse="")

print(paste(c("SAVING TO: ",f),collapse=""))
save.image(file=f)

if (Data.Name=="Bob"){
	bob.theta<-data.frame(ALL.theta)
}


if (Data.Name=="Fred"){
	fred.theta<-data.frame(ALL.theta)
}

# Dump all the text coefs to a table
if (Data.Name=="Alan.10000"){
	f<-paste(c(Run.dir,"/",Data.Name,".theta.txt"),collapse="")
 	write.table(ALL.theta,file=f,quote=FALSE,sep="\t")

	alan.theta<-data.frame(ALL.theta)
 	for(i in names(alan.theta)){	
 		f<-paste(c(Run.dir,"/",Data.Name,i,".THETA.txt"),collapse="")
 		write.table(ALL.theta[,which(names(alan.theta)==i)],file=f,quote=FALSE,sep="\t") 		
 	}	
}
