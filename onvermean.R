## overmean function

overmean <- function(data)
        {
	m <- apply(data,1, mean)
    	meansignal<-m[!is.na(m)]	
    	baselinemean<-mean(meansignal)
    	meansignal<-data.frame(meansignal)
      
    	combined<-merge(meansignal,data,by="row.names")
	
	    row.names(combined)<-combined$Row.names
		combined$Row.names<-NULL
	

	v <- apply(combined[,2:ncol(combined)],1, var)
	varsignal<-v[!is.na(v)]
    	baselinevar<-mean(varsignal)
    	varsignal<-data.frame(varsignal)
        varsignal
    	recombined<-merge(varsignal,combined,by="row.names")

	    row.names(recombined)<-recombined$Row.names
		recombined$Row.names<-NULL
	recombined<-recombined[!(row.names(recombined) ==""),]

	    combord<-recombined[with(recombined,order(-varsignal)),]
	    #colord<-ncol(combord)
	    #combord<-combord[,3:colord]
	    
    	plot(recombined$meansignal,recombined$varsignal,main= "Selection of variable high expressed genes",
	ylab= "Variance of Signal",xlab= "Mean of Signal")
    	abline(h=baselinevar,col="blue",lty=3)
	    abline(v=baselinemean,col="red",lty=1)
    	sub<-subset(combord, combord$varsignal > baselinevar & combord$meansignal > baselinemean)
    	head(sub,n=30)[,1:3]
	    nrow(sub)
    	sub
        }
