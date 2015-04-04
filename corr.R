corr<-function(directory,threshold=0){
	 fn <- dir(directory)
  	 idx <- as.numeric(substr(fn,1,3))
     fn <- paste(directory,"/",fn,sep="")
	 result<-numeric(0)
	 for (file in fn){
		data<-read.csv(file)
		good<-complete.cases(data)
		data<-data[good,]
		#print(data)
		r<-nrow(data)
		if(r>=threshold){
			#print(data$sulfate)
			#print(data$nitrate)
			c<-cor(data$sulfate,data$nitrate)
			result<-c(result,c)
	    }	
     } 
	return(result)
}