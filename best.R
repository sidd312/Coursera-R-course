best <-function(state,outcome){
	data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
	states<-unique(data$State)
	if(!(state %in% states)){
		stop("invalid state")
	}
	outcomes<-c("heart attack","heart failure","pneumonia")
	if(!(outcome %in% outcomes)){
		stop("invalid outcome")
	}
	#11,17,23
	df<-data[data$State==state,c(2,11,17,23)]
	#df<-df[df[,2]!="Not Available",]
	#df<-df[df[,3]!="Not Available",]
	#df<-df[df[,4]!="Not Available",]
	df[,c(2,3,4)]<-sapply(df[,c(2,3,4)],as.numeric)
	df<-df[order(df[,1]),]
	if(outcome=="heart attack"){
		best<-df[which.min(df[,2]),"Hospital.Name"]
	}
	else if(outcome=="heart failure"){
		best<-df[which.min(df[,3]),"Hospital.Name"]
	}
	else if(outcome=="pneumonia"){
		best<-df[which.min(df[,4]),"Hospital.Name"]
	}
	best
}