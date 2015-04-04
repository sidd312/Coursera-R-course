rankhospital<-function(state,outcome,num){
	source("best.R")
	source("worst.R")
	data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
	states<-unique(data$State)
	if(!(state %in% states)){
		stop("invalid state")
	}
	outcomes<-c("heart attack","heart failure","pneumonia")
	if(!(outcome %in% outcomes)){
		stop("invalid outcome")
	}
	df<-data[data$State==state,c(2,11,17,23)]
	if(num=="best"){
		ranked_hospital<-best(state,outcome)
	}
	else if(num=="worst"){
		ranked_hospital<-worst(state,outcome)
	}
	else if(num>nrow(df)){
		ranked_hospital<-"NA"
	}
	else{
		#df<-df[df[,2]!="Not Available",]
		#df<-df[df[,3]!="Not Available",]
		#df<-df[df[,4]!="Not Available",]
		df[,c(2,3,4)]<-sapply(df[,c(2,3,4)],as.numeric)
		#df<-df[order(df[,1]),]
		if(outcome=="heart attack"){
			df<-df[order(df[,2],df[,1],na.last=NA),]
			#df<-df[order(df[,1]),]
			ranked_hospital<-df[num,1]
		}
		else if(outcome=="heart failure"){
			df<-df[order(df[,3],df[,1],na.last=NA),]
			#df<-df[order(df[,1]),]
			ranked_hospital<-df[num,1]
		}
		else if(outcome=="pneumonia"){
			df<-df[order(df[,4],df[,1],na.last=NA),]
			#df<-df[order(df[,1]),]
			ranked_hospital<-df[num,1]
		}
	}
	ranked_hospital
}