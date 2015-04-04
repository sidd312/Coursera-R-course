rankall <-function(outcome,num="best"){
	data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
	states<-sort(unique(data$State))
	outcomes<-c("heart attack","heart failure","pneumonia")
	if(!(outcome %in% outcomes)){
		stop("invalid outcome")
	}
	source("rankhospital.R")
	hospital<-character()
	states_list<-character()
	for(state in states){
		hospital<-c(hospital,rankhospital(state,outcome,num))
		states_list<-c(states_list,state)
	}
	df<-data.frame(hospital=hospital,state=states_list)
	df
}