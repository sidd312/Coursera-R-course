pollutantmean<- function(directory,pollutant,id=1:332){
	files_list<- list.files(directory)
	file.names=as.numeric(sub("\\.csv$","",files_list))
	selected.files=files_list[file.names[id]]
	data=lapply(file.path(directory,selected.files),read.csv)
	data=do.call(rbind.data.frame,data)
	round(mean(data[,pollutant],na.rm=TRUE),digits=3)
}