complete<-function(directory,id=1:332){
	files_list <-list.files(directory)
	files.names=as.numeric(sub("\\.csv$","",files_list))
	selected.files=files_list[id]
	data=lapply(file.path(directory,selected.files),read.csv)
	count<-numeric(0)
	for (i in seq_along(data)){
		k<-data[[i]]
		good=complete.cases(k)
		count<-c(count,nrow(k[good,]))
		#print(nrow(k[good,]))
	}
	data.frame(id=id,nobs=count)
}