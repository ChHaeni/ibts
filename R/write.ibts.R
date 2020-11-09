write.ibts <-
function(x,file="",format="%Y-%m-%d %H:%M:%OS3",st_et_names=c("st","et"),sep=";",row.names=FALSE,tz=tzone(x),addCoverage=FALSE,...){
	out <- cbind(st=format(st(x),format=format,tz=tz),et=format(et(x),format=format,tz=tz),as.data.frame(x,keepAtts=FALSE))
	names(out)[1:2] <- paste(st_et_names[1:2],paste0("(",tz,")",collapse=""))
	if(addCoverage){
		addCov <- coverage(x)
		colnames(addCov) <- paste0("coverage(",colnames(addCov),")")
		out <- cbind(out,addCov)
	}
	write.table(out,file=file,sep=sep,row.names=row.names,...)
}
