uptime <- function(x){
	# if(ncol(x)==0){
	# 	cat("data frame of class 'ibts' with 0 columns and",nrow(x),"rows\n")
	# } else {
	coverage <- attr(x, "coverage")
	t1 <- as.numeric(sum(attr(x,"et") - attr(x,"st")),units="secs")
	# add average coverage
	cvrg <- colSums(coverage)/nrow(coverage)*100
	cvrg_str <- ifelse(cvrg < 100 & cvrg >= 99.95,sprintf("<%1.1f%%",cvrg),ifelse(cvrg > 0 & cvrg <= 0.05,sprintf(">%1.1f%%",cvrg),sprintf("%1.1f%%",cvrg)))
	# add uptime
	if(t1 < 120){
		add <- paste(round(t1,2),"seconds")
	} else if(floor(t1 / 60) < 120){
		add <- paste(round(t1/60,2),"minutes")
	} else if(floor(t1 / 3600) < 48){
		add <- paste(round(t1/3600,2),"hours")
	} else {
		add <- paste(round(t1/(3600*24),2),"days")
	}
	cat("from:   ",format(st(x)[1]),"\n")
	cat("to:     ",format(et(x)[nrow(x)]),"\n")
	cat("uptime: ",add,"\n")
	ls <- lengths(strsplit(names(x),"")) + 1
	lmax <- max(ls)
	tabs <- if(lmax < 11){
		"\t"
	} else if(lmax < 19){
		ifelse(ls < 11,"\t\t","\t")
	} else {
		ifelse(ls < 11,"\t\t\t",ifelse(ls < 19,"\t","\t\t"))
	}
	cat(paste0(" |- ",names(cvrg_str),": ",tabs,cvrg_str,"\n",collapse=""))
	t1 <- as.difftime(t1,units="secs")
	out <- list(total=t1,individual=lapply(cvrg/100,function(x)x*t1))
	names(out[[2]]) <- names(x)
	invisible(out)
}
