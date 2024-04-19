merge.ibts <-
function(x,y,to="x",na.rm=TRUE,suffixes=c(".x",".y"),closed=NULL,...){
	st1 <- attr(x,"st")
	et1 <- attr(x,"et")
	cov1 <- attr(x,"coverage")
	colC1 <- attr(x,"colClasses")
	cn1 <- names(x)
	st2 <- attr(y,"st")
	et2 <- attr(y,"et")
	cov2 <- attr(y,"coverage")
	colC2 <- attr(y,"colClasses")
	cn2 <- names(y)
	suffixes <- if(is.null(suffixes)) c(".x",".y") else suffixes[1:2]
	if(suffixes[1]==suffixes[2])suffixes <- paste0(suffixes,c(".x",".y"))
	if(to=="x"){
		x_merge <- cbind.data.frame(x,"__rn"=seq.int(nrow(x)))
		if(length(st1)==length(st2) && all(st1 - st2 == 0) && all(et1 - et2 == 0)){
			y_merge <- cbind.data.frame(y,"__rn"=seq.int(nrow(x)))
		} else {
			y_merge <- cbind.data.frame(y2 <- pool(y,st.to=st1,et.to=et1,na.rm=na.rm,tz=tzone(x)),"__rn"=seq.int(nrow(x)))
			cov2 <- attr(y2,"coverage")
		}
		st_out <- st1
		et_out <- et1
		if(is.null(closed)) closed <- attr(x,"closed") 
	} else if(to=="y"){
		y_merge <- cbind.data.frame(y,"__rn"=seq.int(nrow(y)))
		if(length(st1)==length(st2) && all(st1 - st2 == 0) && all(et1 - et2 == 0)){
			x_merge <- cbind.data.frame(x,"__rn"=seq.int(nrow(y)))
		} else {
			x_merge <- cbind.data.frame(x2 <- pool(x,st.to=st2,et.to=et2,na.rm=na.rm,tz=tzone(y)),"__rn"=seq.int(nrow(y)))
			cov1 <- attr(x2,"coverage")
		}
		st_out <- st2
		et_out <- et2
		if(is.null(closed)) closed <- attr(y,"closed") 
	} else {
		stop("argument 'to' must be either \"x\" or \"y\"")
	}
	out <- merge.data.frame(x_merge,y_merge,by="__rn",suffixes=suffixes,all=TRUE)
	out <- as.ibts.data.frame(out[,-1,drop=FALSE],st_out,et_out,closed=closed)
	attr(out,"coverage")[] <- cbind(cov1,cov2)
	attr(out,"colClasses")[] <- c(colC1,colC2)
	return(check_names(out))
}
