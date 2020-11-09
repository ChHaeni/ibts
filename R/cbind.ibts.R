cbind.ibts <- function(...,COL.CLASSES=NULL){
	ArgList <- list(...)
	arg.names <- names(ArgList)
	if(is.null(arg.names))arg.names <- rep("",length(ArgList))
	isIBTS <- sapply(ArgList,is.ibts)
	w_isIBTS <- which(isIBTS)
	w_notIBTS <- which(!isIBTS)
	x <- ArgList[w_isIBTS][[1]]
	if(any(sapply(ArgList,NROW) %in% 0)){
		stop("empty argument has been supplied!")
	}
	cvR <- lapply(ArgList[w_isIBTS],coverage)
	st_out <- st(x)
	et_out <- et(x)
	closed_out <- closed(x)
	COVERAGE <- lapply(ArgList,coverage)
	CLASSES <- unlist(lapply(ArgList,colClasses))
	nr <- nrow(x)
	if(length(w_isIBTS)>1){
		for(i in w_isIBTS[-1])if(!identical(st(ArgList[[i]],tzone(x)),st_out)|!identical(et(ArgList[[i]],tzone(x)),et_out))stop("'st' and 'et' of ibts objects must match!")
	}
	if(length(w_notIBTS)){
		for(i in w_notIBTS){
			xx <- ArgList[[i]]
			if((is.vector(xx)|is.factor(xx))){
				if(length(xx)!=nr)xx <- rep(xx,nr)[seq.int(nr)]
				if(arg.names[i] %in% "") arg.names[i] <- paste0("X",i)
				ArgList[[i]] <- setNames(as.data.frame(xx,stringsAsFactors=FALSE,check.names=FALSE),arg.names[i])
				arg.names[i] <- ""
			} else {
				ArgList[[i]] <- as.data.frame(xx,stringsAsFactors=FALSE,check.names=FALSE)
			}
			COVERAGE[[i]] <- coverage(ArgList[[i]])
		}
	}
	if(!all(sapply(ArgList,nrow) %in% nr)){
		stop("number of rows not matching!")
	}
	if(any(nms <- arg.names!="")){
		ArgList[nms] <- mapply(function(x,y)`names<-`(x,sprintf(y,names(x))),ArgList[nms],arg.names[nms],SIMPLIFY=FALSE)
	}
	names(ArgList) <- NULL
	out <- do.call("cbind.data.frame",ArgList)
	out <- check_names(out)
	COVERAGE[w_isIBTS] <- cvR
	COVERAGE <- do.call("cbind",COVERAGE)
	colnames(COVERAGE) <- names(out)
	names(CLASSES) <- names(out)
	if(!is.null(COL.CLASSES)) CLASSES[names(COL.CLASSES)] <- COL.CLASSES
	as.ibts.data.frame(out,st_out,et_out,coverage=COVERAGE,colClasses=CLASSES,closed=closed_out)
}
