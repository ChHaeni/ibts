mw.apply <-
function(data,FUN,window=21,colClasses=NULL,...){
	# if(is.character(FUN)){
	# 	FUNname <- FUN
	# } else {
	# 	FUNname <- deparse(substitute(FUN))
	# 	if(any(grepl("function",FUNname))){
	# 		FUNname <- "FUN"
	# 	}
	# }
	if(!inherits(data,"ibts"))stop("object has to be of class 'ibts'")
	if(nrow(data)<1)stop("'ibts' object is empty")
	ArgList <- match.call(expand.dots=FALSE)[["..."]]
	stindex <- st(data)
	etindex <- et(data)
	d_t <- unique(as.numeric(etindex-stindex,units="secs"))
	if(all(d_t<1))d_t <- median(d_t)
	if(length(d_t)>1)stop("only equi-distant time series allowed!")
	if(is.character(window)|is.difftime(window)){
		window <- parse_time_diff(window)/d_t
	}
	window <- round(window)
	if(window%%2 == 0){
		warning("extended window to nearest uneven number (oder so)")
		window <- window + 1
	}
	winhalf <- (window - 1)/2
	inds <- mwIndices(winhalf,d_t,as.numeric(stindex))
	data_calc <- as.data.frame(data)
	out <- lapply(inds,function(x){
		res <- try(do.call(FUN,c(list(data_calc[x,]),ArgList)),silent=TRUE)
		if(inherits(res,"try-error")){
			res <- NA
		}
		res	
	})
	isna <- sapply(out,function(x)identical(is.na(x),TRUE))
	if(all(isna))stop("All values returned NA")
	dummy <- out[!isna][[1]]
	if(is.null(names(dummy))){
		names(dummy) <- paste0("apply.",seq.int(length(dummy)))
	}
	dfout <- data.frame(matrix(NA,nrow=length(out),ncol=length(dummy)))
	# names(dfout) <- paste(FUNname,names(dummy),sep="_")
	names(dfout) <- names(dummy)
	if(length(dummy)==1){
		dfout[!isna,] <- unlist(out[!isna])
	} else {
		dfout[!isna,] <- do.call(rbind,out[!isna])
	}
	if(is.null(colClasses)){
		clCl <- rep("num",length(dummy))
	} else {
		clCl <- rep(colClasses,length(dummy))[seq_along(dummy)]
	}
	as.ibts.data.frame(dfout,st=stindex,et=etindex,tzone=tzone(data),colClasses=clCl,closed=closed(data))		
}
