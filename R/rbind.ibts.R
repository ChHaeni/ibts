rbind.ibts <- function(...){
	ArgList <- list(...)
	tz <- tzone(ArgList[[1]])
	CLASSES <- colClasses(ArgList[[1]])
	st_index <- do.call(c,lapply(ArgList,function(x)with_tz(st(x),tz)))
	et_index <- do.call(c,lapply(ArgList,function(x)with_tz(et(x),tz)))
	# cat("colClasses check einbauen!\n")
	cclasses <- lapply(ArgList[-1],colClasses)
	if(any(!sapply(cclasses,function(x,y)identical(x,y),y=CLASSES))){
		if(any(!sapply(cclasses,function(x,y)identical(names(x),y),y=names(CLASSES)))){
			warning("column names do not match!")
		}
		if(any(!sapply(cclasses,function(x,y)identical(unname(x),y),y=unname(CLASSES)))){
			warning("colClasses are not identical!")
		}
	}
	out <- as.data.frame(do.call(rbind.data.frame,ArgList),keepAtts=FALSE)
	COVERAGE <- do.call(rbind,lapply(ArgList,coverage))
	## sort
	if(anyDuplicated(st_index) != 0 | anyDuplicated(et_index) != 0){
		stop("Cannot rbind objects because there are ",sum(duplicated(st_index), duplicated(et_index)), " duplicated start/end times.")
	}
	ind <- order(st_index)
	if(any(as.numeric(et_index[ind][-length(et_index)]-st_index[ind][-1],units="secs")>0)){
		stop("Cannot rbind objects because there are overlapping intervals")
	}
	as.ibts.data.frame(out[ind, ,drop = FALSE],st=st_index[ind],et=et_index[ind],tz=tz,colClasses=CLASSES,coverage=COVERAGE[ind,, drop = FALSE])
}
