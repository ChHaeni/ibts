datagaps <- function(x,check.all=FALSE,tol=1E-2,return.indices=FALSE,invert=FALSE){
	if(check.all){
		out <- vector("list",1)
		names(out) <- "all"
		check <- list(seq_along(x))
	} else {
		nc <- ncol(x)
		out <- vector("list",nc)
		check <- names(out) <- names(x)	
	}
	ST_x <- st(x)
	ET_x <- et(x)
	tz <- tzone(x)
	s1 <- start(x)
	e2 <- end(x)
	for(i in seq_along(check)){
		indNA <- rowSums(is.na(x[,check[[i]]])) == length(check[[i]])
		et_x <- with_tz(c(ST_x[!indNA],e2),tz)
		st_x <- with_tz(c(s1,ET_x[!indNA]),tz)
		diffs <- as.numeric(st_x - et_x,units="secs")
		ind <- which(diffs< -tol)
		if(return.indices){
			if(invert){
				if(length(ind)){
					out[[i]] <- lapply(seq_along(ind[-1]),function(y){which(ST_x == et_x[ind[y]]):which(ET_x == st_x[ind[y + 1]])})
				} else {
					out[[i]] <- c(1,length(ST_x))
				}
			} else {
				if(length(ind)){
					out[[i]] <- lapply(ind,function(y){which(ST_x == st_x[y]):which(ET_x == et_x[y])})
				} else {
					out[[i]] <- numeric(0)
				}				
			}
		} else {
			if(invert){
				if(length(ind)){
					ind_list <- lapply(seq_along(ind[-1]),function(y)data.frame(st=et_x[ind[y]],et=st_x[ind[y + 1]],duration=as.numeric(st_x[ind[y + 1]] - et_x[ind[y]],units="secs")))
					out[[i]] <- do.call(rbind,ind_list)
				} else {
					out[[i]] <- data.frame(st=s1,et=e2,duration=as.numeric(e2-s1,units="secs"))
				}
			} else {
				if(length(ind)){
					ind_list <- lapply(ind,function(y)data.frame(st=st_x[y],et=et_x[y],duration=as.numeric(et_x[y]-st_x[y],units="secs")))
					out[[i]] <- do.call(rbind,ind_list)
				} else {
					out[[i]] <- "no data gaps"
				}
			}
		}
	}
	out
}
