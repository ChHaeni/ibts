rbind <-
function (..., deparse.level = 1){
	if(all(sapply(list(...),inherits,"ibts"))){
		return(rbind.ibts(...))
	} else {
		# return(.Internal(rbind(deparse.level, ...)))
		return(base::rbind(...,deparse.level=deparse.level))
	}
}
