cbind <-
function (..., deparse.level = 1){
	if(any(sapply(list(...),inherits,"ibts"))){
		return(cbind.ibts(...))
	} else {
		# return(.Internal(cbind(deparse.level, ...)))
		return(base::cbind(...,deparse.level=deparse.level))
	}
}
