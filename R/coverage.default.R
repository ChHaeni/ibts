coverage.default <-
function(x){
	if(!is.null(attr(x,"coverage"))){
		out <- attr(x,"coverage")
	} else {
		out <- matrix(as.numeric(!is.na(x)),nrow=NROW(x),ncol=NCOL(x))
	}
	out
}
