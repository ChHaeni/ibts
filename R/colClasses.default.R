colClasses.default <-
function(x,check.names=FALSE,stringsAsFactors=FALSE,...){
	nc <- NCOL(x)
	if(!is.null(attr(x,"colClasses"))){
		out <- attr(x,"colClasses")
	} else if(is.data.frame(x)){
		out <- c("other","avg")[as.numeric(sapply(x,is.numeric)) + 1]
	} else if(is.vector(x)) {
		out <- c("other","avg")[as.numeric(is.numeric(x)) + 1]
	} else {
		x <- as.data.frame(x,check.names=FALSE,stringsAsFactors=FALSE)
		out <- c("other","avg")[as.numeric(sapply(x,is.numeric)) + 1]		
	}
	out
}
