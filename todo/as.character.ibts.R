as.character.ibts <-
function(x,...){
	if(ncol(x)==1){
		x <- x[,1,keepAtts=FALSE]
	}
	NextMethod("as.character")
}
