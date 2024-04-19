`%<c%` <-
function(x,y){
	nc <- ncol(x)
	cv <- coverage(x) < y
	x[rowSums(cv)==nc,]
}
