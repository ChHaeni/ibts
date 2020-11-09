`coverage<-` <-
function(x,value){
	if(any(value>1))stop("coverage > 1 not allowed!")
	cv <- coverage(x)
	cv[] <- value
	cv[is.na(x)] <- 0
	attr(x,"coverage") <- cv
	x
}
