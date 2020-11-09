`closed<-` <-
function(x,value){
	if(value %in% c("st","et")){
		attr(x,"closed") <- value
		return(x)
	} else {
		stop("attribute 'closed' must be either 'st' or 'et'")
	}
}
