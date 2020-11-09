`colClasses<-` <-
function(x,value){
	if(length(x)!=length(value)){
		if(length(value)==1){
			attr(x,"colClasses") <- rep(value,length(x))
		} else {
			stop("colClasses must be of length ",length(x))
		}
	} else {
		attr(x,"colClasses") <- value
	}
	# test entries!
	names(attr(x,"colClasses")) <- names(x)
	x
}
