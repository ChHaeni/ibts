`names<-.ibts` <-
function(x,value){
	colnames(attr(x,"coverage")) <- value
	names(attr(x,"colClasses")) <- value
	attr(x,"names") <- value
	x
}
