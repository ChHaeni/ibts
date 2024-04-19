`tzone<-.ibts` <-
function(x,value){
	attr(x,"st") <- with_tz(attr(x,"st"),value)
	attr(x,"et") <- with_tz(attr(x,"et"),value)
	attr(x,"tzone") <- value
	x
}
