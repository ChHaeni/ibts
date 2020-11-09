as.data.frame.ibts <-
function(x, row.names = NULL, optional = FALSE, keepAtts = FALSE, ...){
	if(keepAtts){
		NextMethod("as.data.frame")
	} else {
		attr(x,"st") <- NULL
		attr(x,"et") <- NULL
		attr(x,"tzone") <- NULL
		attr(x,"colClasses") <- NULL
		attr(x,"coverage") <- NULL
		attr(x,"closed") <- NULL
		attr(x,"class") <- "data.frame"
		x
	}
}
