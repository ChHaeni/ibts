"%+%" <- function(x,y){
	l1 <- length(x)
	x <- parse_timerange(x,tz="UTC")
	l2 <- length(x)
	y <- parse_time_diff(y)
	if(l1!=l2){
		mapply(deparse_timerange,x[1] + y,x[2] + y)
	} else {
		out <- x + y
		format(out, tz = "UTC", usetz = FALSE)
	}
}

