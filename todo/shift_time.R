shift_time <-
function(x,by){
	by <- parse_time_diff(by)
	attr(x,"st") <- attr(x,"st") + by
	attr(x,"et") <- attr(x,"et") + by
	x
}
