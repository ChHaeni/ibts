mt <-
function(x,tz=tzone(x)){
	st(x,tz) + (et(x,tz) - st(x,tz))/2
}
