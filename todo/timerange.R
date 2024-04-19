timerange <-
function(x){
	with_tz(c(st(x)[1],et(x)[nrow(x)]),tzone(x))
}
