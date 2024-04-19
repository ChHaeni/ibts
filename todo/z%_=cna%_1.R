`%>=cNA%` <-
function(x,y){
	cv <- coverage(x)
	isna <- cv < y
	for(i in seq.int(NCOL(cv))){
		x[isna[,i],i] <- NA
	}
	cv[isna] <- 0
	coverage(x) <- cv
	x
}
