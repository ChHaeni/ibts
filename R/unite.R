
unite <- function(...,sorted=TRUE,decreasing = FALSE){
	psx <- sapply(list(...),inherits,"POSIXt")
	if(all(psx)){
		out <- ..1[1] + Reduce(union,lapply(list(...),function(x,st1)as.numeric(x - st1,units="secs"),st1=..1[1]))
	} else {
		out <- Reduce(union,list(...))
	}
	if(sorted){
		sort(out,decreasing=decreasing)
	} else {
		out
	}
}

common <- function(...,sorted=TRUE,decreasing = FALSE){
	psx <- sapply(list(...),inherits,"POSIXt")
	if(all(psx)){
		out <- ..1[1] + Reduce(intersect,lapply(list(...),function(x,st1)as.numeric(x - st1,units="secs"),st1=..1[1]))
	} else {
		out <- Reduce(intersect,list(...))
	}
	if(sorted){
		sort(out,decreasing=decreasing)
	} else {
		out
	}
}
