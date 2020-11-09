check_names <-
function(x){
	nms <- names(x)
	is_dup <- which(duplicated(nms))
	if(length(is_dup)){
		unique_dup <- unique(nms[is_dup])
		for(i in unique_dup){
			nms[nms==i] <- paste0(nms[nms==i],".",seq.int(sum(nms==i)))
		}
	}
	names(x) <- nms
	x
}
