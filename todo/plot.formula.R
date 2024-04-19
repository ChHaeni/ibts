plot.formula <-
function(formula,data=parent.frame(),...){
	if(is.ibts(data)){
		plot.ibts(formula,data,...)
	} else if(missing(data)){
		dta <- lapply(all.vars(formula),get,envir=parent.frame())
		if(length(dta)<=2 & all(sapply(dta,is.ibts))){
			frml <- as.character(formula)
			eval(parse(text=paste0("plot.ibts(",frml[3],",",frml[2],",...)")))
			# plot.ibts(formula,data,...)
		} else {
			do.call(graphics:::plot.formula,c(list(formula=formula,data=data),list(...)))
		}
	} else {
		do.call(graphics:::plot.formula,c(list(formula=formula,data=data),list(...)))
	}
}
