mw.rlm <-
function(formula,data,window=21,stats=coef,...){
	mw.regression(formula,data,window=window,stats=stats,FUN="rlm",...)
}
