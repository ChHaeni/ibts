mw.lm <-
function(formula,data,window=21,stats=coef,...){
	mw.regression(formula,data,window=window,stats=stats,FUN="lm",...)
}
