mw.sd <-
function(data,window=21,column=1,colClasses=NULL,...){
	mw.apply(data[,column],"sd",window,colClasses,...)
}
