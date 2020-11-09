mw.median <-
function(data,window=21,column=1,colClasses=NULL,...){
	mw.apply(data[,column],"median",window,colClasses,...)
}
