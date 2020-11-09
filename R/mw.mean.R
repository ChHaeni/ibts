mw.mean <-
function(data,window=21,column=1,colClasses=NULL,...){
	mw.apply(data[,column],"mean",window,colClasses,...)
}
