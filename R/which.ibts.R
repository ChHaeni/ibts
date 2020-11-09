which.ibts <-
function(x,timeindex=TRUE,asCharacter=FALSE,timeindexFormat=c("st","et","mt","stet"),format="",usetz=TRUE,...){
	out <- which.default(as.logical(x),...)
	if(timeindex){
		if(missing(timeindexFormat)) timeindexFormat <- closed(x)
		out <- switch(timeindexFormat[1],
			"st"= st(x)[out],
			"et"= et(x)[out],
			"mt"= mt(x)[out],
			"stet" = data.frame(st=st(x)[out],et=et(x)[out]),
			stop("'timeindexFormat' not valid!")
			)
		if(asCharacter){
			if(timeindexFormat=="stet"){
				out <- apply(out,1,deparse_timerange,format=format,sep=" to ")
			} else {
				out <- format(out,format=format,usetz=usetz)	
			}
		} 
		if(timeindexFormat %in% c("st","et")){
			cat(switch(timeindexFormat,
				"st" = "start of intervals:\n"
				,"et" = "end of intervals:\n"
				))
		}
	}
	out
}
