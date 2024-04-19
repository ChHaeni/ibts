row.names.ibts <-
function(x, TimeString=FALSE, as.printed=FALSE,usetz=TRUE, ...){
	if(TimeString){
		st_x <- attr(x,"st")
		et_x <- attr(x,"et")
		a <- as.numeric(st_x - trunc(st_x[1],"day"),"secs")
		b <- as.numeric(et_x - trunc(et_x[1],"day"),"secs")
		if(any(a%%60>0.5)|any(b%%60>0.5)){
			fmt1 <- "%Y-%m-%d %H:%M:%S"
			if(as.printed){
				fmt2 <- "%H:%M:%S"
				fmt2 <- paste0("- ",if(any(as.numeric(et_x-st_x,units="days")>=1)) "%Y-%m-%d " else "",fmt2)
			} else {
				fmt2 <- paste0("- ",fmt1)
			}
		} else if(any(b%%(24*3600)>0.5)|any(b%%(24*3600)>0.5)){
			fmt1 <- "%Y-%m-%d %H:%M"
			if(as.printed){
				fmt2 <- "%H:%M"
				fmt2 <- paste0("- ",if(any(as.numeric(et_x-st_x,units="days")>=1)) "%Y-%m-%d " else "",fmt2)
			} else {
				fmt2 <- paste0("- ",fmt1)
			}
		} else {
			fmt1 <- "%Y-%m-%d"
			if(as.printed){
				fmt2 <- "to %Y-%m-%d"
			} else{
				fmt2 <- paste0("- ",fmt1)
			}
		}
		sprintf("%s %s",format(st_x,fmt1,usetz=FALSE),format(et_x,fmt2,usetz=usetz))
	} else {
		NextMethod("row.names")
	} 
}

row.names <- function(x,...)UseMethod("row.names")
