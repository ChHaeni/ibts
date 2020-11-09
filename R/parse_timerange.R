parse_timerange <- function(x,tz=""){
	seps <- getOption("time.separators")
	split_char <- seps[sapply(seps,grepl,x)]
	if(length(split_char)){
		x <- unlist(strsplit(x,split_char,fixed=TRUE))
	}
	parse_date_time3(x,tz=tz)
}
