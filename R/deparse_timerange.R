deparse_timerange <- 
function(x, y = NULL, sep = NULL, usetz = FALSE, format = "", tz = tzone(x)) {
	if (missing(sep) & !is.null(y)) {
		if (length(y) == 1 && is.character(y) && y %in% getOption("time.separators")) {
			sep <- y
			y <- x[2]
			x <- x[1]
		}
	} else if (is.null(y)) {
		y <- x[2]
		x <- x[1]		
	}
	if (is.null(sep)) {
		sep <- getOption("time.separators")[1]
	}
	paste(format(x, format = format, tz = tz, usetz = FALSE), format(y, format = format, tz = tz, usetz = usetz), sep = sep)
}
