parse_timerange <- function(x, tz = "") {
	seps <- getOption("time.separators")
	split_char <- seps[sapply(seps, grepl, x)]
	if (length(split_char)) {
		x <- unlist(strsplit(x, split_char, fixed = TRUE))
	}
	out <- parse_date_time3(x, tz = tz)
    if (is.na(out[2])) {
        out[2] <- parse_date_time3(paste(date(out[1]), x[2]), tz = tz)
    }
    out
}
