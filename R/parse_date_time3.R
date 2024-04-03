parse_date_time3 <- function(x, orders = getOption("time.orders"), tz = tzone(x), quiet = TRUE) {
	out <- parse_date_time(x, orders = orders, tz = tz, quiet = quiet, exact = TRUE)
    if (anyNA(out)) {
        out[is.na(out)] <- parse_date_time(x[is.na(out)], orders = orders, tz = tz, quiet = quiet)
    }
	out
}
