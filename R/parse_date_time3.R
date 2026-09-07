parse_date_time3 <- function(x, orders = getOption("time.orders"), tz = tzone(x), quiet = TRUE) {
    # clean whitespace in x argument
    x <- trimws(x)
    # try all orders
    out <- lapply(x, fast_strptime, format = orders, tz = tz, lt = FALSE)
    # unlist & return
    as.POSIXct(unlist(out), tz = tz)
}
