"%+%" <- function(x, y) {
    lx <- length(x)
    ly <- length(y)
    xp <- lapply(x, parse_timerange, tz = 'UTC')
    yp <- parse_time_diff(y)
    if (lx == length(unlist(xp))) {
        fmt_fun <- function(a) format(a, tz = 'UTC', usetz = FALSE)
    } else {
        fmt_fun <- deparse_timerange
    }
    if (ly == 1) {
        unlist(lapply(xp, \(z) fmt_fun(z + yp)))
    } else if (ly == 2) {
        unlist(lapply(xp, \(z) fmt_fun(z + yp)))
    } else {
        stop('length of second object must be 1 or 2')
    }
}

"%-%" <- function(x, y) {
    lx <- length(x)
    ly <- length(y)
    xp <- lapply(x, parse_timerange, tz = 'UTC')
    yp <- parse_time_diff(y)
    if (lx == length(unlist(xp))) {
        fmt_fun <- function(a) format(a, tz = 'UTC', usetz = FALSE)
    } else {
        fmt_fun <- deparse_timerange
    }
    if (ly == 1) {
        unlist(lapply(xp, \(z) fmt_fun(z - yp)))
    } else if (ly == 2) {
        unlist(lapply(xp, \(z) fmt_fun(z - yp)))
    } else {
        stop('length of second object must be 1 or 2')
    }
}
