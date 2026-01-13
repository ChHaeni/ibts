
c.POSIXct <- function (..., recursive = FALSE, tz = tzone(..1)) {
    # convert to numeric vector
    out <- unlist(lapply(list(...), function(x) as.numeric(with_tz(x, tzone = tz))))
    # convert to POSIXct
    .POSIXct(out, tz = tz)
}

c.POSIXlt <- function (..., recursive = FALSE, tz = tzone(..1), lt = all_lt) {
    # check if all lt
    all_lt <- all(sapply(list(...), \(x) inherits(x, 'POSIXlt')))
    # convert to numeric vector
    out <- unlist(lapply(list(...), function(x) as.numeric(with_tz(x, tzone = tz))))
    # convert to POSIX
    if (lt) {
        as.POSIXlt(out, tz = tz)
    } else {
        .POSIXct(out, tz = tz)
    }
}
