"%+%" <- function(x, y) {
    # return either POSIXct or character
    clx <- lapply(x, class)
    lx <- length(x)
    ly <- length(y)
    xp <- lapply(x, parse_timerange, tz = 'UTC')
    lsx <- lengths(xp)
    if (is.list(y)) {
        yp <- lapply(y, parse_time_diff)
    } else {
        yp <- parse_time_diff(y)
    }
    # either times or timeranges
    if (lx == sum(lsx)) {
        # times only -> apply as is
        if (clx[[1]][1] == 'character') {
            out <- mapply('+', xp, yp, SIMPLIFY = FALSE)
            # convert back
            out <- sapply(out, format)
        } else {
            out <- x + yp
        }
    } else if ((2 * lx) == sum(lsx)) {
        # timeranges only
        out <- mapply('+', xp, yp, SIMPLIFY = FALSE)
        # convert back
        out <- sapply(out, deparse_timerange)
    } else {
        # mixed, is this even occuring?
        stop('Fix mixed times/timeranges in %+%\n')
    }
    # return
    out
}

"%-%" <- function(x, y) {
    # return either POSIXct or character
    clx <- lapply(x, class)
    lx <- length(x)
    ly <- length(y)
    xp <- lapply(x, parse_timerange, tz = 'UTC')
    lsx <- lengths(xp)
    if (is.list(y)) {
        yp <- lapply(y, parse_time_diff)
    } else {
        yp <- parse_time_diff(y)
    }
    # either times or timeranges
    if (lx == sum(lsx)) {
        # times only -> apply as is
        if (clx[[1]][1] == 'character') {
            out <- mapply('-', xp, yp, SIMPLIFY = FALSE)
            # convert back
            out <- sapply(out, format)
        } else {
            out <- x - yp
        }
    } else if ((2 * lx) == sum(lsx)) {
        # timeranges only
        out <- mapply('-', xp, yp, SIMPLIFY = FALSE)
        # convert back
        out <- sapply(out, deparse_timerange)
    } else {
        # mixed, is this even occuring?
        stop('Fix mixed times/timeranges in %-%\n')
    }
    # return
    out
}
