`[.ibts` <- function(x, i, j, drop = FALSE, keepAtts = TRUE) {
	st_index <- attr(x, "st")
	et_index <- attr(x, "et")
	x_closed <- attr(x, "closed")
	has.i <- !missing(i)
	if (has.i) {
		cl_i <- substitute(i)
		if (!is.name(cl_i)) {
			if (deparse(cl_i[[1]]) %in% "-") {
				if (is.character(cl_i[[2]]) || 
                    (is.name(cl_i[[2]]) && is.character(eval(cl_i[[2]])))) {
					cl_i <- cl_i[[-1]]
					neg <- TRUE
					i <- eval(cl_i)
				} else {
					neg <- FALSE
				}
			} else {
				neg <- FALSE
			}
		} else {
			neg <- FALSE
		}
		if (is.character(i)) {
			seps <-  paste0("(", paste(getOption("time.separators"), collapse = "|"), "){1}")
			split_char <- grepl(seps, i)
			i_list <- vector("list", length(i))
			if (any(split_char)) {
				i_list[split_char] <- lapply(i[split_char], 
                    function(indx, tz, st_index, et_index) {
                        ind0 <- unlist(strsplit(indx, seps))
                        # check "new" formats here (01.01.2023 16:00 to 17:00; 16:00 to 17:00)
                        ind <- parse_date_time3(ind0, tz = tz, quiet = TRUE)
                        if (length(ind) == 2 && is.na(ind[2])) {
                            ind[2] <- parse_date_time3(paste(date(ind[1]), ind0[2]), tz = tz, 
                                quiet = TRUE)
                        }
                        a <- which(et_index > ind[1]) [1]
                        b <- rev(which(st_index < ind[2])) [1]
                        if (is.na(a) && ind0[1] == "") {
                            a <- 1
                        }
                        if (is.na(b) && length(ind) == 1) {
                            b <- length(st_index)
                        }
                        if (any(is.na(a), is.na(b))) {
                            return(0L)
                        } else if (a > b) {
                            return(0L)
                        } else {
                            return(a:b)
                        }
                    },
                    tz = tzone(x), st_index = st_index, et_index = et_index
                )
			}
			if (any(!split_char)) {
				i <- parse_date_time3(i[!split_char], tz = tzone(x), quiet = TRUE)
				i_list[!split_char] <- getIntervals(i, st_index, et_index, x_closed)
			}
			i <- unlist(i_list)
			if (neg) {
				if (identical(unique(i), 0L)) {
					i <- 1:nrow(x)
				} else {
					i <- -i
				}
			}
		} else if (is.POSIXt(i)) {
			i <- getIntervals(i, st_index, et_index, x_closed)
			if (neg) {
				if (identical(unique(i), 0L)) {
					i <- 1:nrow(x)
				} else {
					i <- -i
				}
			}
		} else if (is.ibts(i)) {
			if (length(cl_i) == 2 && deparse(cl_i[[1]]) %in% "-") {
				stop("unary \"-\" not defined when merging \"ibts\" objects!")
			}
			suffixes <- paste0(".", c(deparse(substitute(x)), deparse(substitute(i))))
			if (missing(j)) {
				xout <- merge(x, i, suffixes = suffixes, to = "y")
			} else {
				xout <- merge(x[, j], i, suffixes = suffixes, to = "y")
			}
			return(xout)		
		} else {
			if (neg) {
				i <- -i
			}
		}
	}

	if (missing(j)) {
		j <- 1:length(x)
	}
	if (length(j) > 1) keepAtts <- TRUE
	if (!keepAtts) drop <- TRUE
	
	# remove NA values in st/et:
	if (has.i && anyNA(i)) {
		isna <- is.na(i)
		if (is.logical(i)) {
			i[isna] <- FALSE
		} else {
			i <- i[!isna]
		}
		warning("NA values in subsetting: Removed ", sum(isna), " entries!")
	}

	if (has.i && !is.logical(i) && !all(i[i > 0] %in% (1:nrow(x)))) 
        stop("row index out of bounds")

	if (has.i) {
		st_index <- st_index[i]
		et_index <- et_index[i]
	}
	xout <- "[.data.frame"(x, i, j, drop)

	if (keepAtts) {
		attr(xout, "st") <- st_index
		attr(xout, "et") <- et_index
		attr(xout, "colClasses") <- attr(x, "colClasses")[j]
		attr(xout, "coverage") <- attr(x, "coverage")[i, j, drop = FALSE]
		attr(xout, "tzone") <- attr(x, "tzone")
		attr(xout, "closed") <- x_closed
	}
	xout
}

