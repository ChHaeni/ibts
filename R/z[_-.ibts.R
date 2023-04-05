`[<-.ibts` <- function(x, i, j, value) {
	if (is.null(value)) {
		 if (missing(i)) {
		 	if (is.logical(j)) {
		 		j <- which(j)
		 	} else if (is.character(j)) {
		 		j <- which(names(x) %in% j)
		 	}
		 	return(x[, -j])
		 } else {
		 	stop("replacement has 0 length")
		 }
	}
	st_index <- attr(x, "st")
	et_index <- attr(x, "et")
	x_closed <- attr(x, "closed")
	x_tzone <- attr(x, "tzone")
	has.i <- !missing(i)
	has.j <- !missing(j)
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
			if (is.ibts(i)) {
				stop("Object merging on value assignment is not supported!")
			} else {
				neg <- FALSE
			}			
		}

		if (is.character(i)) {
			seps <-  paste0("(", paste(getOption("time.separators"), collapse = "|"), "){1}")
			split_char <- grepl(seps, i)
			i_list <- vector("list", length(i))
			if (any(split_char)) {
				i_list[split_char] <- lapply(i[split_char], 
                    function(indx, tz, st_index, et_index) {
                        ind0 <- unlist(strsplit(indx, seps))
                        # add new formats here
                        ind <- parse_date_time3(ind0, tz = tz, quiet = TRUE)
                        a <- which(et_index > ind[1])[1]
                        b <- rev(which(st_index < ind[2]))[1]
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
		} else {
			if (neg) {
				i <- -i
			}
		}
	}

	if (has.i && is.numeric(i) && !all(i[i > 0] %in% c(1:nrow(x))))
        stop("adding rows is not defined for \"ibts\" objects")

	classes_x <- attr(x, "colClasses")
	coverage_x <- attr(x, "coverage")

	xout <- "[<-.data.frame"(as.data.frame(x, keepAtts = FALSE), i, j, value)
	
	if (has.i) {
		CLASSES <- classes_x
		COVERAGE <- coverage_x
		COVERAGE[i, j] <- as.numeric(!is.na(xout[i, j]))
	} else {
		COVERAGE <- matrix(0, nrow = NROW(xout), ncol = NCOL(xout))
		if (has.j) {
			CLASSES <- colClasses(xout)
			seq_x <- 1:NCOL(x)
			seq_y <- 1:NCOL(xout)
			if (is.numeric(j)) {
				j <- seq_y %in% seq_y[j]
			} else if (is.character(j)) {
				j <- names(xout) %in% j
			}
			ind <- is.na(match(seq_x, seq_y[j]))
			COVERAGE[, !j] <- coverage_x[, ind, drop = FALSE]
			CLASSES[!j] <- classes_x[ind]
		} else {
			CLASSES <- classes_x
		}
		if (!all(is.na(value))) {
			COVERAGE[, j] <- coverage(value)
		}
	}

	# set attributes:
	attr(xout, "st") <- st_index
	attr(xout, "et") <- et_index
	attr(xout, "tzone") <- x_tzone
	attr(xout, "colClasses") <- CLASSES
	attr(xout, "coverage") <- COVERAGE
	attr(xout, "closed") <- x_closed
	attr(xout, "class") <- c("ibts", "data.frame")
	
	# check names & return:
	check_names(xout)

}

