as.data.frame.ibts <-
function(x, keep_times = FALSE, keepAtts = FALSE, row.names = NULL, optional = FALSE, ...){
    if (keep_times) {
        x_st <- st(x)
        x_et <- et(x)
        class(x) <- 'data.frame'
        x <- cbind.data.frame(st = x_st, et = x_et, x)
    }
	if(!keepAtts){
		attr(x,"st") <- NULL
		attr(x,"et") <- NULL
		attr(x,"tzone") <- NULL
		attr(x,"colClasses") <- NULL
		attr(x,"coverage") <- NULL
		attr(x,"closed") <- NULL
    }
    as.data.frame.data.frame(x, row.names = row.names, optional = optional, ...)
}

as.data.table.ibts <-
function(x, keep_times = TRUE, keepAtts = FALSE, keep.rownames = FALSE, ...){
    if (!requireNamespace('data.table')) {
        stop('data.table package not installed')
    }
    if (keep_times) {
        x_st <- st(x)
        x_et <- et(x)
        class(x) <- 'data.frame'
        x <- cbind.data.frame(st = x_st, et = x_et, x)
    }
	if(!keepAtts){
		attr(x,"st") <- NULL
		attr(x,"et") <- NULL
		attr(x,"tzone") <- NULL
		attr(x,"colClasses") <- NULL
		attr(x,"coverage") <- NULL
		attr(x,"closed") <- NULL
    }
    data.table:::as.data.table.data.frame(x, keep.rownames = keep.rownames, ...)
}
