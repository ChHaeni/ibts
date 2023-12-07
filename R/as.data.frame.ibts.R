as.data.frame.ibts <-
function(x, row.names = NULL, optional = FALSE, keepAtts = FALSE, time_columns = TRUE, ...){
    if (time_columns) {
        x_st <- st(x)
        x_et <- et(x)
        class(x) <- 'data.frame'
        x <- cbind.data.frame(x, st = x_st, et = x_et)
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

as.data.table <- function (x, keep.rownames = FALSE, ...) {
    if (!requireNamespace('data.table')) {
        stop('data.table package not installed')
    }
    if (is.ibts(x)) {
        as.data.table.ibts(x, keep.rownames = keep.rownames, ...)
    } else {
        data.table::as.data.table(x, keep.rownames = keep.rownames, ...)
    }
}
as.data.table.ibts <-
function(x, keep.rownames = FALSE, keepAtts = FALSE, time_columns = TRUE, ...){
    if (!requireNamespace('data.table')) {
        stop('data.table package not installed')
    }
    if (time_columns) {
        x_st <- st(x)
        x_et <- et(x)
        class(x) <- 'data.frame'
        x <- cbind.data.frame(x, st = x_st, et = x_et)
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
