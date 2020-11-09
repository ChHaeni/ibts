`$<-.ibts` <- function(x, name, value){
    nrows <- nrow(x)
    if (!is.null(value)) {
        N <- NROW(value)
        if (N > nrows) 
            stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                "replacement has %d rows, data has %d"), N, nrows), 
                domain = NA)
        if (N < nrows) 
            if (N > 0L && (nrows%%N == 0L) && length(dim(value)) <= 
                1L) 
                value <- rep(value, length.out = nrows)
            else stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                "replacement has %d rows, data has %d"), N, nrows), 
                domain = NA)
        if (is.atomic(value) && !is.null(names(value))) 
            names(value) <- NULL
    }
    x[,name] <- value
    return(x)	
}
