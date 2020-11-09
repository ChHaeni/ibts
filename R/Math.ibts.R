Math.ibts <-
function(x, ...){
    mode.ok <- vapply(x, function(x) is.numeric(x) || is.complex(x), 
        NA)
    if (all(mode.ok)) {
    	CLASSES <- colClasses(x)
    	COVERAGE <- coverage(x)
        x[] <- lapply(X = x, FUN = .Generic, ...)
        attr(x,"colClasses") <- CLASSES
        attr(x,"coverage") <- COVERAGE
        return(x)
    }
    else {
        vnames <- names(x)
        if (is.null(vnames)) 
            vnames <- seq_along(x)
        stop("non-numeric variable in ibts object: ", paste(vnames[!mode.ok],collapse=", "))
    }
}
