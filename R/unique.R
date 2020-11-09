
unique.POSIXct <- function(x, incomparables = FALSE, ...){
    x[1] + unique(as.numeric(x - x[1],units="secs"))
}
