tzone.default <-
function (x,...) 
{
    if (is.null(attr(x, "tzone")) && !is.POSIXt(x)) 
        return("UTC")
    tzs <- attr(as.POSIXlt(x), "tzone")
    tzs[1]
}
