
c.POSIXct <- function (..., recursive = FALSE){
    tz <- tzone(..1)
    .POSIXct(c(unlist(lapply(list(...),function(x,tz)unclass(with_tz(x,tz)),tz=tz))),tz=tz)
}
