lines.ibts <-
function(x, ...) {
    if (nrow(x) > 0) {
        plot.ibts(x, add = TRUE, ...)
    }
}
