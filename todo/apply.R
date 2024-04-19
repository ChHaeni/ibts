
apply <- function(X, MARGIN, FUN, ...)UseMethod("apply")
apply.default <- base::apply
apply.ibts <- function(X, MARGIN, FUN,.colClasses = "avg", ...){
    if(is.character(FUN)){
        FUN <- get(FUN,mode="function")
    }   
    if(MARGIN==2L){
        out <- apply.default(X,MARGIN,FUN,...)
    } else if(MARGIN==1L){
        out <- apply.default(X,MARGIN,FUN,...)
        nms <- rownames(out)
        out <- matrix(t(out),nrow=nrow(X))
        if(is.null(nms)){
            nms <- paste("V",seq.int(ncol(out)),sep=".")
        }
        colnames(out) <- nms
        out <- cbind(X,out)[,-seq_along(X)]
        colClasses(out) <- .colClasses
    }
    out
}
