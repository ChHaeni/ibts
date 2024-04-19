papply <- function(data,FUN,times,periodicity="1days",base.date=NULL,colClasses=NULL,tz=NULL,...){
    if(is.null(tz)){
        tz <- tzone(data)
    }
	dots <- match.call(expand.dots = FALSE)[["..."]]
	pind <- pindex(data,times,periodicity)
    data_calc <- as.data.frame(data)
    if(is.character(FUN)){
    	FUN <- get(FUN)
    }
    if(!("wts" %in% names(formals(FUN)))){
		formals(FUN) <- c(formals(FUN),alist(wts=))
	}
    cvrg <- coverage(data)
    out <- lapply(pind, function(x) {
        res <- try(do.call(FUN, c(list(data_calc[x[,1], ],wts=x[,2]*cvrg[x[,1], ]), dots)))
        if (inherits(res, "try-error")) {
            res <- NA
        }
        res
    })
    cvrg_out <- lapply(pind,function(x){
        sum(x[,2]*cvrg[x[,1], ])
    })
    isna <- sapply(out, function(x) identical(is.na(x), TRUE))
    if (all(isna)) 
        stop("All values returned NA")
    dummy <- out[!isna][[1]]
    if (is.null(names(dummy))) {
        names(dummy) <- paste0("papply.", seq.int(length(dummy)))
    }
    dfout <- data.frame(matrix(NA, nrow = length(out), ncol = length(dummy)))
    names(dfout) <- names(dummy)
    cvrg_out <- do.call(rbind,cvrg_out)
    if (length(dummy) == 1) {
        dfout[!isna, ] <- unlist(out[!isna])
    }
    else {
        dfout[!isna, ] <- do.call(rbind, out[!isna])
        cvrg_out <- cvrg_out[,rep(1,length(dummy))]
    }
    if (is.null(colClasses)) {
        clCl <- rep("num", length(dummy))
    } else {
        clCl <- rep(colClasses, length(dummy))[seq_along(dummy)]
    }
    p.st <- attr(pind,"p.st")
    p.et <- attr(pind,"p.et")    
    if(!is.null(base.date)){
        base.date <- trunc(parse_date_time3(base.date,tz=tz),"day")
    	p.st <- p.st - trunc(p.st[1],"day") + base.date
    	p.et <- p.et - trunc(p.et[1],"day") + base.date
    }
    rownames(dfout) <- names(pind)
    as.ibts.data.frame(dfout, st = p.st, et = p.et, tz = tz, 
        colClasses = clCl, coverage = cvrg_out)
}
