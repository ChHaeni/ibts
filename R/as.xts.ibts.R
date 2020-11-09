as.xts.ibts <-
function(x,order.by=NULL,...,keepAtts = TRUE){
	if(is.null(order.by)){
		order.by <- switch(closed(x),
			"st" = st(x)
			,"et" = et(x)
			)
	} else if(is.character(order.by)){
		order.by <- switch(order.by,
			"st" = st(x)
			,"et" = et(x)
			,stop("argument 'order.by' must be either 'st' or 'et'")
			)
	}
	if(keepAtts){
    	Atts <- attributes(x)[c("st","et","colClasses","coverage")]
    	xx <- xts(x, order.by = order.by,...)
        xtsAttributes(xx) <- Atts
    } else {
        xx <- xts(x, order.by = order.by,...)
    }
    xx	
}
