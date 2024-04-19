Ops.ibts <-
function(e1,e2){
	if(nargs() == 1L){
		atts <- attributes(e1)
		e <- NextMethod(.Generic)
		if(.Generic %in% "!"){
			e <- as.data.frame(e)
		}
		attributes(e) <- atts
		return(e)
		# stop(gettextf("unary %s not defined for ibts objects",sQuote(.Generic)))
	} else if(.Generic %in% c("+", "-", "*", "/", "^", "%%", "%/%","==", "!=", "<", "<=", ">=", ">","&","|")){
		if(e1_noibts <- !is.ibts(e1)){
			a <- e1
			e1 <- e2
			e2 <- a
		}
		st_out <- st(e1)
		et_out <- et(e1)
		closed_out <- closed(e1)
		coverage_out <- coverage(e1)
		colClasses_out <- colClasses(e1)
		if(is.ibts(e2)){
			nc1 <- ncol(e1)
			nc2 <- ncol(e2) 
			if(nc1!=nc2){
				ncind <- seq.int(max(nc1,nc2))
				ind1 <- rep(seq.int(nc1),nc2)[ncind]
				ind2 <- rep(seq.int(nc2),each=nc1)[ncind]
				e1 <- e1[,ind1]
				e2 <- e2[,ind2]
				coverage_out <- coverage(e1)
				colClasses_out <- colClasses(e1)
			}
			colClasses_out2 <- colClasses(e2)
			coverage_out2 <- coverage(e2)
			if(!identical(st_out,st(e2,tzone(e1)))|!identical(et_out,et(e2,tzone(e1))))stop("'st' and 'et' of ibts objects must match!")
			if(!identical(as.vector(colClasses_out),as.vector(colClasses_out2)))warning("'colClasses' of ibts objects don't match!")
		} else {
			coverage_out2 <- matrix(coverage(e2),ncol=NCOL(e1),nrow=NROW(e1))
		}
		coverage_out[coverage_out2<coverage_out] <- coverage_out2[coverage_out2<coverage_out]
		if(e1_noibts && .Generic == "/"){
			a <- e1
			e1 <- e2
			e2 <- a
		    mode.ok <- vapply(e2, function(x) is.numeric(x) || is.complex(x), 
		        NA)
    		if (all(mode.ok)) {
    			e2 <- as.matrix(e2)
    		} else {
		        vnames <- names(e2)
		        if (is.null(vnames)) 
		            vnames <- seq_along(e2)
		        stop("non-numeric variable in ibts object: ", paste(vnames[!mode.ok],collapse=", "))    			
    		}	
			.Class <- "matrix"
		} else {
			if(e1_noibts){
				a <- e1
				e1 <- e2
				e2 <- a
			}
			# e1 <- as.data.frame(e1)
			.Class <- "data.frame"
		}
		e <- check_names(as.data.frame(NextMethod(.Generic)))
		return(as.ibts.data.frame(e,st_out,et_out,colClasses=colClasses_out,coverage=coverage_out,closed=closed_out))
	} else {
		return(NextMethod(.Generic))
	}
}
