
# generic
as.ibts <- function(x, st = "st", et = "et", 
    colClasses = ifelse(sapply(x, is.numeric), "avg", "other"), tz = NULL, 
	format = NULL, granularity = NULL, coverage = NULL, closed = "st", ...) {
    UseMethod('as.ibts')
}

# default
as.ibts.default <- function(x,...){
	Name <- make.names(deparse(substitute(x)))
	df <- data.frame(V1=x,stringsAsFactors=FALSE)
	names(df) <- Name
	as.ibts.data.frame(df,...)
}

# NULL
as.ibts.NULL <- function(x,...)NULL

# matrix
as.ibts.matrix <- function(x,check.names=FALSE,...){
	as.ibts.data.frame(as.data.frame(x,stringsAsFactors=FALSE,check.names=check.names),...)
}

# list
as.ibts.list <- function(x,check.names=FALSE,...){
	if(is.null(names(x))){
		names(x) <- paste0("V",seq_along(x))
	} else {
		names(x)[names(x) %in% ""] <- paste0("V",which(names(x) %in% ""))
	}
	as.ibts.data.frame(as.data.frame(x,stringsAsFactors=FALSE,check.names=check.names),...)
}

# data.table
as.ibts.data.table <- function(x,check.names=FALSE,...){
	as.ibts.data.frame(as.data.frame(x,stringsAsFactors=FALSE,check.names=check.names),...)
}

# data.frame
as.ibts.data.frame <- function(x, st = "st", et = "et", colClasses = ifelse(sapply(x, is.numeric), "avg", "other"), tz = NULL, 
	format = NULL, granularity = NULL, coverage = NULL, closed = "st", ...) {
	
	remove_cols <- character(2)
	if(is.null(format)){
		format <- getOption("time.orders")
	}

	# check if we can get tzone information:
	if(is.null(tz)){
		if(is.character(st)){
			tz <- try(lubridate::tz(x[,st]),silent=TRUE)
		} else {
			tz <- try(lubridate::tz(st),silent=TRUE)
		}
		if(inherits(tz,"try-error")){
			if(is.character(et)){
				tz <- try(lubridate::tz(x[,et]),silent=TRUE)
			} else {
				tz <- try(lubridate::tz(et),silent=TRUE)
			}			
		}
		if(inherits(tz,"try-error"))tz <- ""
	}
	
	# get st_index:
	if (is.null(granularity) || !missing(st)) {
		if(is.character(st)){
			if(length(st)==1){
				st_index <- try(x[,st],silent = TRUE)
				if(inherits(st_index,"try-error")){
					st_index <- parse_date_time3(st,orders=format,tz=tz,quiet=TRUE)
					if(is.na(st_index)){
						if(missing(st)){
							stop("(column/list) name 'st' does not exist")
						} else {
							stop("cannot convert '",st,"' to a (column/list) name or time object")
						}
					}
				} else if(!is.POSIXt(st_index)){
					st_index <- parse_date_time3(st_index,orders=format,tz=tz,quiet=TRUE)
					if(anyNA(st_index)){
						stop("cannot convert column/list entry '",st,"' to a time object")
					}
					remove_cols[1] <- st 
				} else {
					st_index <- as.POSIXct(st_index)
					remove_cols[1] <- st 
				}
			} else {
				st_index <- parse_date_time3(st,orders=format,tz=tz,quiet=TRUE)
				if(anyNA(st_index)){
					stop("failed to parse argument 'st' to time object")
				}
			}
		} else if(is.POSIXt(st)){
			st_index <- as.POSIXct(st)
        } else if(is.numeric(st) && length(st) == 1 && st <= length(x)) {
            st_index <- x[, st]
            remove_cols[1] <- names(x)[st]
		} else if(!is.POSIXct(st)){
			stop("Please provide either a (column/list) name or a vector that can be converted to a time object")
		} else {
			st_index <- st
		}
	} else {
		st_index <- NULL
	}

	# get et_index:
	if (is.null(granularity) || !missing(et)) {
		if(is.character(et)){
			if(length(et)==1){
				et_index <- try(x[,et],silent = TRUE)
				if(inherits(et_index,"try-error")){
					et_index <- parse_date_time3(et,orders=format,tz=tz,quiet=TRUE)
					if(is.na(et_index)){
						if(missing(et)){
							stop("(column/list) name 'et' does not exist")
						} else {
							stop("cannot convert '",et,"' to a (column/list) name or time object")
						}
					}
				} else if(!is.POSIXt(et_index)){
					et_index <- parse_date_time3(et_index,orders=format,tz=tz,quiet=TRUE)
					if(anyNA(et_index)){
						stop("cannot convert column/list entry '",et,"' to a time object")
					}
					remove_cols[2] <- et 
				} else {
					et_index <- as.POSIXct(et_index)
					remove_cols[2] <- et 
				}
			} else {
				et_index <- parse_date_time3(et,orders=format,tz=tz,quiet=TRUE)
				if(anyNA(et_index)){
					stop("failed to parse argument 'et' to time object")
				}
			}
		} else if(is.POSIXt(et)){
			et_index <- as.POSIXct(et)
        } else if(is.numeric(et) && length(et) == 1 && et <= length(x)) {
            et_index <- x[, et]
            remove_cols[2] <- names(x)[et]
		} else if(!is.POSIXct(et)){
			stop("Please provide either a (column/list) name or a vector that can be converted to a time object")
		} else {
			et_index <- et
		}
	} else {
		et_index <- NULL
		if(is.null(st_index))stop("Please provide either a column name or a vector that can be converted to a time object")
	}

	# if !is.null(granularity):
	if(is.null(st_index))st_index <- et_index - parse_time_diff(granularity)
	if(is.null(et_index))et_index <- st_index + parse_time_diff(granularity)
	
	# adjust tz:
	st_index <- with_tz(st_index,tz)
	et_index <- with_tz(et_index,tz)

    # add coverage
	if(is.null(coverage)){
		coverage <- matrix(1L,ncol=length(x),nrow=length(st_index))
		colnames(coverage) <- names(x)
		coverage[is.na(x)] <- 0L		
	}

	# remove NA values in st/et:
	if (anyNA(st_index) || anyNA(et_index)) {
		isna <- unique(which(c(is.na(st_index), is.na(et_index))))
		st_index <- st_index[-isna]
		et_index <- et_index[-isna]
		coverage <- coverage[-isna, , drop = FALSE]
		x <- x[-isna, , drop = FALSE]
		warning("NA values in time indices: Removed ", length(isna), " entries!")
	}
	
    # sort by st_index
    ind <- order(st_index)
    st_index <- st_index[ind]
    et_index <- et_index[ind]
    x <- x[ind, , drop = FALSE]

	# strictly increasing?
	if (any(check1 <- as.numeric(diff(st_index), units = "secs") <= 0) ||
		any(check2 <- as.numeric(et_index - st_index, units = "secs") <= 0) ||
		any(check3 <- as.numeric(et_index[-length(et_index)] - st_index[-1], units = "secs") > 0)) {
        if (any(check1)) {
            stop("interval times failure! -> diff(st) <= 0 (st index: ",
                paste(st_index[check1], collapse = ', '), ')')
        } else if (any(check2)) {
            stop("interval times failure! -> et - st <= 0 (st index: ",
                paste(st_index[check2], collapse = ', '), ')')
        } else {
            stop("interval times failure! -> difference st to previous et < 0")
            stop("interval times failure! -> difference st to previous et < 0 (index: ",
                paste(st_index[check3], collapse = ', '), ')')
        }
	}

	# remove st/et columns:
	x <- x[, !(names(x) %in% remove_cols), drop = FALSE]
    coverage <- coverage[, !(colnames(coverage) %in% remove_cols), drop = FALSE]

	# set attributes:
	attr(x, "st") <- st_index
	attr(x, "et") <- et_index
	attr(x, "tzone") <- tz
	attr(x, "colClasses") <- colClasses
	attr(x, "coverage") <- coverage
	attr(x, "closed") <- closed
	attr(x, "class") <- c("ibts", class(x))
	
	# check names & return:
	check_names(x)
}

# xts
as.ibts.xts <- function(x,xts_index=c("st","et"),closed=xts_index[1],interval_lentghs=min(diff(index(x))),colClasses=NULL,coverage=NULL,...){
    if(missing(xts_index)&&missing(interval_lentghs)&&!is.null(attr(x,"st"))&&!is.null(attr(x,"et"))){
    	st_out <- attr(x,"st")
    	et_out <- attr(x,"et")
    } else {
    	if(xts_index[1]=="st"){
	    	st_out <- index(x)
	    	et_out <- st_out + parse_time_diff(interval_lentghs)  			
    	} else if(xts_index[1]=="et"){
	    	et_out <- index(x)
	    	st_out <- et_out - parse_time_diff(interval_lentghs)
    	} else {
    		stop("xts_index must be either 'st' or 'et'")
    	}
    }
    if(is.null(colClasses)){
    	colClasses <- attr(x,"colClasses")
    }
    x <- as.data.frame(x,check.names=FALSE,stringsAsFactors=FALSE)
    if(!is.null(colClasses)){
    	cn <- names(x) 
    	x <- data.frame(lapply(seq_along(colClasses),function(i,x,cC){
    		if(cC[i]=="other"){
    			return(as.character(x[,i])) 
    		} else {
    			return(as.numeric(x[,i]))
    		}
    	},x=x,cC=colClasses),check.names=FALSE,stringsAsFactors=FALSE)
    	names(x) <- cn
    }
    if(is.null(coverage))coverage <- attr(x,"coverage")
    if(is.null(colClasses)){
	    xx <- as.ibts.data.frame(x,st_out,et_out,coverage=coverage,closed=closed,...)
    } else {
	    xx <- as.ibts.data.frame(x,st_out,et_out,colClasses=colClasses,coverage=coverage,closed=closed,...)
    }
    xx
}
