# generic
pool <- function(dat, granularity = NULL, st.to = NULL, et.to = NULL, 
    closed = attr(dat, "closed"), format = NULL , tz = NULL, na.rm = TRUE, 
    FUN = NULL, ...) {
    UseMethod('pool')
}

# ibts
pool.ibts <- function(dat, granularity = NULL, st.to = NULL, et.to = NULL, 
    closed = attr(dat, "closed"), format = NULL , tz = NULL, na.rm = TRUE, FUN = NULL) {
	if(is.null(dat)){
		return(NULL)
	}

	if(is.null(tz)){
		tz <- tzone(dat)
	} else {
		tzone(dat) <- tz
		tz <- tzone(dat)
	}
	cn <- names(dat)
	st_in <- attr(dat,"st")
	et_in <- attr(dat,"et")
	colClasses <- attr(dat,"colClasses")
	coverage <- attr(dat,"coverage")
	if(!is.ibts(granularity)){
		if(is.null(format)){
			format <- getOption("time.orders")
		}
		if(!is.null(granularity)&!is.numeric(granularity)){
			granularity <- parse_time_diff(granularity)
		}
		if(!is.null(st.to)&&!is.null(et.to)){
			if(is.character(st.to)){
				st.to <- parse_date_time3(st.to,orders=format,tz=tz)
			} else {
				st.to <- with_tz(st.to,tz)
			}
			if(is.character(et.to)){
				et.to <- parse_date_time3(et.to,orders=format,tz=tz)
			} else {
				et.to <- with_tz(et.to,tz)
			}

			if(length(st.to)==1&&length(et.to)==1&&!is.null(granularity)){
				st.to <- seq(st.to,et.to-granularity,granularity)
				et.to <- st.to + granularity
			}
		} else if(!is.null(st.to)&&is.null(et.to)){
			if(is.character(st.to)){
				st.to <- parse_date_time3(st.to,orders=format,tz=tz)
			} else {
				st.to <- with_tz(st.to,tz)
			}
			if(length(st.to)==1) st.to <- st.to + unique(floor(as.numeric(st_in - st.to,units="secs")/granularity)*granularity)
			et.to <- st.to +  granularity
		} else if(is.null(st.to)&&!is.null(et.to)){
			if(is.character(et.to)){
				et.to <- parse_date_time3(et.to,orders=format,tz=tz)
			} else {
				et.to <- with_tz(et.to,tz)
			}
			if(length(et.to)==1) et.to <- et.to + unique(floor(as.numeric(et_in - et.to,units="secs")/granularity)*granularity)
			st.to <- et.to -  granularity
		} else {
			if(is.null(granularity))stop("please supply information on pooling intervals!")
			st.to <- st_in[1] + unique(floor(as.numeric(st_in - st_in[1],units="secs")/granularity)*granularity)
			et.to <- st.to + granularity
		}
	} else {
		st.to <- with_tz(attr(granularity,"st"),tz)
		et.to <- with_tz(attr(granularity,"et"),tz)
	}

	if(any(as.numeric(diff(st.to),units="secs")<=0)){
		if(any(duplicated(st.to),duplicated(et.to))){
				stop("interval times failure: duplicated time values in argument 'st.to'")
		}
		stop("interval times failure: 'st.to' must be strictly increasing!")
	}
	if(any(as.numeric(et.to-st.to,units="secs")<=0)){
		stop("interval times failure: negative intervals not allowed!")
	}
	if(any(as.numeric(et.to[-length(et.to)]-st.to[-1],units="secs")>0)){
		if(any(as.numeric(diff(et.to),units="secs")<=0)){
			if(any(duplicated(et.to))){
					stop("interval times failure: duplicated time values in argument 'et.to'")
			}
			stop("interval times failure: 'et.to' must be strictly increasing!")
		}		
		stop("interval times failure: overlapping intervals not allowed!")
	}

	out <- as.data.frame(matrix(NA,nrow=length(st.to),ncol=length(colClasses)))
	FUN.list <- getOption("pooling.functions")
	coverage_out <- matrix(0L,ncol=length(cn),nrow=length(st.to))
	colnames(coverage_out) <- cn
	sumBins <- function(wts,x){
			x <- na.exclude(x)
			if(length(x)==0){
				x <- wts <- 0
			} else if(!is.null(na.action(x))){
				wts <- wts[-na.action(x)]
			}
			out <- sum(x*wts) 
            if (out > 1) {
                1
            } else {
                out
            }
		}
	
	fuNames <- colClasses
	if(!is.null(FUN)){
		# check for recognized pooling FUN:
		for(i in which(sapply(FUN,is.character))){
			if(!(FUN[[i]] %in% names(FUN.list))){
				cat("Function:",FUN[[i]],"\n")
				stop("Argument 'FUN': Please provide either a function or a recognized 'pooling.function' (see getOption('pooling.functions'))")
			}
			fuNames[i] <- FUN[[i]]
			FUN <- FUN[-i]
		}
		FUN.ColClass <- FUN[names(FUN) %in% colClasses]
		FUN.Cols <- FUN[names(FUN) %in% cn]
		# check and add FUN assigned to colClasses
		for(i in seq_along(FUN.ColClass)){
			frml <- formals(FUN.ColClass[[i]])
			if(length(frml)!=4){
				frml2 <- alist(t=,u=,v=,w=)
				formals(FUN.ColClass[[i]]) <- c(frml,frml2[!(c("t","u","v","w") %in% names(frml))])[1:4]
			}
		}
		FUN.list[names(FUN.ColClass)] <- FUN.ColClass
		# check and add FUN assigned to columns
		for(i in seq_along(FUN.Cols)){
			frml <- formals(FUN.Cols[[i]])
			if(length(frml)!=4){
				frml2 <- alist(t=,u=,v=,w=)
				formals(FUN.Cols[[i]]) <- c(frml,frml2[!(c("t","u","v","w") %in% names(frml))])[1:4]
			}
		}
		FUN.list[names(FUN.Cols)] <- FUN.Cols
		# change function call names
		fuNames[names(FUN.Cols)] <- names(FUN.Cols)
	}

	# get bins
	bins <- cutIntervals(as.numeric(st_in,units="secs"),as.numeric(et_in,units="secs"),as.numeric(st.to,units="secs"),as.numeric(et.to,units="secs"))

	# loop over columns
	for(i in seq_along(dat)){
		if(fuNames[i] %in% c("sum","sum_se")){
			out[,i] <- mapply(function(bM,dt_out,cvrg,x,na.rm,dat,dt_in){
				FUN.list[["sum"]](x[bM[,1]],bM[,2],na.rm,dat[bM[,1],],dt_in[bM[,1]],dt_out)
			},bM=bins,dt_out = as.numeric(et.to - st.to, units="secs")
			,MoreArgs = list(cvrg=coverage[,i,drop=TRUE],x=dat[,i,keepAtts=FALSE],na.rm=na.rm,dat=dat
			,dt_in = as.numeric(et_in - st_in, units="secs")))
		} else {
			out[,i] <- sapply(bins,function(bM,cvrg,x,na.rm,dat){
				FUN.list[[fuNames[i]]](x[bM[,1]],bM[,2]*cvrg[bM[,1]],na.rm,dat[bM[,1],])
			},cvrg=coverage[,i,drop=TRUE],x=dat[,i,keepAtts=FALSE],na.rm=na.rm,dat=dat)
		}
		coverage_out[,i] <- sapply(bins,function(bM,x){
			sumBins(bM[,2],x[bM[,1]])

		},x=coverage[,i,drop=TRUE])
	}

	coverage_out[is.na(coverage_out)] <- 0
	names(out) <- cn
	out <- as.ibts.data.frame(out,st.to,et.to,colClasses,closed=closed,coverage=coverage_out)
	return(out)
}

# pool data.table
pool.data.table <- function(dat, granularity = NULL, st.to = NULL, 
    et.to = NULL, closed = 'st', format = NULL, tz = NULL, na.rm = TRUE, 
    FUN = NULL, to.ibts.format = format, to.ibts.tz = tz,
    by = NULL, st = 'st', et = 'et', ...) {
    if (is.null(by)) {
        num <- sapply(dat, is.numeric)
        # fix st/et columns
        num[c(st, et)] <- TRUE
        if (any(!num)) {
            by <- names(dat)[!num]
        }
    }
    if (is.null(by)) {
        out <- pool(
            as.ibts(dat, closed = closed, format = to.ibts.format, 
                tz = to.ibts.tz, ...), 
            granularity = granularity, st.to = st.to, et.to = et.to,
            closed = closed, format = format, tz = tz, na.rm = na.rm,
            FUN = FUN
        )
        as.data.table(out)
    } else {
        dat[, {
            out <- pool(
                as.ibts(.SD, closed = closed, format = to.ibts.format, 
                    tz = to.ibts.tz, ...), 
                granularity = granularity, st.to = st.to, et.to = et.to,
                closed = closed, format = format, tz = tz, na.rm = na.rm,
                FUN = FUN
            )
            as.data.table(out)
        }, by = by]
    }
}
