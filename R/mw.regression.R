mw.regression <-
function(formula,data,window=21,min.counts=2/3,FUN=c("rlm","lmrob","lm","deming","pbreg","thielsen"),stats=coef
	,RegPlot=FALSE,model.list=FALSE,...){
	if(min.counts>1){
		min.counts <- 1
		warning("argument 'min.counts' can not be larger than 1 and has been reset to 1!")
	}
	FUN <- FUN[1]
	switch(FUN
		, "deming" =
		, "pbreg" =
		, "thielsen" = {
            if(!requireNamespace("deming", quietly = TRUE)){
                stop("package 'deming' is missing - please install the package by running install.packages('deming')")
            }
        }
		, "rlm" = {
            if(!requireNamespace("MASS", quietly = TRUE)){
                stop("package 'MASS' is missing - please install the package by running install.packages('MASS')")
            }
        }
		, "lmrob" = {
            if(!requireNamespace("robustbase", quietly = TRUE)){
                stop("package 'robustbase' is missing - please install the package by running install.packages('robustbase')")
            }
        }
		)
	ArgList <- match.call(expand.dots=FALSE)[["..."]]
	if("subset" %in% names(ArgList)){
		data <- data[eval(ArgList$subset,parent.frame()),]
		if("weights" %in% names(ArgList))ArgList$weights <- ArgList$weights[ArgList$subset]
	}
	stindex <- st(data)
	etindex <- et(data)
	ArgList <- c(list(formula=formula,data=as.data.frame(data,keepAtts=FALSE)),ArgList)
	d_t <- unique(as.numeric(etindex-stindex,units="secs"))
	if(length(d_t)>1)stop("only equi-distant time series allowed!")
	if(is.character(window)|is.difftime(window)){
		window <- parse_time_diff(window)/d_t
		winhalf <- window/2
	} else {
		window <- floor(window)
		if(window%%2 == 0){
			warning("extended window to nearest uneven number (oder so)")
			window <- window + 1
		}
		winhalf <- (window - 1)/2
	}

	#
	winInd <- as.numeric(seq(-winhalf,winhalf)*d_t,units="secs")
	stnum <- (as.numeric(stindex) + as.numeric(etindex))/2
	inds <- mwIndices(winhalf,d_t,stnum)
	#

	if(RegPlot){
		mf <- model.frame(formula,data)
		fuOut <- function(ind,format=NULL,tz=tzone(stindex),pch="+",p.col=1,lwd=2,l.col="blue",xlab=NULL,ylab=NULL
			,main=NULL,panel.first=grid(),Equation=TRUE,Equ.pos="topleft",...){
			if(is.character(ind)){
				if(is.null(format)){
					format <- getOption("time.orders")
				}
				ind <- parse_date_time3(ind,orders=format,tz=tz)
			} else if(is.numeric(ind)) {
				ind <- stindex[ind]
			}
			# winInd <- as.numeric(seq(-winhalf,winhalf)*d_t,units="secs")
			# stnum <- as.numeric(stindex)
			# stnum <- (as.numeric(stindex) + as.numeric(etindex))/2
			# ind <- stnum[which(ind>=stindex & ind < etindex)]
			ind <- which(ind>=stindex & ind < etindex)
			if(length(ind)==0)stop("time outside timeseries")
			# ArgList$subset <- findInterval(winInd+ind,stnum)
			ArgList$subset <- inds[[ind]]
			nms <- names(mf)
			if(is.null(xlab)) xlab <- nms[2]
			if(is.null(ylab)) ylab <- nms[1]
			TimeString <- paste(format(stindex[ArgList$subset][1]),"to",format(rev(etindex[ArgList$subset])[1]))
			if(is.null(main)) main <- TimeString
			do.call(plot,c(alist(mf[,c(2,1)],type="n",xlab=xlab,ylab=ylab,main=main,panel.first=panel.first),list(...)))
			do.call(points,c(ArgList[names(ArgList) %in% c("formula","data","subset")],pch=pch,col=p.col))
			if(sum(ArgList$subset != 0) >= (min.counts*window)){
				out <- try(do.call(FUN,ArgList),silent=TRUE)
				if(!inherits(out,"try-error")){
					cfs <- coef(out)		
					if(FUN %in% c("deming","pbreg","thielsen")){
						x <- out$model[,2]
						xrng <- range(x,na.rm=TRUE)
						yrng <- cfs[1] + cfs[2]*xrng
					} else {
						x <- model.matrix(out)[,2]
						y <- predict(out)
						xrng <- range(x,na.rm=TRUE)
						yrng <- y[match(xrng,x)]
					}
					lines(xrng,yrng,col=l.col,lwd=lwd)
			  		if(Equation){
				  		if(abs(cfs[1])<1E-2){
				  			cfs1str <- " = %1.1e"
				  		} else {
				  			cfs1str <- " = %1.2f"
				  		}
				  		cfs2 <- abs(cfs[2])
				  		if(cfs2<1E-2){
				  			cfs2str <- " + %1.1e \u00D7 "
				  		} else {
				  			cfs2str <- " + %1.2f \u00D7 "
				  		}
				  		if(cfs[2]<0) cfs2str <- sub("[+]","-",cfs2str)	  		
				  		cfsstr <- paste0(nms[1],cfs1str,cfs2str,nms[2])
				  		legend(Equ.pos,legend=sprintf(cfsstr,cfs[1],cfs2),bty="n",text.font=2,cex=0.75)
				  	}
				}
			}
			print(TimeString)
		}
		# environment(fuOut) <- list2env(list(stindex=stindex,etindex=etindex,ArgList=ArgList,mf=mf,min.counts=min.counts,FUN=FUN,winhalf=winhalf,d_t=d_t))
		environment(fuOut) <- list2env(list(stindex=stindex,etindex=etindex,ArgList=ArgList,mf=mf,min.counts=min.counts,FUN=FUN,inds=inds))
	} else {
		fuOut <- NULL
	}
	
	if(model.list){
		# winInd <- as.numeric(seq(-winhalf,winhalf)*d_t,units="secs")
		# stnum <- as.numeric(stindex)
		# stnum <- (as.numeric(stindex) + as.numeric(etindex))/2
		# inds <- mwIndices(winhalf,d_t,stnum)
		out <- lapply(inds,function(x){
			ArgList$subset <- x
			if(sum(ArgList$subset != 0) >= (min.counts*window)){
				res <- try(do.call(FUN,ArgList),silent=TRUE)
				if(inherits(res,"try-error")){
					res <- NA
				}
			} else {
				res <- NA
			}
			res
		})
		structure(out,st=stindex,et=etindex,tzone=tzone(data),RegPlot=fuOut)
	} else {
		dummy <- NULL
		# winInd <- as.numeric(seq(-winhalf,winhalf)*d_t,units="secs")
		# stnum <- as.numeric(stindex)
		# stnum <- (as.numeric(stindex) + as.numeric(etindex))/2
		frmls <- formals(FUN)
		if(!("..." %in% names(frmls))){
			chk <- names(ArgList) %in% names(frmls)
			if(any(!chk)){
				if(sum(!chk)>1){
					warning(paste0(paste(sQuote(names(ArgList)[!chk]),collapse=", ")," are not valid arguments and have been ignored."))
				} else {
					warning(paste0(sQuote(names(ArgList)[!chk])," is not a valid argument and has been ignored."))
				}
				ArgList <- ArgList[chk]
			}
		}
		# inds <- mwIndices(winhalf,d_t,stnum)
		out <- lapply(inds,function(x){
			ArgList$subset <- x
			if(sum(ArgList$subset != 0) >= (min.counts*window)){
				mod <- try(do.call(FUN,ArgList),silent=TRUE)
				if(!inherits(mod,"try-error")){		
					res <- stats(mod)
				} else {
					res <- NA
				}
			} else {
				res <- NA
			}
			res
		})
		isna <- sapply(out,function(x)identical(is.na(x),TRUE))
		if(all(isna)){
			stop(paste0("No valid model results!\nwindow (# of intervals): ",window,"\nmin. # of intervals accepted: ",round(min.counts*window,1)))
		}
		dummy <- out[!isna][[1]]
		if(is.null(names(dummy))){
			names(dummy) <- paste0("stats.",seq.int(length(dummy)))
		}
		dfout <- data.frame(matrix(NA,nrow=length(out),ncol=length(dummy)))
		names(dfout) <- paste(FUN,names(dummy),sep="_")
		if(length(dummy)==1){
			dfout[!isna,] <- unlist(out[!isna])
		} else {
			dfout[!isna,] <- do.call(rbind,out[!isna])
		}
		out <- as.ibts.data.frame(dfout,st=stindex,et=etindex,tzone=tzone(data),colClasses=rep("num",length(dummy)),closed=closed(data))
		attr(out,"RegPlot") <- fuOut
		out		
	}
}
