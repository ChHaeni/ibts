plot.ibts <- function(x, column = seq.int(min(2,ncol(x))), se = NULL, xlim = NULL, 
	ylim = NULL, min.coverage = 0.75, add = FALSE, xlab = "", ylab = NULL, 
	blank = (!is.null(type) && type == "n"), type = NULL,
	lty = 1, col = 1, lwd = 1, lty.v = lty, col.v = col, lwd.v = lwd,
	grid.col = "lightgrey", grid.lty = 3, gridv.col = grid.col, ylim2 = NULL, 
	col2 = "grey", lty2 = lty, lwd2 = lwd, lty.v2 = lty2, col.v2 = col2,
	lwd.v2 = lwd2, type2 = NULL, ylab2 = NULL, gap.size.max = NULL,
	gridv.lty = grid.lty, include.zero = FALSE, grid.y = NULL, 
	pret_n = 5, xlab_at = NULL, xlab_fmt = NULL, xlab_labels = NULL,
	shadeEdges = TRUE, border.col = col, dx = NULL, 
	na.mark = c("none", "simple", "lines"), na.jitter = FALSE, col.se = NULL, 
	border.se = NULL, alpha = 0.05, pcol = "#FFFFFFCC", pch = "+", 
	stats = c("cor", "lm", "rlm", "lmrob", "loess", "deming", "pbreg", "thielsen"),
	stats.Args = NULL, stats.pos = "topleft", stats.plot.CI95 = FALSE,
	npred = 1E4, reg.col = "blue", reg.lwd = 2, reg.lty = 1, leg.col = "black",
	ci.col = reg.col, ci.lty = 3, ci.lwd = 1, eq.xlab = NULL, eq.ylab = NULL, ...){
	if(is.ibts(column)){
		if(inherits(x,"formula")){
			# browser()
			formula <- x
			data <- column
			mf <- model.frame(formula,data,na.action=na.pass)
			if(formula[[3]] == 1L){
				mf <- cbind(mf,"(Intercept)"=0)
			}
			if(ncol(mf)!=2)stop("formula must contain exactly one argument on both sides!")
			mm <- cbind(mf,as.data.frame(data[,all.vars(formula)]))
			column <- x <- data[,1,drop=FALSE]
			x[] <- mf[,2,drop=FALSE]
			column[] <- mf[,1,drop=FALSE]
			names(x) <- names(mf)[2]
			names(column) <- names(mf)[1]
			# xName0 <- "dummy"
			# columnName0 <- "dummy"
		} else {
			if(ncol(x)!=1 | ncol(column)!=1)stop("argument 'x' and 'column' must contain exactly one column!")
			formula <- NULL
			# xName0 <- gsub("[[].*[]]$","",deparse(substitute(x)))
			# columnName0 <- gsub("[[].*[]]$","",deparse(substitute(column)))
			names(x) <- deparse(substitute(x))
			names(column) <- deparse(substitute(column))
		}
		if(min.coverage>0){
			covX <- attr(x,"coverage")
			covY <- attr(column,"coverage")
			x[covX<min.coverage] <- NA
			column[covY<min.coverage] <- NA
		}
		X <- as.data.frame(merge(x,column),keepAtts=FALSE)
		if(missing(lty)) lty <- NULL
		if(missing(stats)){
			type <- if(blank) "n" else "p"
			if(add){
				if(ncol(X)>2){
					points(X,col=col,lwd=lwd,lty=lty,pch=pch,type=type,...)
				} else {		
					points(X,col=col,lwd=lwd,lty=lty,pch=pch,type=type,...)
				} 
			} else {
				if(ncol(X)>2){
					plot(X,col=col,lwd=lwd,lty=lty,ylim=ylim,xlim=xlim,pch=pch,type=type,...)
				} else {		
					if(missing(xlab))xlab <- names(X)[1]
					if(missing(ylab))ylab <- names(X)[2]
					plot(X,col=col,lwd=lwd,lty=lty,ylim=ylim,xlim=xlim,pch=pch,xlab=xlab,ylab=ylab,type=type,...)
				} 			
			}
			invisible()
		} else {
			if(is.character(stats)){
				stats <- stats[1]
				isfu <- FALSE
			} else {
				statsCall <- stats
				stats <- deparse(substitute(stats))
				isfu <- TRUE
			}
			names(X) <- make.names(names(X))
			# xname <- names(x)[1]
			# yname <- names(column)[1]
	 		xname <- names(X)[1]
			yname <- names(X)[2]

			if(is.null(formula)){
				statsArgs0 <- list(formula=as.formula(paste(yname,xname,sep="~")),data=X)
			} else {
				statsArgs0 <- list(formula=formula,data=data)
			}
			if(!is.null(stats.Args))statsArgs0[names(stats.Args)] <- stats.Args
			formula <- statsArgs0$formula
			mf <- model.frame(formula,statsArgs0$data,na.action=na.pass)
			mm <- cbind(mf,as.data.frame(statsArgs0$data[,all.vars(formula)]))

			if(all(c("x","y") %in% names(statsArgs0))) statsArgs0$formula <- NULL
			if(stats=="cor"){
				statsArgs0 <- statsArgs0[names(statsArgs0) %in% c("x","y","use","method")]
				if(is.null(statsArgs0$x)) statsArgs0$x <- X[,xname]
				if(is.null(statsArgs0$y)) statsArgs0$y <- X[,yname]
				stats_out <- do.call(cor,statsArgs0)
			} else {
				if(stats=="rlm"){
					statsCall <- MASS::rlm
				} else if(stats=="lmrob"){
					statsCall <- robustbase::lmrob
				} else if(stats %in% c("deming","pbreg","thielsen")){
					require(deming)
					statsCall <- stats
				} else if(!isfu){
					statsCall <- stats
				}
				stats_out <- do.call(statsCall,statsArgs0)
				stats_out$call[["data"]] <- quote(X)
			}	
			if("formula" %in% names(statsArgs0)){
				xx <- X[,xname]
				yy <- X[,yname]
			} else {
				if(is.character(statsArgs0$x)){
					xname <- statsArgs0$x
					xx <- X[,xname]	
				}else {
					xx <- statsArgs0$x
					xname <- "x"
				}
				if(is.character(statsArgs0$y)){
					yname <- statsArgs0$y
					yy <- X[,yname]	
				}else {
					yy <- statsArgs0$y
					yname <- "y"
				}
			}
			if(missing(xlab)) xlab <- names(x)[1]
			if(missing(ylab)) ylab <- names(column)[1]
			if(add){
				if(blank){
					points(xx,yy,type="n",...)
				} else {
					points(xx,yy,pch=pch,col=col,lwd=lwd,lty=lty,type=type,...)
				}
			} else {
				if(blank){
					plot(xx,yy,ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab,type="n",...)
				} else {
					plot(xx,yy,col=col,lwd=lwd,lty=lty,ylim=ylim,xlim=xlim,pch=pch,xlab=xlab,ylab=ylab,type=type,...)
				}			
			}
			if(stats=="cor"){
				legend(stats.pos,legend=sprintf("R = %1.2f",stats_out),bty="n",text.font=2,cex=0.75)
			} else if(stats!="loess"){
				cfs <- coef(stats_out)
				if(abs(cfs[1])<1E-2){
					cfs1str <- " = %1.1e"
				} else {
					cfs1str <- " = %1.2f"
				}
				if(length(cfs)==1L){
					cfs[2] <- 0
				}
				cfs2 <- abs(cfs[2])
				if(cfs2<1E-2){
					cfs2str <- " + %1.1e \u00D7 "
				} else {
					cfs2str <- " + %1.2f \u00D7 "
				}
				if(cfs[2]<0) cfs2str <- sub("[+]","-",cfs2str)
				if(is.null(eq.xlab)) eq.xlab <- xlab		
				if(is.null(eq.ylab)) eq.ylab <- ylab		
				cfsstr <- paste0(eq.ylab,cfs1str,cfs2str,eq.xlab) 
				abline(cfs[1],cfs[2],col=reg.col,lwd=reg.lwd,lty=reg.lty,untf=TRUE)
				if(stats.plot.CI95){
					mm <- na.omit(mm)
					xx <- c(which.min(mm[,2]),which.max(mm[,2]))
					nms <- all.vars(formula)
					MM <- mm[,-c(1,2)][,nms]
					MM2 <- as.data.frame(matrix(0,ncol=NCOL(MM),nrow=npred))
					for(i in seq_len(NCOL(MM2))){
						MM2[,i] <- seq(MM[xx[1],i],MM[xx[2],i],length.out=npred)
					}
					names(MM2) <- nms
					x <- model.frame(formula,MM2,na.action=na.pass)[,2]
					y <- predict(stats_out,newdata=MM2)
					y2 <- predict(stats_out,newdata=MM2,se=TRUE)
					lines(x,y + y2$se.fit*qnorm(0.975),col=ci.col,lty=ci.lty,lwd=ci.lwd)
					lines(x,y + y2$se.fit*qnorm(0.025),col=ci.col,lty=ci.lty,lwd=ci.lwd)
				}
				legend(stats.pos,legend=sprintf(cfsstr,cfs[1],cfs2),bty="n",text.font=2,cex=0.75,text.col=leg.col)
			} else {
				mm <- na.omit(mm)
				xx <- c(which.min(mm[,2]),which.max(mm[,2]))
				nms <- all.vars(formula)
				MM <- mm[,-c(1,2)][,nms]
				MM2 <- as.data.frame(matrix(0,ncol=NCOL(MM),nrow=npred))
				for(i in seq_len(NCOL(MM2))){
					MM2[,i] <- seq(MM[xx[1],i],MM[xx[2],i],length.out=npred)
				}
				names(MM2) <- nms
				x <- model.frame(formula,MM2,na.action=na.pass)[,2]
				y <- predict(stats_out,newdata=MM2)
				lines(x,y,col=reg.col,lwd=reg.lwd,lty=reg.lty)
				if(stats.plot.CI95){
					y2 <- predict(stats_out,newdata=MM2,se=TRUE)
					lines(x,y + y2$se.fit*qnorm(0.975),col=ci.col,lty=ci.lty,lwd=ci.lwd)
					lines(x,y + y2$se.fit*qnorm(0.025),col=ci.col,lty=ci.lty,lwd=ci.lwd)
				}
			}
			invisible(stats_out)
		}
	} else {
		original_column <- column
		for(column in original_column){
			if(min.coverage>0){
				covR <- attr(x[,column],"coverage")
				x[covR<min.coverage,column] <- NA
			}
			cC <- attr(x[,column],"colClasses")
			y <- x[,column,drop=TRUE]

			##### adjust second y-axis
			if(match(column,original_column)==2){
				add <- TRUE
				yr <- range(y,na.rm=TRUE)
				if(include.zero){
					yr <- range(c(yr,0))
				} else if(cC=="circ" && is.null(ylim2)){
					yr <- c(0,360)
				}
				if(!is.null(ylim2)){
					yr <- ylim2
					cat("ylim2 != NULL anpassen!\n")
				}
				py <- pretty(par()$yaxp[1:2])
				yl <- range(py)
				y <- (y - yr[1])/diff(yr)*diff(yl) + yl[1]
				col <- col2
				lty <- lty2
				lwd <- lwd2
				lty.v <- lty.v2
				col.v <- col.v2
				lwd.v <- lwd.v2
				type <- type2
				pyl <- (py - yl[1])/diff(yl)*diff(yr) + yr[1]
				mpy <- max(-floor(log10(abs(diff(pyl)))),-floor(log10(abs(pyl[pyl != 0]))))
				if(abs(mpy) >= 4){
					pyl_fmt <- "%1.2e"
				} else {
					if(all(!as.logical(pyl %% 1))){
						pyl_fmt <- "%1.0f"
					} else {
						# pyl_fmt <- paste0("%1.",max(mpy,0)+1,"f")
						pyl_fmt <- paste0("%1.",max(mpy,0),"f")
					}
				}		
				axis(4,at=py,labels=sprintf(pyl_fmt,pyl), lty = if(drawaxes) 1 else 0)
				if(is.null(ylab2)){
					if(is.character(column)){
						ylab2 <- column
					} else {
						ylab2 <- names(x)[column]
					}					
				}
				mtext(ylab2,side=4,line=2.75, las = 0)
			}
			if(!is.null(type) && type %in% c("avg","sum","circ","num","min","max")){
				cC <- type
				type <- NULL
			}
			if(cC %in% c("min","max")){
				cC <- "num"
			}

			if(!is.null(gap.size.max)){
				gap.size.max <- parse_time_diff(gap.size.max)
				browser()
				stop("remove.gaps has not been implemented yet.")
				# if(length(original_column) > 1){
				# 	na.ind <- is.na(x[,original_column[1]]) & is.na(x[,original_column[2]])
				# } else {
				# 	na.ind <- is.na(x[,original_column])
				# }
				# # gps.ind <- datagaps(x[,original_column],check.all=TRUE,return.indices=TRUE)
				# if(sum(na.ind)){
				# 	x <- x[!na.ind,]
				# 	y <- y[!na.ind]
				# }
				x1 <- attr(x, "st")
				x2 <- attr(x, "et")
				
				gps <- datagaps(x[,original_column],check.all=TRUE,invert=TRUE)[[1]]
				gps <- gps[gps$duration > gap.size.max,]
				gps$mt <- gps$st + gps$duration/2
				
				xx <- pretty_dates(gps$mt,n=nrow(gps))

				xl <- c(x1[1],rev(x2)[1])
				ptx <- pretty_dates(with_tz(xl,tzone(x)),pret_n)
				if(!is.null(xlim)){
					if(is.character(xlim)){
						if(length(xlim)==1){
							xlim <- parse_timerange(xlim,tz=tzone(x))
						} else {
							xlim <- parse_date_time3(xlim,tz=tzone(x),quiet=TRUE)
						}			
					} else if(is.numeric(xlim)){
						xlim <- c(x1[xlim[1]],x2[xlim[2]])
					} else {
						xlim <- as.POSIXct(xlim)
					}
					ptx <- pretty_dates(xlim,pret_n)
				}
				if(is.null(xlab_fmt)){
					b <- as.numeric(ptx[2] - ptx[1],"secs")
					if(floor(b/(24*3600)) > 0){
						if(floor(b/(24*3600)) > 2){
							xlab_fmt <- "%d/%m"
						} else {
							xlab_fmt <- "%d/%m %H:%M"
						}
					} else if(floor(b/60) > 0){
						xlab_fmt <- "%H:%M"
					} else {
						xlab_fmt <- "%H:%M:%S"
					}
					# if(b%%(24*3600)>0.5){
					# 	xlab_fmt <- "%H:%M"
					# } else {
					# 	xlab_fmt <- "%d/%m"
					# }
				}
			} else {
				x1 <- attr(y, "st")
				x2 <- attr(y, "et")
				xl <- c(x1[1],rev(x2)[1])
				ptx <- pretty_dates(with_tz(xl,tzone(x)),pret_n)
				if(!is.null(xlim)){
					if(is.character(xlim)){
						if(length(xlim)==1){
							xlim <- parse_timerange(xlim,tz=tzone(x))
						} else {
							xlim <- parse_date_time3(xlim,tz=tzone(x),quiet=TRUE)
						}			
					} else if(is.numeric(xlim)){
						xlim <- c(x1[xlim[1]],x2[xlim[2]])
					} else {
						xlim <- as.POSIXct(xlim)
					}
					ptx <- pretty_dates(xlim,pret_n)
				}
				if(is.null(xlab_fmt)){
					b <- as.numeric(ptx[2] - ptx[1],"secs")
					if(floor(b/(24*3600)) > 0){
						if(floor(b/(24*3600)) > 2){
							xlab_fmt <- "%d/%m"
						} else {
							xlab_fmt <- "%d/%m %H:%M"
						}
					} else if(floor(b/60) > 0){
						xlab_fmt <- "%H:%M"
					} else {
						xlab_fmt <- "%H:%M:%S"
					}
					# if(b%%(24*3600)>0.5){
					# 	xlab_fmt <- "%H:%M"
					# } else {
					# 	xlab_fmt <- "%d/%m"
					# }
				}
			}
			yl <- range(y,na.rm=TRUE)
			if(!add&&missing(ylim)&&!all(is.finite(yl))){
				stop("Data contains infinite values! Please provide argument ylim!")
			}
			if(!add){
				if(cC=="circ"){
					if(missing(ylim)){
						ylim <- yl <- c(0,360)	
					}
					if(missing(grid.y)) grid.y <- seq(0,360,90)
				}
				if(include.zero){
					yl <- range(c(yl,0))
				}
				if(is.null(ylab)){
					if(is.character(column)){
						ylab <- column
					} else {
						ylab <- names(x)[column]
					}
				}
				if(missing(ylim) && !is.null(se)){
					ylim <- c(
						min(y + x[,se,drop=TRUE]*qnorm(alpha/2),na.rm=TRUE)
						,max(y + x[,se,drop=TRUE]*qnorm(1 - alpha/2),na.rm=TRUE)
						)				
				}
				dots <- list(...)
				if("axes" %in% names(dots)){
					drawaxes <- dots$axes
				} else {
					drawaxes<- TRUE
				}
				if(is.null(grid.y)){
					plot(xl,yl,type="n",xaxt="n",xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...)
					grid(nx=NA,ny=NULL,col=grid.col,lty=grid.lty,equilogs=FALSE)
				} else if(all(is.na(grid.y))){
					plot(xl,yl,type="n",xaxt="n",xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...)
				} else {
					plot(xl,yl,type="n",xaxt="n",yaxt="n",xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...)
					if(all(is.finite(grid.y))){
						abline(h=grid.y,col=grid.col,lty=grid.lty)
						axis(2,at=grid.y, lty = if(drawaxes) 1 else 0)
					}
				}
				if(!is.null(xlab_at)){
					ptx <- as.numeric(xlab_at, units = "secs")
				}
				if(!is.null(xlab_labels)){
					axis(1,at=ptx,labels=xlab_labels, lty = if(drawaxes) 1 else 0)
				} else if(isFALSE(xlab_fmt)){
					axis(1,at=ptx,labels=FALSE, lty = if(drawaxes) 1 else 0)
				} else {
					axis(1,at=ptx,labels=format(ptx, xlab_fmt), lty = if(drawaxes) 1 else 0)
				}
				abline(v=ptx,col=gridv.col,lty=gridv.lty)
			}
			if(!blank){
				if(cC=="sum"){
					if(match(column,original_column)==2){
						zero <- -yr[1]/diff(yr)*diff(yl) + yl[1]
					} else {
						zero <- 0
					}
					if(missing(col)){
						col <- "darkblue"
					} 
					if(is.null(type)){
						if(is.null(dx)){
							dx <- as.numeric(x2 - x1,units="secs")/10
						} else {
							dx <- as.numeric(x2 - x1,units="secs")/dx
						}
						x1 <- as.numeric(x1,units="secs")
						x2 <- as.numeric(x2,units="secs")
						xx <- as.vector(rbind(x1 + dx,x2 - dx,x2 - dx,x1 + dx,0))
						yy <- as.vector(rbind(y,y,0,0,NA))
						polygon(xx,yy,col=col,border=border.col,lty=lty,lwd=lwd)
					} else {
						x3 <- x1 + (x2-x1)/2
						lines(x3,y,col=col,lty=lty,lwd=lwd,type=type,pch=pch)				
					}
				} else if(cC=="circ"){
					dy <- diff(yrm <- na.exclude(y))
					sdy <- sign(dy)
					m <- c(0,cumsum(floor(abs(dy)/180)*sdy))
					um <- unique(m)
					ind <- seq_along(y)
					if(!is.null(na.action(yrm))){
						ind <- ind[-na.action(yrm)]
					}
					y2 <- y[ind] - m*360
					y2r <- floor(range(y2)/360)
					y3 <- y
					if(is.null(type)){
						for(j in seq(y2r[1],y2r[2])){
							y3[ind] <- y2 - j*360
							xx1 <- as.numeric(x1,units="secs")
							xx2 <- as.numeric(x2,units="secs")
							# horizontal lines:
							xx <- as.vector(rbind(xx1,xx2,xx2))
							yy <- as.vector(rbind(y3,y3,NA))
							lines(xx,yy,col=col,lty=lty,lwd=lwd)
							# vertical lines:
							if(length(xx1) > 1){
								xx1 <- xx1[-1]
								xx2 <- xx2[-length(xx2)]
								xx <- as.vector(rbind(xx1,xx1,xx1))
								yInd <- round(xx1-xx2)!=0
								yy <- rbind(y3[-length(y3)],y3[-1],NA)
								yy[,yInd] <- NA
								lines(xx,as.vector(yy),col=col.v,lty=lty.v,lwd=lwd.v)
							}
						}
					} else {
						x3 <- x1 + (x2-x1)/2
						for(j in seq(y2r[1],y2r[2])){
							y3[ind] <- y2 - j*360
							lines(x3,y3,col=col,lty=lty,lwd=lwd,type=type,pch=pch)
						}				
					}
					if(is.null(type)||type=="b"||type=="l"&&shadeEdges){
						usr <- par()$usr
						polygon(c(usr[1],usr[2],usr[2],usr[1]),c(0,0,usr[3],usr[3]),col=pcol,border=NA)
						polygon(c(usr[1],usr[2],usr[2],usr[1]),c(360,360,usr[4],usr[4]),col=pcol,border=NA)
						drawbox <- drawaxes <- TRUE
						dots <- list(...)
						cat("check par()$bty einbauen!\n")
						if("axes" %in% names(dots)){
							drawbox <- drawaxes <- dots$axes
						}
						if("frame.plot" %in% names(dots)){
							drawbox <- dots$frame.plot
						}
						if(drawbox){
							box()
						} else if(drawaxes) {
							xaxp <- par()$xaxp
							lines(c(xaxp[1],xaxp[2]),c(usr[3],usr[3]))
						}
					}
				} else if(cC=="num"){
					x3 <- x1 + (x2-x1)/2
					if(is.null(type)){
						type <- "b"
					}
					lines(x3,y,col=col,lty=lty,lwd=lwd,type=type,pch=pch)
				} else {
					if(!is.null(se)){
						if(is.null(col.se)|(isAlpha <- is.character(col.se)&&nchar(col.se)==2)){
							if(is.character(col)&&grepl("[#]",col)){
								col.se <- paste0(substring(col,1,7),ifelse(isAlpha,col.se,"55"))
							} else {
								col.se <- paste0(gplots_col2hex(col),ifelse(isAlpha,col.se,"55"))
							}
						}
						if(is.null(border.se)|(isAlpha <- is.character(border.se)&&nchar(border.se)==2)){
							if(is.character(col)&&grepl("[#]",col)){
								border.se <- paste0(substring(col,1,7),ifelse(isAlpha,border.se,"22"))
							} else {
								border.se <- paste0(gplots_col2hex(col),ifelse(isAlpha,border.se,"22"))
							}
						}
						y1 <- as.vector(y + x[,se,drop=TRUE]*qnorm(1 - alpha/2))
						y2 <- as.vector(y + x[,se,drop=TRUE]*qnorm(alpha/2))
						NAs <- is.na(y1)
						# bl <- matrix(c(if(NAs[1])numeric(0) else 1,which(diff(NAs)!=0),if(NAs[length(NAs)])numeric(0) else length(NAs)),nrow=2)
						# include st!=et
						wNA <- c(if(NAs[1])numeric(0) else 1,which(diff(NAs)!=0),if(NAs[length(NAs)])numeric(0) else length(NAs))
						stet <- which(x1[-1]!=x2[-length(x2)])
						wNA <- sort(c(wNA,stet,stet + 1))
						bl <- matrix(wNA,nrow=2)
						for(i in 1:ncol(bl)){
							ind <- bl[1,i]:bl[2,i]
							yy <- rep(c(y1[ind],rev(y2[ind])),each=2)
							xx <- rbind(x1[ind],x2[ind])[seq.int(length(ind)*2)]
							xx <- c(xx,rev(xx))
							polygon(xx,yy,col=col.se,border=border.se,lend=2) 	
						}
					}		
					if(is.null(type)){
						x1 <- as.numeric(x1,units="secs")
						x2 <- as.numeric(x2,units="secs")
						# horizontal lines:
						xx <- as.vector(rbind(x1,x2,x2))
						yy <- as.vector(rbind(y,y,NA))
						lines(xx,yy,col=col,lty=lty,lwd=lwd)
						# vertical lines:
						if(length(x1) > 1){
							x1 <- x1[-1]
							x2 <- x2[-length(x2)]
							xx <- as.vector(rbind(x1,x1,x1))
							yInd <- round(x1-x2)!=0
							yy <- rbind(y[-length(y)],y[-1],NA)
							yy[,yInd] <- NA
							lines(xx,as.vector(yy),col=col.v,lty=lty.v,lwd=lwd.v)
						}
					} else {
						x3 <- x1 + (x2-x1)/2
						lines(x3,y,col=col,lty=lty,lwd=lwd,type=type,pch=pch)
					}	
				}
				if(na.mark[1]!="none"&&any((cvrg <- attr(y,"coverage"))<1)){
					mt <- x1 + (x2 - x1)/2
					if(na.jitter){
						mt <- mt[1] + jitter(as.numeric(mt - mt[1]),factor=2)
					}
					ind <- which(cvrg<1)
					if(na.mark[1]=="lines"){
						sapply(ind,function(x){lines(rep(mt[x],2),c(yl[1],y[x]),col="lightgrey");NULL})
					}
					rug(mt[ind])
				}
			}
		}
	# }
	invisible()
	}
}

