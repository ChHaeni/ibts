barplot.ibts <- function(height, column = NULL, se = NULL, width = 1, space = NULL,  
    legend.text = NULL, density = NULL, alpha=0.05, notchwidth = 0.6,
    angle = 45, col = NULL, border = par("fg"), main = NULL, 
    sub = NULL, xlab = "", ylab = NULL, xlim = NULL, ylim = NULL, 
    xpd = TRUE, log = "", plot = TRUE, p.cols = c("#0000FF05","transparent"),
    offset = 0, add = FALSE, args.legend = NULL, pret_n = 5,
    ...){
   if(is.null(column)) column <- names(height)
    x.l <- st(height)
    x.r <- et(height)
    tz <- tzone(height)
    if(!is.null(se)){
      if(is.character(se)) se <- c(t(as.matrix(height[,se])))
      if(is.data.frame(se))se <- c(t(as.matrix(se)))
    }
    height <- t(as.matrix(height[,column]))
    if (is.null(space)) space <- c(0, 1)
    space <- space * mean(width)

    if (is.null(col)) col <- gray.colors(nrow(height))

    if (is.logical(legend.text)) legend.text <- if (legend.text) rownames(height)
    stopifnot(is.character(log))
    logx <- logy <- FALSE
    if (log != "") {
        logx <- length(grep("x", log)) > 0L
        logy <- length(grep("y", log)) > 0L
    }
    if ((logx || logy) && !is.null(density)) stop("Cannot use shading lines in bars when log scale is used")
    NR <- nrow(height)
    NC <- ncol(height)

     
    dx <- as.numeric(x.r - x.l,units="secs")
   
    if (length(space) != 2) stop("argument space must be a vector of length 2!")

    width <- rep_len(width, NR)
    space2 <- cumsum(width + c(space[2L]/2, rep.int(space[1L],NR - 1)))

    offset <- rep_len(as.vector(offset), length(width))
    delta <- width/2
    w.r <- x.l[rep(seq.int(NC),each=NR)] + dx[rep(seq.int(NC),each=NR)]*space2/(NR+1)
    w.m <- w.r - delta*dx[rep(seq.int(NC),each=NR)]/(NR+1)
    w.l <- w.m - delta*dx[rep(seq.int(NC),each=NR)]/(NR+1)

	if(!is.null(xlim)){
		if(is.character(xlim)){
			if(length(xlim)==1){
				xlim <- parse_timerange(xlim,tz=tz)
			} else {
				xlim <- parse_date_time3(xlim,orders=getOption("time.orders"),tz=tz,quiet=TRUE)
			}			
		} else if(is.numeric(xlim)){
			xlim <- c(x.l[xlim[1]],x.r[xlim[2]])
		} else {
			xlim <- as.POSIXct(xlim)
		}
		ptx <- pretty_dates(xlim,pret_n)
	} else {
		xlim <- c(min(w.l), max(w.r))
	}
    log.dat <- logx || logy
    if (log.dat){
        if (min(height + offset, na.rm = TRUE) <= 0) 
            stop("log scale error: at least one 'height + offset' value <= 0")
        if (logx && !is.null(xlim) && min(xlim) <= 0) 
            stop("log scale error: 'xlim' <= 0")
        if (logy && !is.null(ylim) && min(ylim) <= 0) 
            stop("log scale error: 'ylim' <= 0")
        rectbase <- if (logy && !is.null(ylim)) 
            ylim[1L]
        else 0.9 * min(height, na.rm = TRUE)
    } else {
      rectbase <- 0
    }
    rAdj <- offset + (if (log.dat) 
        0.9 * height
    else -0.01 * height)


    # if (is.null(xlim)) 
    #     xlim <- c(min(w.l), max(w.r))
    if (is.null(ylim)) 
        ylim <- range(rAdj, height + offset, na.rm = TRUE)

    w.m <- matrix(w.m, ncol = NC)
    if (plot) {
        if (!add){
          if(is.null(ylab)) ylab <- rownames(height)[1]
      ptx <- pretty_dates(xlim,pret_n)
        b <- as.numeric(ptx[2] - ptx[1],"secs")
        if(b%%(24*3600)>0.5){
                xlab_fmt <- "%H:%M"
        } else {
                xlab_fmt <- "%d/%m"
        }
      plot(1,1,type="n",xlim=xlim, ylim=ylim, log = log, main = main, sub = sub, xlab = xlab, ylab = ylab, xaxt="n", ...)
      axis(1,at=ptx,labels=format(ptx,xlab_fmt,tz=tz))
      usr <- par()$usr
      for(i in seq_along(x.l))polygon(c(x.l[i],x.r[i],x.r[i],x.l[i]),c(rep(usr[3:4],each=2)),col=p.cols[(i-1)%%2+1],border=NA)
      polygon(c(as.numeric(x.r[i]),usr[2],usr[2],as.numeric(x.r[i])),c(rep(usr[3:4],each=2)),col=p.cols[i%%2+1],border=NA)
      grid(nx=NA,ny=NULL,col="lightgrey",lty=3)
      abline(v=ptx,col="lightgrey",lty=3)
      box()
        }
        y <- c(height) + offset
        rect(w.l, rectbase + offset, w.r, y, 
          angle = angle, density = density, 
          col = col, border = border)

        if(!is.null(se)){
          y.lo <- y + se*qnorm(1-alpha/2)
          y.hi <- y + se*qnorm(alpha/2)
          for(i in seq_along(y)){
            lines(rep(w.m[i],2),c(y[i],y.lo[i]),lend=2)
            lines(rep(w.m[i],2),c(y[i],y.hi[i]),lend=2)
            lines(w.m[i] + c(-0.5,0.5)*notchwidth*as.numeric(w.r[i]-w.l[i],units="secs"),rep(y.lo[i],2),lend=2)
            lines(w.m[i] + c(-0.5,0.5)*notchwidth*as.numeric(w.r[i]-w.l[i],units="secs"),rep(y.hi[i],2),lend=2)
          }
        }
        if (!is.null(legend.text)) {
            legend.col <- rep_len(col, length(legend.text))
            xy <- par("usr")
            if (is.null(args.legend)) {
                legend(xy[2L] - xinch(0.1), xy[4L] - yinch(0.1), 
                  legend = legend.text, angle = angle, density = density, 
                  fill = legend.col, xjust = 1, yjust = 1)
            }
            else {
                args.legend1 <- list(x = xy[2L] - xinch(0.1), 
                  y = xy[4L] - yinch(0.1), legend = legend.text, 
                  angle = angle, density = density, fill = legend.col, 
                  xjust = 1, yjust = 1)
                args.legend1[names(args.legend)] <- args.legend
                do.call("legend", args.legend1)
            }
        }
        invisible(w.m)
    }
    else w.m
}
