addWD <- function (WD, frac = 25, y0 = NULL, lwd = 3, length = NULL, angle = 30
  , U = NULL, addUlegend = FALSE, ULegendPos = "right", inset = c(0.04, 0)
  , breaks = function (x) median(x, na.rm = TRUE) * c(0.5, 1, 2), ...){

  usr <- par("usr")
  pin <- par("pin")
  dx <- diff(usr[1:2])
  dy <- diff(usr[3:4])

  if (!is.null(U[[1]])){
    ddy <- dy / frac * U[[1]]
  } else {
    ddy <- dy / frac
  }

  ddx <- ddy / dy * dx / pin[1] * pin[2]

  if (is.null(y0)){
    y0 <- usr[4] - 0.04 * dy
  }
  y1 <- y0 - ddy
  if(length(y1) == 1){
    y1 <- rep(y1, length(WD[[1]]))
    ddy <- rep(ddy, length(WD[[1]]))
    ddx <- rep(ddx, length(WD[[1]]))
  }

  for(i in seq_along(WD[[1]])){

    wd1 <- WD[i]
    if(!is.na(wd1[[1]])){
      mx <- mt(wd1)
      my <- (y0+y1[i])/2

      rad1 <- as.numeric((-wd1 - 180) %% 360)/180*pi

      vnx <- -sin(rad1)*ddx[i]
      vny <- cos(rad1)*ddy[i]
      # browser()
      arrows(mx - vnx/2,my - vny/2,mx + vnx/2,my + vny/2,lwd=lwd,length=if(missing(length)) 0.35*ddy[i]*pin[2]/dy else length,angle=angle,...)
    }
  }

  if(addUlegend){
    U_lims <- range(U[[1]],na.rm=TRUE)
    # Us <- quantile(U,c(0.25,0.5,0.75))
    # Us <- quantile(U,c(0.2,0.4,0.8))
    # Us <- c(0.5,1,2)*median(U)
    if(is.function(breaks)){
      Us <- breaks(U[[1]])
    } else {
      Us <- breaks
    }
    lens <- dx/frac*Us/pin[1]*pin[2]
    # lens <- lens/dy*dx
    lns <- if(missing(length)) 0.35*lens else rep(length,3)
    # browser()
        txt <- sprintf("%1.1f m/s",Us)
        text.width <- max(abs(strwidth(txt, units = "user")))
    text.height <- max(abs(strheight(txt, units = "user")))*0.55
    # browser()
    dly0 <- pmax(dy/dx*lns*sin(angle/180*pi)/pin[2]*pin[1],text.height) + 0.005*dy
    dly <- cumsum(c(0,dly0)) + cumsum(c(0,0,dly0[-length(dly0)]))
    inset <- rep_len(inset, 2)
        insetx <- inset[1L] * (usr[2L] - usr[1L])
        w <- max(lens) + text.width
        h <- max(dly)
        left <- switch(ULegendPos, bottomright = , topright = , 
            right = usr[2L] - w - insetx, bottomleft = , 
            left = , topleft = usr[1L] + insetx, bottom = , 
            top = , center = (usr[1L] + usr[2L] - w)/2)
        insety <- inset[2L] * (usr[4L] - usr[3L])
        top <- switch(ULegendPos, bottomright = , bottom = , bottomleft = usr[3L] + 
            h + insety, topleft = , top = , topright = usr[4L] - 
            insety, left = , right = , center = (usr[3L] + 
            usr[4L] + h)/2)
        # browser()
        # draw
        for(i in seq_along(Us)){
          x0 <- left + text.width
          y0 <- top - dly[i + 1]
          # browser()
          arrows(x0,y0,x0 + lens[i],y0,lwd=lwd,length=lns[i]*pin[1]/dx,angle=angle,...)
          text(left,y0,txt[i])
        }
  }
}
