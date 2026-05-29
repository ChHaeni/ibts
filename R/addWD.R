addWD <- function (WD, frac = 25, y0 = NULL, lwd = 3, length = NULL, angle = 30,
  WD_col = c('wd', 'wdir'), U_col = c('ws', 'u'), addUlegend = FALSE, 
  ULegendPos = "right", inset = c(0.04, 0), pool.args = list(),
  breaks = function (x) median(x, na.rm = TRUE) * c(0.5, 1, 2), 
  col = 'black', indicate = c('none', 'dot', 'rug'), icol = col, ipch = 20, 
  icex = 1, ilwd = 0.5, isize = 0.03, iside = c(3, 1)[1],
  ...){

  usr <- par("usr")
  pin <- par("pin")
  dx <- diff(usr[1:2])
  dy <- diff(usr[3:4])
  indicate <- match.arg(indicate)

  # check column names (WD)
  col_nms <- names(WD)
  if (is.numeric(WD_col)) {
      wd_col <- WD_col
  } else if (missing(WD_col)) {
      wd_col <- which(tolower(col_nms) %in% WD_col)
  } else {
      wd_col <- which(col_nms %in% WD_col)
  }
  if (length(wd_col) == 0 && ncol(WD) == 1) {
      wd_col <- 1
  }
  if (length(wd_col) > 1) {
    stop('WD column not found! Please specify a valid column through argument "WD_col"')
  }
  # pool WD?
  if (length(pool.args) > 0) {
      WD <- do.call(pool, c(list(dat = WD), pool.args))
  }
  # check column names (U)
  U <- NULL
  if (is.ibts(U_col)) {
      U <- U_col
      # pool U?
      if (length(pool.args) > 0) {
          U <- do.call(pool, c(list(dat = U), pool.args))
      }
  } else if (ncol(WD) > 1) {
      if (is.numeric(U_col)) {
          u_col <- U_col
      } else if (missing(U_col)) {
          u_col <- which(tolower(col_nms) %in% U_col)
      } else {
          u_col <- which(col_nms %in% U_col)
      }
      if (length(u_col) > 1) {
        stop('U column not unique! Please specify a valid column through argument "U_col"')
      } else if (length(u_col) == 1) {
          U <- WD[, u_col]
      } else if (ncol(WD) == 2) {
          U <- WD[, -wd_col]
      }
  }
  # fix WD
  WD <- WD[, wd_col]

  # scale dy by U
  if (is.null(U)) {
    ddy <- dy / frac
  } else {
    ddy <- dy / frac * U[[1]]
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

  # col as a function
  if (is.function(col)) {
      col <- col(WD[[1]], U[[1]])
  } else {
      col <- rep(col, length(WD[[1]]))[seq_along(WD[[1]])]
  }
  if (indicate != 'none') {
      if (is.function(icol)) {
          icol <- icol(WD[[1]], U[[1]])
      } else {
          icol <- rep(icol, length(WD[[1]]))[seq_along(WD[[1]])]
      }
  }

  for(i in seq_along(WD[[1]])){
    wd1 <- WD[i]
    if(!is.na(wd1[[1]])){
      mx <- mt(wd1)
      my <- (y0+y1[i])/2

      rad1 <- as.numeric((-wd1 - 180) %% 360)/180*pi

      vnx <- -sin(rad1)*ddx[i]
      vny <- cos(rad1)*ddy[i]
      arrows(mx - vnx / 2, my - vny / 2, mx + vnx / 2, my + vny / 2, 
          length = if (missing(length)) 0.35 * ddy[i] * pin[2] / dy 
            else length,
          lwd = lwd, angle = angle, col = col[i], ...)
      switch(indicate
          , dot = points(mx, my, pch = ipch, col = icol[i], cex = icex)
          , rug = rug(mx, ticksize = isize, side = iside, lwd = ilwd, 
              col = icol[i])
      )
    }
  }
  # switch(indicate
  #     , dot = points(mt(WD), (y0 + y1) / 2, pch = ipch, col = icol, cex = icex)
  #     , rug = rug(mt(WD), ticksize = isize, side = iside, lwd = ilwd, 
  #         col = icol)
  # )

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
        txt <- sprintf("%1.1f m/s",Us)
        text.width <- max(abs(strwidth(txt, units = "user")))
    text.height <- max(abs(strheight(txt, units = "user")))*0.55
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
        # draw
        for(i in seq_along(Us)){
          x0 <- left + text.width
          y0 <- top - dly[i + 1]
          arrows(x0,y0,x0 + lens[i],y0,lwd=lwd,length=lns[i]*pin[1]/dx,angle=angle,...)
          text(left,y0,txt[i])
        }
  }
}
