
shinyCompare <- function(...,d_t=NULL,type=NULL,min.coverage=0.5){

    if(!requireNamespace("shiny", quietly = TRUE)){
		stop("package 'shiny' is missing - please install the package by running install.packages('shiny')")
    }
    if(!requireNamespace("deming", quietly = TRUE)){
		stop("package 'deming' is missing - please install the package by running install.packages('deming')")
    }
    if(!requireNamespace("robustbase", quietly = TRUE)){
		stop("package 'robustbase' is missing - please install the package by running install.packages('robustbase')")
    }
    if(!requireNamespace("MASS", quietly = TRUE)){
        stop("package 'MASS' is missing - please install the package by running install.packages('MASS')")
    }

	cat("\nThis might take a while!!!\n")
	dots <- list(...)
	nms0 <- names(dots)
	data <- dots[[1]]
	if(!is.null(d_t)){
		data <- pool(data,d_t)
	}
	if(length(dots)>1){
		data2 <- dots[[2]]
		# warning("argument 'data2' is merged to 'data' might have differing values from the original input!")
		data <- merge(data[,1],data2[,1])
	} else {
		data2 <- data[,2]
	}
	data2 <- data2[,1]
	dta <- data[,1:2] %>=c% min.coverage
	data <- data[,1]
	nms <- names(dta)
	if(!is.null(nms0)){
		nms[nms0!=""] <- nms0[nms0!=""]
	}	
	dffs <- as.numeric(diff(st(dta)),units="secs")
	d_t <- median(dffs)
	# cols <- c("indianred","lightblue")
	cols <- c("#023FA5","#8E063B")
	runApp(list(
		ui={
			fluidPage(
				fluidRow(
					column(width = 6, class = "well",
						h4("Time series"),
						plotOutput("plot1",
							height = 300,
							brush = brushOpts(
								id = "plot1_brush"
								# ,resetOnNew = TRUE
							)
						)
					),
					column(width = 6, class = "well",
						h4("Time series Zoom"),
						plotOutput("plot2",
							height = 300
						)
					)
				),
				fluidRow(
					column(width = 3, class = "well",
						h4("Diff vs. Avg (Altman & Bland)"),
						plotOutput("plot3"
							,height = 300
							# ,height = 400
							# ,width = 400
						)
					),
					column(width = 3, class = "well",
						h4("relative Diff vs. Avg"),
						plotOutput("plot4"
							,height = 300
							# ,height = 400
							# ,width = 400
						)
					),
					column(width = 3, class = "well",
						h4("Scatterplot"),
						plotOutput("plot5"
							,height = 300
							# ,height = 400
							# ,width = 400
						)
					),
					column(width = 3, class = "well",
						h4("Control")
						,uiOutput("ui_timeShift")
						# ,numericInput("time.shift", 
						# 	label = "Time shift (secs):", 
						# 	value = 0L,
						# 	step = d_t,
						# 	min = -1000*d_t,
						# 	max = 1000*d_t
						# )
						,selectInput("regression.type", 
							label = "Regression type:",
							choices = c("none", "cor", "lm", "rlm", "lmrob", "loess", "deming", "pbreg", "thielsen"),
							selected = "none"
						)
						,numericInput("time.avg", 
							label = "Averaging (secs):", 
							value = d_t,
							step = d_t,
							min = -1000*d_t,
							max = 1000*d_t
						)
						,numericInput("offset.dta", 
							label = "Offset (y):", 
							value = 0
						)
						,numericInput("span.dta", 
							label = "Span (y):", 
							value = 1
						)						
						,checkboxInput("is_light"
							,label = "use exact time values?"
							,value = FALSE
						)
						,checkboxInput("switch_xy"
							,label = "y as x?"
							,value = FALSE
						)
						,checkboxInput("add_loess"
							,label = "add local trend?"
							,value = FALSE
						)
					)					
				)
			)
		},
		server = function(input, output){
			# main plot
			output$plot1 <- renderPlot({
				plot(isolate(ranges2$dta_x),min.coverage=min.coverage,col=cols[1],type=type,ylim=range(isolate(ranges2$dta),na.rm=TRUE),ylab=paste(isolate(ranges2$nms),collapse=" / "))
				lines(ranges2$dta_y,min.coverage=min.coverage,col=cols[2],type=type)
				legend("topleft",legend=paste0(isolate(ranges2$nms),c(" (ref.)","")),lty=1,col=c(cols[1],cols[2]),bty="n")
			})
			# zoom plot
			output$plot2 <- renderPlot({
				plot(isolate(ranges2$dta2_x),min.coverage=min.coverage,col=cols[1],ylim=ranges2$y,xlim=ranges2$xlim,type=ranges2$type2,ylab=paste(isolate(ranges2$nms),collapse=" / "))
				lines(ranges2$dta2_y,min.coverage=min.coverage,col=cols[2],type=ranges2$type2)
			})
			# zoom plot
			ranges2 <- reactiveValues(x = NULL, y = NULL, dta_x = dta[,1], dta_y = dta[,2], dta2_x=dta[,1], dta2_y =dta[,2]
				, xlim = NULL, tshift = 0,type2 = type ,is_light = FALSE, time.avg = d_t, switch_xy = FALSE, tShft = 0
				, nms = nms, data = data, data2 = data2, dta = dta, tshift.trigger = FALSE, offset = 0, span = 1)

			output$ui_timeShift <- renderUI({
				tagList(
					numericInput("time.shift", 
						label = "Time shift (secs):", 
						value = ranges2$tShft,
						step = d_t,
						min = -1000*d_t,
						max = 1000*d_t
					)
				)
			})

			observe({
				if(is.null(input$time.shift)){
					time.shift <- isolate(ranges2$tshift)
				} else {
					time.shift <- input$time.shift
				}
				isolate({
					if(ranges2$tshift!=time.shift){
						ranges2$tshift.trigger <- !ranges2$tshift.trigger
					}
				})
			})

			observe({
				trigOn <- ranges2$tshift.trigger
				if(is.null(isolate(input$time.shift))){
					time.shift <- isolate(ranges2$tshift)
				} else {
					time.shift <- isolate(input$time.shift)
				}
				time.avg <- input$time.avg
				span <- input$span.dta
				offset <- input$offset.dta
				dta_y <- isolate(ranges2$dta_y)
				dta2_y <- isolate(ranges2$dta2_y)
				if(isolate(ranges2$switch_xy)!=input$switch_xy){
					isolate(ranges2$nms <- ranges2$nms[c(2,1)])
					# change original
					dummy1 <- isolate(ranges2$data2)
					isolate(ranges2$data2 <- ranges2$data)
					isolate(ranges2$data <- dummy1)
					# change dta:
					if(!input$is_light){
						isolate(ranges2$dta <- merge(ranges2$data,ranges2$data2)  %>=c% min.coverage)
						isolate(ranges2$dta_x <- ranges2$dta[,1])
						dta_y <- isolate(ranges2$dta[,2])
						# update timeshift and time.avg:
						isolate(ranges2$tshift <- 0)
						isolate(ranges2$time.avg <- d_t)
						isolate(ranges2$is_light <- TRUE)
					} else {
						# change dta_x/dta_y
						dummy1 <- isolate(ranges2$dta_x)
						isolate(ranges2$dta_x <- dta_y)
						dta_y <- dummy1						
					}					
					# change dta2_x/dta2_y
					# further down...
					# update ranges2 and tAvg			
					isolate(ranges2$switch_xy <- input$switch_xy)
					ranges2$tshift <- -ranges2$tshift
					isolate(ranges2$tshift <- -ranges2$tshift)
				}
				brush <- input$plot1_brush
				if(time.shift!=isolate(ranges2$tshift) | input$is_light!=isolate(ranges2$is_light)){
					if(time.shift!=isolate(ranges2$tshift)){
						isolate(ranges2$time.avg <- d_t)
					}
					if(input$is_light){
						dta_y <- isolate(ranges2$data2[,1])
						st(dta_y) <- st(isolate(ranges2$data2)) + time.shift
						et(dta_y) <- et(isolate(ranges2$data2)) + time.shift
						isolate(ranges2$tshift <- time.shift)
						isolate(ranges2$is_light <- input$is_light)
					} else {
						dta_y <- isolate(ranges2$dta[,2])
						st(dta_y) <- st(isolate(ranges2$dta)) + time.shift
						et(dta_y) <- et(isolate(ranges2$dta)) + time.shift
						dta_y <- pool(dta_y,st.to=st(isolate(ranges2$dta)),et.to=et(isolate(ranges2$dta)))
						isolate(ranges2$tshift <- time.shift)
						isolate(ranges2$is_light <- input$is_light)				
						isolate(ranges2$offset <- 0)
						isolate(ranges2$span <- 1)
					}
				}
				if(isolate(ranges2$time.avg)!=time.avg){
					trange_x <- timerange(isolate(ranges2$dta_x))
					trange_y <- timerange(dta_y)
					isolate(ranges2$dta_x <- pool(ranges2$dta_x,granularity=time.avg,st.to=trange_x[1],et.to=trange_x[2]))
					dta_y <- pool(dta_y,granularity=time.avg,st.to=trange_y[1],et.to=trange_y[2])
					isolate(ranges2$time.avg <- time.avg)
					# isolate(ranges2$offset <- 0)
					# isolate(ranges2$span <- 1)
				}
				if (!is.null(brush)) {
					tzn <- isolate(tzone(ranges2$dta))
					isolate(ranges2$xlim <- c(format(as.POSIXct(brush$xmin,origin="1970-01-01 00:00.00 UTC",tz=tzn),tz=tzn)
						,format(as.POSIXct(brush$xmax,origin="1970-01-01 00:00.00 UTC",tz=tzn),tz=tzn)))
					isolate(ranges2$x <- paste(ranges2$xlim,collapse=" to "))
					isolate(ranges2$y <- c(brush$ymin, brush$ymax))
					isolate(ranges2$dta2_x <- ranges2$dta_x[ranges2$x])
					dta2_y <- dta_y[isolate(ranges2$x)]
					isolate(ranges2$type2 <- type)
				} else {
					isolate(ranges2$x <- NULL)
					isolate(ranges2$xlim <- NULL)
					isolate(ranges2$y <- NULL)
					isolate(ranges2$dta2_x <- ranges2$dta_x)
					dta2_y <- dta_y
					isolate(ranges2$type2 <- "n")
				}
				# Offset + Span
				if(isolate(ranges2$offset != offset)){
					isolate(dta_y <- dta_y + offset - ranges2$offset)
					isolate(dta2_y <- dta2_y + offset - ranges2$offset)
					isolate(ranges2$offset <- offset)					
				}
				if(isolate(ranges2$span != span)){
					if(span != 0){
						isolate(dta_y <- dta_y * span / ranges2$span)
						isolate(dta2_y <- dta2_y * span / ranges2$span)					
						isolate(ranges2$span <- span)											
					}
				}
				### call reactive:
				if(!identical(dta_y,isolate(ranges2$dta_y))){
					isolate(ranges2$dta_y <- dta_y)
				}
				if(!identical(dta2_y,isolate(ranges2$dta2_y))){
					isolate(ranges2$dta2_y <- dta2_y)
				}
				isolate(if(ranges2$tshift!=ranges2$tShft)ranges2$tShft <- ranges2$tshift)
			})

			# B & A
			output$plot3 <- renderPlot({
				if(!input$is_light){
					y <- ranges2$dta2_y - isolate(ranges2$dta2_x)
					my <- mean(y[[1]],na.rm=TRUE)
					sdy <- sd(y[[1]],na.rm=TRUE)
					qy <- qt(0.975,sum(!is.na(y[[1]])))
					ylim <- c(-1,1)*max(abs(y),na.rm=TRUE)
					if(input$add_loess){
						plot((isolate(ranges2$dta2_x) + ranges2$dta2_y)/2,y,min.coverage=min.coverage
							,ylab=paste0("Difference: ",isolate(ranges2$nms[2])," - ",isolate(ranges2$nms[1])),xlab=paste0("Average: (",isolate(ranges2$nms[1])," + ",isolate(ranges2$nms[2]),")/2")
							,panel.first={grid();abline(h=0,col="darkgrey");abline(h=my + c(0,-qy,qy)*sdy,col="orange",lty=c(1,3,3))}
							,ylim=ylim,xlim=NULL,stats="loess",stats.plot.CI95=TRUE,stats.Args=list(family="symmetric",degree=1))
					} else {
						plot((isolate(ranges2$dta2_x) + ranges2$dta2_y)/2,y,min.coverage=min.coverage
							,ylab=paste0("Difference: ",isolate(ranges2$nms[2])," - ",isolate(ranges2$nms[1])),xlab=paste0("Average: (",isolate(ranges2$nms[1])," + ",isolate(ranges2$nms[2]),")/2")
							,panel.first={grid();abline(h=0,col="darkgrey");abline(h=my + c(0,-qy,qy)*sdy,col="orange",lty=c(1,3,3))}
							,ylim=ylim,xlim=NULL)						
					}
				}
			})
			# B & A, relative
			output$plot4 <- renderPlot({
				if(!input$is_light){
					x <- (isolate(ranges2$dta2_x) + ranges2$dta2_y)/2
					y <- (ranges2$dta2_y - isolate(ranges2$dta2_x))/x*100
					my <- mean(y[[1]],na.rm=TRUE)
					sdy <- sd(y[[1]],na.rm=TRUE)
					qy <- qt(0.975,sum(!is.na(y[[1]])))
					ylim <- c(-1,1)*max(abs(y),na.rm=TRUE)
					xlim <- range(x,na.rm=TRUE)
					if(xlim[1] <= 0){
						xlim[1] <- min(x[x[[1]]>0],na.rm=TRUE)
					}
					plot(x,y,min.coverage=min.coverage,log="x"
						# ,ylab=paste0("Difference: 100*(",nms[2]," - ",nms[1],")/(",nms[1]," + ",nms[2],")*2"),xlab=paste0("Average: (",nms[1]," + ",nms[2],")/2")
						,ylab=paste0("relative Difference: 100*(",isolate(ranges2$nms[2])," - ",isolate(ranges2$nms[1]),")/Average"),xlab=paste0("Average: (",isolate(ranges2$nms[1])," + ",isolate(ranges2$nms[2]),")/2")
						,panel.first={grid(equilogs=FALSE);abline(h=0,col="darkgrey");abline(h=my + c(0,-qy,qy)*sdy,col="orange",lty=c(1,3,3))}
						,ylim=ylim,xlim=xlim)
					if(input$add_loess){
						logx <- log(x[[1]])
						y <- y[[1]]
						mod <- loess(y~logx,family="symmetric",degree=1)
						xseq <- seq(min(logx,na.rm=TRUE),max(logx,na.rm=TRUE),length.out=100)
						pmod <- predict(mod,newdata=data.frame(logx=xseq),se=TRUE)
						xseq <- exp(xseq)
						lines(xseq,pmod$fit,col="blue",lwd=2)
						lines(xseq,pmod$fit + pmod$se.fit*qnorm(0.975),col="blue",lty=3)
						lines(xseq,pmod$fit + pmod$se.fit*qnorm(0.025),col="blue",lty=3)
					}
				}
			})
			# scatter
			output$plot5 <- renderPlot({
				if(!input$is_light){
					if(input$regression.type!="none"){
						plot(isolate(ranges2$dta2_x),ranges2$dta2_y,min.coverage=min.coverage,stats=input$regression.type,eq.xlab=isolate(ranges2$nms[1]),eq.ylab=isolate(ranges2$nms[2])
							,panel.first={grid();abline(0,1,col="darkgrey")},xlim=ranges2$y,ylim=ranges2$y,xlab=isolate(ranges2$nms[1]),ylab=isolate(ranges2$nms[2]))
					} else {
						plot(isolate(ranges2$dta2_x),ranges2$dta2_y,min.coverage=min.coverage,eq.xlab=isolate(ranges2$nms[1]),eq.ylab=isolate(ranges2$nms[2])
							,panel.first={grid();abline(0,1,col="darkgrey")},xlim=ranges2$y,ylim=ranges2$y,xlab=isolate(ranges2$nms[1]),ylab=isolate(ranges2$nms[2]))					
					}
				}
			})
		}
		))
}


