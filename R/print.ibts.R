print.ibts <-
function(x,digits=2,Nrows=20,nMargs=6,...){
	if(ncol(x)==0){
		cat("data frame of class 'ibts' with 0 columns and",nrow(x),"rows\n")
	} else {
		st_x <- attr(x,"st")
		et_x <- attr(x,"et")
		colClasses_x <- attr(x,"colClasses")
		coverage <- attr(x, "coverage")
		x <- as.data.frame(x,check.names=FALSE,stringsAsFactors=FALSE,keepAtts=FALSE, time_columns = FALSE)
		
		if(Cut <- nrow(x)>Nrows & nrow(x) > (nMargs*2 + 1)){
			t1 <- as.numeric(sum(et_x - st_x),units="secs")
			# add average coverage
			cvrg <- colSums(coverage)/nrow(coverage)*100
			cvrg_str <- ifelse(cvrg < 100 & cvrg >= 99.95,sprintf("(<%1.1f%%)",cvrg),ifelse(cvrg > 0 & cvrg <= 0.05,sprintf("(>%1.1f%%)",cvrg),sprintf("(%1.1f%%)",cvrg)))
			# add uptime
			if(t1 < 120){
				add <- paste(round(t1,2),"seconds")
			} else if(floor(t1 / 60) < 120){
				add <- paste(round(t1/60,2),"minutes")
			} else if(floor(t1 / 3600) < 48){
				add <- paste(round(t1/3600,2),"hours")
			} else {
				add <- paste(round(t1/(3600*24),2),"days")
			}
			# Cut:
			ind <- c(seq.int(nMargs),seq.int(to = nrow(x), length.out = nMargs))
			x <- x[ind,,drop=FALSE]
			st_x <- st_x[ind]
			et_x <- et_x[ind]
			coverage <- coverage[ind,,drop=FALSE]
		}

		a <- as.numeric(st_x - trunc(st_x[1],"day"),"secs")
		b <- as.numeric(et_x - trunc(et_x[1],"day"),"secs")
		if(any(a%%60>1E-3) || any(b%%60>1E-3)){
			if(any(a%%1 >= 1e-3) || any(b %% 1 >= 1e-3)){
				fmt1 <- "%Y-%m-%d %H:%M:%OS3"
				fmt2 <- "%H:%M:%OS3"
			} else {
				fmt1 <- "%Y-%m-%d %H:%M:%S"
				fmt2 <- "%H:%M:%S"				
			}
			fmt2 <- paste0("- ",if(any(as.numeric(et_x-st_x,units="days")>=1)) "%Y-%m-%d " else "",fmt2)
		} else if(any(b%%(24*3600)>1E-7) || any(b%%(24*3600)>1E-7)){
			fmt1 <- "%Y-%m-%d %H:%M"
			fmt2 <- "%H:%M"
			fmt2 <- paste0("- ",if(any(as.numeric(et_x-st_x,units="days")>=1)) "%Y-%m-%d " else "",fmt2)
		} else {
			fmt1 <- "%Y-%m-%d"
			fmt2 <- "to %Y-%m-%d"
		}
		isfactor <- sapply(x,is.factor)
		row.names(x) <- sprintf("%s %s",format(st_x,fmt1,usetz=FALSE),format(et_x,fmt2,usetz=TRUE))
		names(x) <- paste(names(x),paste0("[",colClasses_x,"]"))
		if(is.null(digits))digits <- getOption("digits")
		whichNA <- apply(coverage,2,function(x)any(x<0.999))
		for(i in seq_along(x)){
			na.ind <- which(is.na(x[,i]))
			if(length(na.ind)){
				x2 <- x[-na.ind,i]
			} else {
				x2 <- x[,i]
			}
			if(length(x2)){
				if(is.integer(x2)){
					x2 <- as.character(x2)
				} else if(is.numeric(x2)){
					if(any(x2!=0)&&min(abs(x2[x2!=0]))>=1E-2){
						x2temp <- sprintf(paste0("%1.",digits,"f"),x2)
						md <- max(nchar(gsub("0+$","",gsub("^[-]?[0-9]*[.]([0-9]{2})$","\\1",x2temp))))
						if(md<2){
							x2temp <- sprintf(paste0("%1.",md,"f"),x2)
						}
					} else {
						x2temp <- sprintf(paste0("%1.",digits,"e"),x2)
						# md <- max(nchar(gsub("0+$","",gsub("^[-]?[0-9]*[.]([0-9]{2})e[0-9]*$","\\1",x2temp))))
					}
					x2 <- x2temp
				} else if(is.factor(x2)){
					x2 <- as.character(x2)
				} else if(is.POSIXt(x2) || is.Date(x2)){
					x2 <- format(x2)
                }
				# x2[is.na(x[,i])] <- "NA"
				# x2[is.nan(x[,i])] <- "NaN"
								
				if(whichNA[i]){
					ind <- seq_along(x[,i])
					if(length(na.ind))ind <- ind[-na.ind]
					covR <- coverage[ind,i]
					x2[covR<0.999]  <- paste0("{",sprintf("%1.2f",covR[covR<0.999]),"}/",x2[covR<0.999])
				}
				if(length(na.ind)){
					x[,i] <- as.character(x[,i])
					x[-na.ind,i] <- x2			
				} else {
					x[,i] <- x2			
				}
			} else if(length(na.ind)){
				x[,i] <- as.character(x[,i])				
			}
		}

		x[,!isfactor][is.na(x[,!isfactor])] <- "NA"
		if(Cut){
			x <- rbind(x, rep("----", length(x)), cvrg_str)
			row.names(x)[nrow(x) + c(-1,0)] <- c("   ",paste0("uptime: ",add))
			hx <- x[seq.int(nMargs),,drop=FALSE]
			# hx[is.na(hx)] <- "NA"
			tx <- x[seq.int(to = nrow(x), length.out = nMargs + 2),,drop=FALSE]
			# tx[is.na(tx)] <- "NA"
			px <- rbind(hx,rep("***",ncol(x)),tx)
			row.names(px) <- c(row.names(x)[1:nMargs],"",row.names(x)[seq.int(to = nrow(x), length.out = nMargs + 2)])
			print.data.frame(px,quote=FALSE,...)
		} else {
			print.data.frame(x,...)
		}
	}
}
