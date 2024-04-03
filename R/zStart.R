.onLoad <- function(libname, pkgname) {
	options(time.orders = c("%Y", "%d.%m.%Y", "%d.%m.%y", 
            "%d.%m.%Y %H:%M", "%d.%m.%y %H:%M", "%d.%m.%Y %H:%M:%S", "%d.%m.%y %H:%M:%S", 
            "%Y-%m-%d", "%y-%m-%d", 
            "%Y-%m-%d %H:%M", "%y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S", "%y-%m-%d %H:%M:%S"
            ))
	options(time.separators=c(" - ","::","/"," to "))
	options(pooling.functions=list(
		"avg" = function(x,wts,na.rm,dat){
			if(na.rm){
				x <- na.exclude(x)
				if(!is.null(na.action(x)))wts <- wts[-na.action(x)]
			}
			if(length(x)==0){
				NA
			} else {
				sum(x*wts)/sum(wts)				
			}
		}
		,"avg_se" = function(x,wts,na.rm,dat){
			if(na.rm){
				x <- na.exclude(x)
				if(!is.null(na.action(x)))wts <- wts[-na.action(x)]
			}
			if(length(x)==0){
				NA
			} else {
				sqrt(sum(x^2*wts^2))/sum(wts)
			}
		}
		,"sum" = function(x,wts,na.rm,dat,dt_in,dt_out){
			if(na.rm){
				x <- na.exclude(x)
				if(!is.null(na.action(x))){
					dt_in <- dt_in[-na.action(x)]
					wts <- wts[-na.action(x)]
				}
			}
			if(length(x)==0){
				NA
			} else {
				# wts are equally distributed to dt_out
				sum(x*wts/dt_in)*dt_out
			}
		}
		,"sum_se" = function(x,wts,na.rm,dat,dt_in,dt_out){
			cat("fix sum_se pooling.function someday!\n")
			if(na.rm){
				x <- na.exclude(x)
				if(!is.null(na.action(x))){
					dt_in <- dt_in[-na.action(x)]
					wts <- wts[-na.action(x)]
				}
			}
			if(length(x)==0){
				NA
			} else {
				sqrt(sum(x^2*wts^2/dt_in^2)*dt_out^2)/length(x)
			}
		}
		,"min" = function(x,wts,na.rm,dat){
			if(na.rm){
				x <- na.exclude(x)
			}
			if(length(x)==0){
				NA
			} else {
				min(x) 
			}
		}
		,"max" = function(x,wts,na.rm,dat){
			if(na.rm){
				x <- na.exclude(x)
			}
			if(length(x)==0){
				NA
			} else {
				max(x) 
			}
		}
		,"num" = function(x,wts,na.rm,dat){
			if(na.rm){
				x <- na.exclude(x)
			}
			if(length(x)==0){
				NA
			} else {
				mean(x) 
			}
		}
		,"circ" = function(x,wts,na.rm,dat){
			if(na.rm){
				x <- na.exclude(x)
				if(!is.null(na.action(x)))wts <- wts[-na.action(x)]
			}
			if(length(x)==0){
				x <- wts <- NA
			} else {
				(atan2(sum(sin(x/180*pi)*wts),sum(cos(x/180*pi)*wts))*180/pi) %% 360
			}
		}
		,"circ_se" = function(x,wts,na.rm,dat){
			if(na.rm){
				x <- na.exclude(x)
				if(!is.null(na.action(x)))wts <- wts[-na.action(x)]
			}
			if(length(x)==0){
				NA
			} else {
				sqrt(sum(x^2*wts^2))/sum(wts)
			}
		}
		,"other" = function(x,wts,na.rm,dat){
			if(na.rm){
				x <- na.exclude(x)
			}
			if(length(x)==0){
				NA_character_
			} else {
				paste(unique(x),collapse=";") 
			}
		}
		))

}
