pindex <- function(x,times,periodicity="1days"){

	if(!inherits(x, "ibts"))stop("object has to be of class 'ibts'")
	if(nrow(x) < 1)stop("'ibts' object is empty")
	periodicity <- parse_time_diff(periodicity)
	strt <- start(x)
	nd <- end(x)
	tot <- as.numeric(nd - strt,units="secs")
	tms <- unlist(strsplit(times,"/"))

	switch(length(tms),
		"1" = {
			dtime <- parse_time_diff(tms)
			st.p <- trunc(strt,"days") + seq(0,tot - dtime,periodicity)
			et.p <- st.p + periodicity
			ind_in_p <- seq(0,periodicity - dtime,dtime)
			ind_in_t <- seq(0,tot - dtime,periodicity)
			st.t <- strt + rep(ind_in_p,length(ind_in_t)) + rep(ind_in_t,each=length(ind_in_p))
			et.t <- st.t + dtime
		},
		"2" = {
			st_first <- parse_date_time3(paste0(format(strt,format="%Y%m%d "),tms[1]),tz=tzone(x))
			et_first <- parse_date_time3(paste0(format(strt,format="%Y%m%d "),tms[2]),tz=tzone(x))
			if(st_first>et_first){
				et_first <- et_first + as.difftime(1,units="days")
			}
			if(strt >= et_first){
				st_first <- st_first + as.difftime(1,units="days")
				et_first <- et_first + as.difftime(1,units="days")
			}
			st_last <- parse_date_time3(paste0(format(nd,format="%Y%m%d "),tms[1]),tz=tzone(x))
			et_last <- parse_date_time3(paste0(format(nd,format="%Y%m%d "),tms[2]),tz=tzone(x))
			if(nd <= st_last){
				st_last <- st_last - as.difftime(1,units="days")
				et_last <- et_last - as.difftime(1,units="days")
			}
			st.p <- seq(trunc(st_first,"days"),trunc(st_last,"days"),periodicity)
			et.p <- st.p + periodicity
			st.t <- seq(st_first,st_last,periodicity)
			et.t <- seq(et_first,et_last,periodicity)
			ind_in_p <- NULL
		},
		"3" = {
			dtime <- parse_time_diff(tms[3])
			st_first <- parse_date_time3(paste0(format(strt,format="%Y%m%d "),tms[1]),tz=tzone(x))
			et_first <- parse_date_time3(paste0(format(strt,format="%Y%m%d "),tms[2]),tz=tzone(x))
			if(st_first>et_first){
				et_first <- et_first + as.difftime(1,units="days")
			}
			if(dtime > as.numeric(et_first - st_first,units="secs"))stop("increment is bigger than limits")
			if(strt >= et_first){
				st_first <- st_first + as.difftime(1,units="days")
				et_first <- et_first + as.difftime(1,units="days")
			}
			st_last <- parse_date_time3(paste0(format(nd,format="%Y%m%d "),tms[1]),tz=tzone(x))
			et_last <- parse_date_time3(paste0(format(nd,format="%Y%m%d "),tms[2]),tz=tzone(x))
			if(nd <= st_last){
				st_last <- st_last - as.difftime(1,units="days")
				et_last <- et_last - as.difftime(1,units="days")
			}
			ind_in_p <- seq(0,as.numeric(et_first - st_first,units="secs") - dtime,dtime)
			st.p <- seq(trunc(st_first,"days"),trunc(st_last,"days"),periodicity)
			et.p <- st.p + periodicity
			st.t0 <- seq(st_first,st_last,periodicity)
			st.t <- rep(st.t0,each=length(ind_in_p)) + rep(ind_in_p,length(st.t0))
			et.t <- st.t + dtime
		}
		)

	ind.t <- cutIntervals(as.numeric(st(x)),as.numeric(et(x)),as.numeric(st.t),as.numeric(et.t))
	# ind.t <- lapply(ind.t,function(x)x[x[,2]>1E-12,])
	n_p <- if(!is.null(ind_in_p)) length(ind_in_p) else 1
	ind_out <- vector("list",length(n_p))
	for(i in seq.int(n_p)){
		ind_i <- ind.t[seq(i,length(ind.t),n_p)]
		mat_out <- do.call(rbind,ind_i)
		if(is.null(mat_out)){
			ind_out[[i]] <- cbind(NA,NA)
		} else {
			ind_out[[i]] <- mat_out
			ind_out[[i]][,2] <- ind_out[[i]][,2]/length(ind_i)
		}
	}
	ind_out <- lapply(ind_out,"colnames<-",c("index","wts"))
	st_out <- st.t[seq.int(n_p)]
	et_out <- et.t[seq.int(n_p)]
	names(ind_out) <- paste0(format(st_out,"%H:%M - "),format(et_out,"%H:%M"))
	structure(ind_out,p.st = st_out,p.et = et_out)
}
