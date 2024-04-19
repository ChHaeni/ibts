getIntervals <-
function(time,st,et,closed="st"){
	switch(pmatch(closed[1],c("st","et"),nomatch=3),
		"st"={
			findI_st(as.numeric(time),as.numeric(st),as.numeric(et))
		}
		,"et"={
			findI_et(as.numeric(time),as.numeric(st),as.numeric(et))
		}
		,stop("argument closed must match either 'st or 'et!")
		)
}
