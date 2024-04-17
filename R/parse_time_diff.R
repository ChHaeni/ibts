parse_time_diff <-
function(x){
    if (length(x) == 1) {
        if(!is.numeric(x)){
            if(is.difftime(x)){
                y <- as.numeric(x,"secs")
            } else {
                if(sub("^([-]*)([0-9]*[.]?[0-9]*)[ ]*([a-z]*)(.*)","\\4",x) != ""){
                    y <- parse_time_diff(sub("^([-]*)([0-9]*[.]?[0-9]*)[ ]*([a-z]*)(.*)","\\4",x))
                    x <- sub("^([-]*[0-9]*[.]?[0-9]*[ ]*[a-z]*)(.*)","\\1",x)
                } else {
                    y <- 0
                }

                pm <- sub("^([-]*)[ ]*([0-9]*[.]?[0-9]*)[ ]*([a-z]*)","\\1",x)
                num <- sub("^([-]*)[ ]*([0-9]*[.]?[0-9]*)[ ]*([a-z]*)","\\2",x)
                unt <- sub("^([-]*)([0-9]*[.]?[0-9]*)[ ]*([a-z]*)","\\3",x)
                if(num!=""){
                    num <- as.numeric(num)
                } else {
                    num <- 0
                }
                if(unt=="")unt <- "secs"
                y <- y + as.numeric(as.difftime(num,units=unt),"secs")
                if(pm!=""){
                    y <- -y
                }
            }
            return(y)	
        } else {
            return(x)
        }
    }
    # length > 1
    out <- numeric(length(x))
    for (i in seq_along(x)) {
        out[i] <- parse_time_diff(x[i])
    }
    out
}
