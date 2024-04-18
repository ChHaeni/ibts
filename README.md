# ibts - an R package to process measurement data that base on time intervals
The R package ibts provides my collection of R classes and functions to help screening and processing 'interval-based time series' as obtained from field observations of environmental parameters. Interval-based means that the data are representing values (averages, maxima, minima, etc.) over a given time interval with corresponding interval start and end times.
The package is intended to give easy access to and facilitate manipulation of data that has been recorded on different time intervals and to aggregate such data to coarser time resolutions. It is **not** intended to solve any time series related statistical problems **nor** to be fast, complete or nicely programmed.  
The ibts package introduces a new class `ibts` that inherits from class `data.frame`. This new class adds additional attributes related to interval-based data such as start of intervals, end of intervals, time zone, temporal coverage of intervals and secondary classes of column entries.

## Installation
```r
devtools::install_github('ChHaeni/ibts')
```

## Usage examples
### Set up example data.frames
```r
# load ibts
library(ibts)

#### create example data.frames
set.seed(123)
dummy <- abs(rnorm(ind <- seq_len(len <- 100), 15, 2) + sin(ind / len * 2 * pi) ^ 2 * 20 * rnorm(ind, 1, 1))
dummy2 <- abs(rnorm(ind <- seq_len(len <- 100), 2, 0.2) + sin(ind / len * 2 * pi) ^ 2 * 2 * rnorm(ind, 1, 1))
df1 <- data.frame(
	WD = as.integer((runif(ind, 0, 100) + sin(ind / len * 4 * pi) * 360) %% 360),
	U = dummy2,
	NH3 = dummy,
	NH3_SE = pmax(dummy * abs(rnorm(ind, 0.01, 0.01)), 0.2 * abs(rnorm(ind, 1, 0.1))),
	Rain = pmin(floor(dummy / sample(dummy)), 7),
	Temp = round(runif(ind, 0, 10) + sin(ind / len * pi) * 20, 2),
	Factor = as.factor(letters[sample.int(26, len, replace = TRUE)]),
	Char = letters[sample.int(26, len, replace = TRUE)],
	stringsAsFactors = FALSE
	)
df2 <- data.frame(
	WD = as.integer((runif(ind, 0, 100) + sin(ind / len * 4 * pi) * 360) %% 360),
	U = dummy2,
	NH3 = dummy,
	NH3_SE = pmax(dummy * abs(rnorm(ind, 0.01, 0.01)), 0.2 * abs(rnorm(ind, 1, 0.1))),
	Rain = pmin(floor(dummy / sample(dummy)), 7),
	Temp = round(runif(ind, 0, 10) + sin(ind / len * pi) * 20, 2),
	Factor = as.factor(letters[sample.int(26, len, replace = TRUE)]),
	Char = letters[sample.int(26, len, replace = TRUE)],
	st = parse_date_time("2016-08-16 21:27:32", "YmdHMS") + (ind-1) * 60,
	et = parse_date_time("2016-08-16 21:27:32", "YmdHMS") + ind * 60,
	stringsAsFactors = FALSE
	)

#### set a few missing values:
df1[as.matrix(expand.grid(seq_len(nrow(df1)), seq_len(ncol(df1)), KEEP.OUT.ATTRS = FALSE)[sample.int(prod(dim(df1)), 50), ])] <- NA
df2[as.matrix(expand.grid(seq_len(nrow(df2)), seq_len(ncol(df2)-2), KEEP.OUT.ATTRS = FALSE)[sample.int(prod(dim(df2[, -(1:2)])), 15), ])] <- NA
```

### Convert to ibts object
```r
#### Create ibts objects from data.frame:
d1 <- as.ibts(df1, st = parse_date_time("2016-08-16 21:30", "YmdHM") + (ind-1) * 60 * 30, granularity = "30mins")
d1
d2 <- as.ibts(df2)
d2

#### set proper colClasses:
colClasses(d2)[c(1, 3:5)] <- colClasses(d1)[c(1, 3:5)] <- c(
	"circ" 		# circular data, i.e. WD. Note that wind speed is 
                # not taken into account for pooling and true vector
                # averaging needs to be done by hand. See example
                # in the 'Aggregating' section.
	, "avg_se" 	# SE of avg data
	, "sum"		# (cum)sum data
	, "num"		# point meas./moment/numeric
	)
d1
d2
```

### Access some attributes
```r
st(d1)
et(d1)
colClasses(d1)
head(coverage(d1))
tzone(d1)
```

### Aggregating to coarser intervals
```r
# args(pool)
D1a <- pool(d1, "12hours")
D1a
coverage(D1a)

pool(d1, "12hours", st.to = "2016-08-16 18:00")
pool(d1, "12hours", st.to = "2016-08-16 18:00", et.to = "2016-08-18 18:00")
pool(d1, st.to = "2016-08-16 18:00", et.to = "2016-08-18 18:00")

# using user defined functions
# getOption('pooling.functions')
wdavg <- function(x, wts, na.rm, dat){
    x <- data.frame(theta = x / 180 * pi, u = dat$U)
    if (na.rm) {
        x <- na.exclude(x)
        if (!is.null(na.action(x))){
            wts <- wts[-na.action(x)]
        }
    }
    if (length(x) == 0) {
        NA
    }
    else {
        X <- x$u * cos(x$theta)
        Y <- x$u * sin(x$theta)
        Xavg <- sum(X * wts)/sum(wts)
        Yavg <- sum(Y * wts)/sum(wts)
        (atan2(Yavg, Xavg) / pi * 180) %% 360
    }
}
pool(d1, "12hours", FUN = list(
            other = function(x){x[1]},
            WD = wdavg
            ))

# aggregate d2 onto d1 intervals
D2 <- pool(d2, d1)
```

### Merging and susetting
```r
# merging and subsetting:
D3 <- merge(d1,d2)
D4 <- d2[d1]
D4b <- d2[d1[,1],2]
D4c <- d2[d1[1:3,1],2]

# subsetting by time format
#getOption("time.orders")
#getOption("time.separators")
d1["2016-08-16 - 2016-08-17"]
d1["2016-08-16/2016-08-17"]
d1["2016-08-16/2016-08-17"]
d1["2016-08-16::2016-08-17"]
d1["2016-08-17/"]
d1["/2016-08-17"]

# same day
d1["16.08.2016 22:00 to 23:00"]

# exclude time range
d1[-"2016-08-16 - 2016-08-17"]

# edge of interval -> attribute "closed" matters:
d1["2016-08-16 22:28:32"]
```

### Plotting
```r
# time series
par(mfrow = c(2, 2), mar = c(5,4,4,4))
plot(d1, main = 'a)')
plot(d1[,"NH3"], main = 'b)')
plot(d1,"NH3", main = 'c)')
plot(d1,"NH3","NH3_SE", main = 'd)')

# scatter plot
require(deming)
par(mfrow = c(2, 2))
plot(d1[,"U"],d1[,"NH3"], main = 'a)')
plot(NH3 ~ U,d1, main = 'a)')
x1 <- plot(NH3 ~ U,d1,stats="cor",stats.Args=list(use="pairwise.complete.obs"))
x2 <- plot(NH3 ~ U,d1,stats=deming::deming)
```
