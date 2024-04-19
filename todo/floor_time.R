
floor_time <- function(x, unit = "1secs"){
  unit <- parse_time_diff(unit)
  tx <- trunc(x, "days")
  y <- as.numeric(x - tx, units = "secs")
  tx + floor(y / unit) * unit
}
