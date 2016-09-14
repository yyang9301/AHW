###############################################################################
## DESCRIPTION: This function produces basic meta-data for a time series
## USAGE: You must feed this function a single time series 
## ARGUMENTS: The (x) must have "date" and "temp" columns
## DETAILS:
## VALUE:
## AUTHORS(S):
## REFERENCE(S):
## EXAMPLE(S)
##############################################################################

library(plyr)
library(dplyr)
library(lubridate)
library(zoo)

meta.data <- function(x) { # x must be a 
  a <- min(x$date, na.rm = T) # Start date of time series
  b <- max(x$date, na.rm = T) # End date of time series
  c <- length(x$date) # Length in days of time series
  # d <- length(na.omit(na.trim(x))) # Number days with temps
  # e <- length(na.trim(x)) - length(na.omit(na.trim(x))) # Number of NaNs
  f <- round_any((100 - ((length(na.omit(x$temp)) / length(x$temp)) * 100)),1) # %NA
  g <- round_any(mean(x$temp, na.rm = TRUE), 0.001) # Mean
  h <- round_any(sd(x$temp, na.rm = TRUE), 0.001) # SD
  i <- min(x$temp, na.rm = TRUE) # Min
  j <- max(x$temp, na.rm = TRUE) # Max
  k <- j-i # Range
  all <- data.frame(a, b, c, f, g, h, k, i, j) # Put them into one data frame
  names(all) <- c("date.start", "date.end", "length", "NA.perc",
                  "mean", "sd", "range", "min", "max")
  return(all)
}