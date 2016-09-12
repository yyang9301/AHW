###############################################################################
### "func/metaTemp.R"
## DESCRIPTION: This function calculates meta-data and statisitics for time series fed into it.
## USAGE: You must be in the project root directory to use this function. 
## ARGUMENTS: The (data) must have columns labeled "site" and "src"
## DETAILS:
## VALUE:
## AUTHORS(S): AJ Smit
## REFERENCE(S):
## EXAMPLE(S):
## DEPENDS: 
library(lubridate)
library(zoo)
## USED BY: "trend_analysis.R", "text.R"
##############################################################################

metaTemp <- function(x) {
  a <- as.data.frame(start(na.trim(x))) # Start date of time series
  b <- as.data.frame(end(na.trim(x))) # End date of time series
  c <- length(na.trim(x)) # Length in days of time series
  d <- length(na.omit(na.trim(x))) # Number days with temps
  e <- length(na.trim(x)) - length(na.omit(na.trim(x))) # Number of NaNs
  f <- 100 - ((length(na.omit(na.trim(x))) / length(na.trim(x))) * 100) # % NaNs in time series
  g <- mean(x, na.rm = TRUE)
  h <- sd(x, na.rm = TRUE)
  i <- min(x, na.rm = TRUE)
  j <- max(x, na.rm = TRUE)
  all <- cbind(a, b, c, d, e, round(f, 1), round(g, 3), round(h, 3), round(i, 3), round(j, 3)) # Put them into one data frame
  names(all) <- c("date.start", "date.end", "ts.length", "temp.days", "NaN.days", "NaN.perc",
                  "mean", "sd", "min", "max")
  return(all)
}
