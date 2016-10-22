#############################################################################
###"func/expand.gaps.R"
## This script does:
# 1. Orders a time series based on its date column;
# 2. Creates a complete length of dates based on the given time series;
# 3. Merges the time series with the complete length of dates to create NA gaps for missing data
## DEPENDS ON:
# (x) must have a "date", "site" and "src" column
## USED BY:
#"func/load_v4.0.R"
## CREATES:
#nothing
#############################################################################

## Expand gaps to match coast dataframe column structure
# expand.gaps.coast <- function(df, unit = "day"){
#   y <- df[order(df$date),]
#   y <- na.trim(y)
#   grid.df <- data.frame(site = y$site[1], src = y$src[1], index = y$index[1], 
#                         date = seq(min(y$date, na.rm = T), max(y$date, na.rm = T), by = unit), temp = NA)
#   y <- merge(y, grid.df, by = colnames(y), all.y = T)
#   return(y)
# }

## Expand gaps to match air dataframe column structure
# expand.gaps.air <- function(df, unit = "day"){
#   y <- df[order(df$date),]
#   y <- na.trim(y)
#   grid.df <- data.frame(site = y$site[1], date = seq(min(y$date, na.rm = T), max(y$date, na.rm = T), by = unit), 
#                         tmax = NA, tmin = NA, temp = NA)
#   y <- merge(y, grid.df, by = colnames(y), all.y = T)
#   return(y)
# }

# Fill gaps in data
fast.gaps.air <- function(df, prec = 0.1){
  y <- df[order(df$date),]
  y <- na.trim(y)
  colnames(y)[2] <- "t"
  colnames(y)[3:5] <- "temp"
  tmax <- make_whole(y[,c(2,3)])
  tmin <- make_whole(y[,c(2,4)])
  temp <- make_whole(y[,c(2,5)])
  df2 <- data.frame(site = y$site[1], date = temp$date,
                    tmax = round_any(tmax$temp, prec),
                    tmin = round_any(tmin$temp, prec),
                    temp = round_any(temp$temp, prec))
  return(df2)
}
