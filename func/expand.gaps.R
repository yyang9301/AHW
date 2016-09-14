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

expand.gaps <- function(x){
  y <- x[order(x$date),]
  y <- unique.data.frame(y)
  grid.df <- data.frame(date = seq(min(x$date, na.rm = T), max(x$date, na.rm = T), by = "day"))
  y <- merge(x, grid.df, by = "date", all.y = T)
  # Fill gaps in site and source labels
  y$site <- y$site[1]
  y$src <- y$src[1]
  return(y)
}
