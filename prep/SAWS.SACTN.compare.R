#############################################################################
###"prep/SAWS.SACTN.compare.R"
## This script does:
# 1. Load SAWS and SACTN site lists
# 2. Calculate distance and bearing for each site between the datasets
# 3. Calculate days of overlap for each site between the datasets and save
# 4. Estimate best SAWS time series to use with each SACTN time series
# 5. Calculate analysis period for each SACTN time series
# 6. "grow" the SAWS time series for use in "proc/SAWS.RMarineHeatwaves.R"
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(fossil)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(tidyr)
library(purrr)
library(broom)
source("func/earthdist.R")
## USED BY:
# "setupParams/SAWS.SACTN.match.R"
## CREATES:
# "setupParams/distances_bearings.Rdata"
# "setupParams/distances_SACTN.Rdata"
# "setupParams/distances_SAWS.Rdata"
# "setupParams/SACTN_SAWS_nearest.Rdata"
# "setupParams/SACTN_analysis_period.Rdata"
#############################################################################


# 1. Load SAWS and SACTN site lists ---------------------------------

# SAWS
load("setupParams/SAWS_site_list.Rdata")
SAWS_site_list$dataset <- "SAWS"
load("data/SAWS/homogenised/SAWS_homogenised.Rdata")

# SACTN
load("setupParams/SACTN_site_list.Rdata")
SACTN_site_list$dataset <- "SACTN"
load("data/SACTN/SACTN_cropped.Rdata")

# 2. Calculate distance and bearing for each site between the datasets ------

# Combine site lists
site_list <- rbind(SAWS_site_list[,c(1:3,23)], SACTN_site_list[,c(2,5,6,19)])

## Calculate distances and bearings in long format

distances_bearings <- ddply(site_list, .(site), dist.bear.many, df = site_list, .parallel = TRUE)
distances_bearings$site <- NULL
save(distances_bearings, file = "setupParams/distances_bearings.Rdata")

## Calculate distances in wide format

# Convert decimal degrees to radians
site_list$lon <- deg2rad(site_list$lon); site_list$lat <- deg2rad(site_list$lat)

# Calculate distances
distances <- site_list
row.names(distances) <- NULL
row.names(distances) <- distances$site
distances$site <- NULL
distances <- CalcDists(distances)
distances <- melt(as.matrix(distances), varnames = c("row", "col"))

# Remove SACTN comparisons, leaving only SAWS comparisons
distances <- distances[1:407,]

# Cast to wide format
distances <- dcast(distances, row~col)
colnames(distances)[1] <- "SACTN"

# Separate the data frames
distances_SACTN <- distances[12:37,]
save(distances_SACTN, file = "setupParams/distances_SACTN.Rdata")
distances_SAWS <- distances[1:11,]
save(distances_SAWS, file = "setupParams/distances_SAWS.Rdata")

# 3. Calculate days of overlap for each site between the datasets ---------

## This isn't really necessary as the length of the land data far exceeds the sea data

# 4. Estimate best SAWS time series to use with each SACTN time seies -----

nearest.site <- function(x){
  y <- melt(x, id.vars = "SACTN")
  best <- droplevels(y[y$value == min(y$value, na.rm = T),])
  colnames(best) <- c("SACTN", "SAWS", "distance")
  return(best)
}

SACTN_SAWS_nearest <- ddply(distances_SACTN, .(SACTN), nearest.site)

save(SACTN_SAWS_nearest, file = "setupParams/SACTN_SAWS_nearest.Rdata")

# 5. Calculate analysis period for each SACTN time series ----------------

analysis.period <- function(x){
  start <- year(x$date[1])+1
  end <- year(x$date[nrow(x)])-1
  dat <- data.frame(site = x$site[1], start = start, end = end)
  return(dat)
}

SACTN_analysis_period <- ddply(SACTN_cropped, .(site), analysis.period)

save(SACTN_analysis_period, file = "setupParams/SACTN_analysis_period.Rdata")


# 6. "grow" the SAWS time series ------------------------------------------
## This "grown" data frame is used in "proc/SACTN.RMarineHeatwaves.R"
## This is done so that each analysis period for each SACTN time series may be used against the SAWS data
## Must think of a way to do this without loops

## 2016/09/28
## This step is no longer necessary as it is done without loops in "proc/SACTN.RMarineHeatwaves.R"

# SAWS_grown <- data.frame()
# system.time(
# for(i in 1:length(levels(SAWS_homogenised$site))){
#   data1 <- droplevels(subset(SAWS_homogenised, site == levels(SAWS_homogenised$site)[i]))
#   for(j in 1:length(levels(SACTN_analysis_period$site))){
#     data2 <- subset(SACTN_analysis_period, site == levels(SACTN_analysis_period$site)[j])
#     dist <- distances_SACTN[distances_SACTN$SACTN == levels(data2$site)[j],]
#     dist <- round_any(as.numeric(dist[colnames(dist) == levels(data1$site)]), 0.01)
#     data3 <- data1
#     data3$SACTN <- data2$site[1]
#     data3$start <- data2$start[1]
#     data3$end <- data2$end[1]
#     data3$dist <- dist
#     SAWS_grown <- rbind(SAWS_grown, data3)
#   }
# }
# ) ## 818 seconds
# 
# SAWS_grown$index <- paste(SAWS_grown$site, SAWS_grown$SACTN, sep = " - ")
# 
# save(SAWS_grown, file = "data/SAWS_grown.Rdata")

