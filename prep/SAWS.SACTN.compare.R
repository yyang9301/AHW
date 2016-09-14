#############################################################################
###"prep/SAWS.SACTN.compare.R"
## This script does:
# 1. Load SAWS and SACTN site lists
# 2. Calculate distance for each site between the datasets
# 3. Calculate days of overlap for each site between the datasets and save
# 4. Estimate best SAWS time series to use with each SACTN time series
# 5. Save as...
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
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
# "setupParams/distances_SACTN.Rdata"
# "setupParams/distances_SAWS.Rdata"
# "setupParams/SACTN_SAWS_nearest.Rdata"
#############################################################################


# 1. Load SAWS and SACTN data and site list---------------------------------

# SAWS
load("setupParams/SAWS_site_list.Rdata")
SAWS_site_list$dataset <- "SAWS"

# SACTN
load("setupParams/SACTN_site_list.Rdata")
SACTN_site_list$dataset <- "SACTN"


# 2. Calculate distance from each site between the datasets  and save ------

# Combine site lists
site_list <- rbind(SAWS_site_list[,c(1:3,23)], SACTN_site_list[,c(1,3,4,18)])

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
distances <- distances[1:352,]

# Cast to wide format
distances <- dcast(distances, row~col)
colnames(distances)[1] <- "SACTN"

# Separate the data frames
distances_SACTN <- distances[12:32,]
save(distances_SACTN, file = "setupParams/distances_SACTN.Rdata")
distances_SAWS <- distances[1:11,]
save(distances_SAWS, file = "setupParams/distances_SAWS.Rdata")

# 3. Calculate days of overlap for each site between the datasets ---------

## This isn't really necessary as the length of the land data far exceeds the sea data

# 4. Estimate best SAWS time series to use with each SACTN time seies -----

SACTN_SAWS_nearest <- data.frame()
for(i in 1:length(distances_SACTN$Jonkershoek)){
  x <- droplevels(distances_SACTN[i,])
  y <- melt(x, id.vars = c("SACTN"))
  best <- droplevels(y[y$value == min(y$value, na.rm = T),])
  colnames(best) <- c("SACTN", "SAWS", "distance")
  SACTN_SAWS_nearest <- rbind(SACTN_SAWS_nearest, best)
}

save(SACTN_SAWS_nearest, file = "setupParams/SACTN_SAWS_nearest.Rdata")

