#############################################################################
###"prep/SAWS.sitelist.R"
## This script does:
# 1. Load SAWS_homogenised.Rdata
# 2. Calculate meta-data
# 3. Manually assign lat/ lon
# 4. Combine and reorder columns
# 5. Reorder sites correctly along coast
# 6. Save as "setupParams/SAWS_site_list.Rdata" and "*.csv"
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
source("func/metaTemp.R")
## USED BY:
# Nothing
## CREATES:
# "setupParams/SAWS_site_list.Rdata"
# "setupParams/SAWS_site_list.csv"
#############################################################################


# 1. Load SAWS_homogenised.Rdata ------------------------------------------

load("data/SAWS/homogenised/SAWS_homogenised.Rdata")


# 2. Calculate meta-data --------------------------------------------------

# Daily minimum stats
wide_mn <- dcast(SAWS_homogenised, date ~ site, value.var = "tmin", mean)
wide_mn_zoo <- zoo(wide_mn[,2:length(colnames(wide_mn))], wide_mn$date)
data_summary_mn <- adply(wide_mn_zoo, 2, metaTemp)
names(data_summary_mn) <- c("site", "start_date", "end_date", "length", "temp_days_mn", "na_days_mn", "na_perc_mn", "mean_mn", "sd_mn", "min_mn", "max_mn")

# Daily maximum stats
wide_mx <- dcast(SAWS_homogenised, date ~ site, value.var = "tmax", mean)
wide_mx_zoo <- zoo(wide_mx[,2:length(colnames(wide_mx))], wide_mx$date)
data_summary_mx <- adply(wide_mx_zoo, 2, metaTemp)
names(data_summary_mx) <- c("site", "start_date", "end_date", "length", "temp_days_mx", "na_days_mx", "na_perc_mx", "mean_mx", "sd_mx", "min_mx", "max_mx")

# Daily mean stats
wide_t <- dcast(SAWS_homogenised, date ~ site, value.var = "temp", mean)
wide_t_zoo <- zoo(wide_t[,2:length(colnames(wide_t))], wide_t$date)
data_summary_t <- adply(wide_t_zoo, 2, metaTemp)
names(data_summary_t) <- c("site", "start_date", "end_date", "length", "temp_days_t", "na_days_t", "na_perc_t", "mean_t", "sd_t", "min_t", "max_t")
## Start and end dates for daily min and max were visually inspected and are the same


# 3. Manually assign lat/ lon ---------------------------------------------

# Create data frame of coordinates manually based on coords given in Kruger and Nxumalo 2016
coordinates <- data.frame(site = data_summary_t$site)
# Cape Agulhas, Cape Columbine, Cape Point, Cape St Blaize, Cape St Francis, Cape St Lucia
# Cape Town DF Malan, East London, Jonkershoek, Mount Edgecombe, Port Elizabeth
## NB: Cape St Francis and East London coordinates are from google earth as they are not given in Kruger and Nxumalo 2016
coordinates$lat <- c(-34.83, -32.83, -34.35, -34.18, -33.95, -28.50, 
                     -33.98, -32.95, -33.97, -29.70, -33.98)
coordinates$lon <- c(20.02, 17.85, 18.50, 22.15, 25.05, 32.40, 
                     18.60, 27.64, 18.93, 31.05, 25.60)
coordinates$height <- c(8, 63, 227, 76, NA, 107, 
                        46, NA, 350, 91, 60)


# 4. Combine and reorder columns ------------------------------------------

# The full meta-data frame
SAWS_site_list <- cbind(coordinates, data_summary_t[c(2:4,7:11)], data_summary_mx[c(7:11)], data_summary_mn[c(7:11)])

# Reorder correctly
SAWS_site_list  <- SAWS_site_list [c(1,3,2,4:8,13,18,9,14,19,10,15,20,11,16,21,12,17,22)]


# 5. Reorder sites correctly along coast ----------------------------------

# Load the shoreline
load("graph/shore.Rdata")
# Converts to data frame and keep the shoreline, not islands etc. The returned indices should be in the same order they appeared in in the original shoreline (i.e. coordinates arranged sequentially from east to west along the shoreline)
shore <- droplevels(subset(shore, (PID == 1)))
# Create index of site position along coast
sites_idx <- as.data.frame(knnx.index(shore[,4:5], as.matrix(SAWS_site_list[,2:3]), k = 1))
# Add the index to the front of the sites data.frame
SAWS_site_list <- cbind(sites_idx, SAWS_site_list)
# Order sites by index, west to east
SAWS_site_list <- SAWS_site_list[order(SAWS_site_list$V1, decreasing = FALSE),]
# Remove index column
SAWS_site_list$V1 <- NULL
# Relabel order of sites to match new sequential order
SAWS_site_list$order <- as.integer(1:length(SAWS_site_list$site))
# plot(SAWS_site_list$lon, SAWS_site_list$lat, pch = 1, cex = 0.3, col = "blue") # just checking --> okay
# Order index factor for use with other scripts
SAWS_site_list$site <- reorder(SAWS_site_list$site, SAWS_site_list$order)
# Remove order column
SAWS_site_list$order <- NULL


# 5. Save as "setupParams/SAWS_sitelist.Rdata" and "*.csv" ----------------

save(SAWS_site_list, file = "setupParams/SAWS_site_list.Rdata")
write.csv(SAWS_site_list, "setupParams/SAWS_site_list.csv", row.names = F)

