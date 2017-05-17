############################################################################
# This script prepares the SACTN, BRAN and ERA-Interim data used in the analysis:
# 1. Load all libraries and functions used in this script 
# 2. Load and crop the daily SACTN data
# 3. Load BRAN daily data and create clims
# 4. Load ERA daily data and create clims
############################################################################


# 1. Load all libraries and functions used in this script  ----------------
library(tidyverse)
library(doMC); registerDoMC(cores = 4)
source("func/load.reanalyses.R")


# 2. Load and crop the daily SACTN data -----------------------------------

# Note that the daily data are not publicly available due to the data 
  # policies of several of the contributors.
# For further information on these policies please contact:
  # 3503570@myuwc.ac.za
# The monthly data are available for download at: 
  # https://robert-schlegel.shinyapps.io/SACTN/

# Load data and meta-data
load("~/SACTNraw/data/SACTNdaily_v4.1.Rdata")
load("~/SACTNraw/metadata/site_list_v4.1.Rdata") # 135 sites

# Use site list to create index of usable sites
site_list <- droplevels(site_list[site_list$NA.perc <= 10, ]) # 50 sites
site_list <- droplevels(site_list[site_list$length >= 3650, ]) # 26 sites
SACTN_cropped <- droplevels(SACTNdaily_v4.1[SACTNdaily_v4.1$index %in% site_list$index,])
SACTN_site_list <- site_list

# Differentiate between the two Tsitsikamma sites
SACTN_cropped$site <- as.character(SACTN_cropped$site)
SACTN_cropped$site[SACTN_cropped$site == "Tsitsikamma" & SACTN_cropped$src == "SAWS"] <- "Tsitsikamma West"
SACTN_cropped$site[SACTN_cropped$site == "Tsitsikamma" & SACTN_cropped$src == "DEA"] <- "Tsitsikamma East"
SACTN_cropped$site <- as.factor(SACTN_cropped$site)
# levels(SACTN_cropped$site)
SACTN_site_list$site <- as.character(SACTN_site_list$site)
SACTN_site_list$site[SACTN_site_list$site == "Tsitsikamma" & SACTN_site_list$src == "SAWS"] <- "Tsitsikamma West"
SACTN_site_list$site[SACTN_site_list$site == "Tsitsikamma" & SACTN_site_list$src == "DEA"] <- "Tsitsikamma East"
SACTN_site_list$site <- as.factor(SACTN_site_list$site)
# levels(SACTN_site_list$site)

# Save SACTN data
save(SACTN_cropped, file = "~/data/SACTN/AHW/SACTN_cropped.Rdata")
rm(SACTN_cropped, SACTNdaily_v4.1)

# Save SACTN meta-data
save(SACTN_site_list, file = "setupParams/SACTN_site_list.Rdata")
write.csv(SACTN_site_list, file = "setupParams/SACTN_site_list.csv")
rm(site_list, SACTN_site_list)


# 3. Load BRAN daily data and create clims --------------------------------

# The script used to download the BRAN data: "data/download.BRAN.R"
# These data are not stored on this GitHub page as they total 2.2 GB

# Indices for loading
temp_idx <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_temp", full.names = TRUE), 
                       x = 1:length(dir("~/data/BRAN", pattern = "ocean_temp")))
u_idx <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_u", full.names = TRUE), 
                    x = 1:length(dir("~/data/BRAN", pattern = "ocean_u")))
v_idx <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_v", full.names = TRUE), 
                    x = 1:length(dir("~/data/BRAN", pattern = "ocean_v")))

# Daily temperature climatologies
  # This is not much slower but much more stable on only one core...
system.time(BRAN_temp_daily <- plyr::ddply(temp_idx, c("files"), BRAN.Rdata, .progress = "text")) # 135 seconds
BRAN_temp_daily$files <- NULL
system.time(BRAN_temp_clim <- plyr::ddply(BRAN_temp_daily, c("x","y"), grid.clim, .progress = "text")) # 659 seconds
colnames(BRAN_temp_clim)[4] <- "temp"
save(BRAN_temp_clim, file = "data/BRAN/BRAN_temp_clim.Rdata")
rm(BRAN_temp_daily, BRAN_temp_clim)

# Daily U climatologies
system.time(BRAN_u_daily <- plyr::ddply(u_idx, c("files"), BRAN.Rdata, .progress = "text")) # 125 seconds
BRAN_u_daily$files <- NULL
system.time(BRAN_u_clim <- plyr::ddply(BRAN_u_daily, c("x","y"), grid.clim, .progress = "text")) # 715 seconds
colnames(BRAN_u_clim)[4] <- "u"
save(BRAN_u_clim, file = "data/BRAN/BRAN_u_clim.Rdata")
rm(BRAN_u_daily, BRAN_u_clim)

# Daily V climatologies
system.time(BRAN_v_daily <- plyr::ddply(v_idx, c("files"), BRAN.Rdata, .progress = "text")) # 146 seconds
BRAN_v_daily$files <- NULL
system.time(BRAN_v_clim <- plyr::ddply(BRAN_v_daily, c("x","y"), grid.clim, .progress = "text")) # 760 seconds
colnames(BRAN_v_clim)[4] <- "v"
save(BRAN_v_clim, file = "data/BRAN/BRAN_v_clim.Rdata")
rm(BRAN_v_daily, BRAN_v_clim)


# 4. Load ERA daily data and create clims ---------------------------------

# The ERA-Interim data were downloaded using the providers web interface
# These data are not stored on this GitHub page as they total 650 MB

# Create date range index so that same date range from BRAN is used for ERA
BRAN_date_range <- seq(as.Date("1994-01-01"), as.Date("2016-08-31"), by = "day")

# Load all ERA daily values
  # ERA1 not loaded as these dates precede any available BRAN data
system.time(ERA2 <- ERA.daily("~/data/ERA/ERA_1990_1998.nc")) # 42 seconds
system.time(ERA3 <- ERA.daily("~/data/ERA/ERA_1999_2007.nc")) # 41 seconds
system.time(ERA4 <- ERA.daily("~/data/ERA/ERA_2008_2016.nc")) # 40 seconds

# Create single dataframe for subsetting
ERA_all_daily <- rbind(ERA2, ERA3, ERA4)
ERA_all_daily <- filter(ERA_all_daily, date %in% BRAN_date_range)
rm(ERA2, ERA3, ERA4)

# Split into three different dataframes
ERA_temp_daily <- ERA_all_daily[,c(1,2,4,3)]
ERA_u_daily <- ERA_all_daily[,c(1,2,5,3)]
ERA_v_daily <- ERA_all_daily[,c(1,2,6,3)]
rm(ERA_all_daily)

# Daily temperature climatologies
system.time(ERA_temp_clim <- plyr::ddply(ERA_temp_daily, c("x","y"), grid.clim, .progress = "text")) # 957 seconds
colnames(ERA_temp_clim)[4] <- "temp"
save(ERA_temp_clim, file = "data/ERA/ERA_temp_clim.Rdata")
rm(ERA_temp_daily, ERA_temp_clim)

# Daily U climatologies
system.time(ERA_u_clim <- plyr::ddply(ERA_u_daily, c("x","y"), grid.clim, .progress = "text")) # 912 seconds
colnames(ERA_u_clim)[4] <- "u"
save(ERA_u_clim, file = "data/ERA/ERA_u_clim.Rdata")
rm(ERA_u_daily, ERA_u_clim)

# Daily V climatologies
system.time(ERA_v_clim <- plyr::ddply(ERA_v_daily, c("x","y"), grid.clim, .progress = "text")) # 889 seconds
colnames(ERA_v_clim)[4] <- "v"
save(ERA_v_clim, file = "data/ERA/ERA_v_clim.Rdata")
rm(ERA_v_daily, ERA_v_clim)

