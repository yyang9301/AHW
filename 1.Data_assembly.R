############################################################################
### "1.Data_assembly.R"
## This script prepares the SACTN, BRAN and ERA-Interim data used in the analysis
# 1. Load all libraries and functions used in this script 
# 2. Load and crop the daily SACTN data
# 3. Load ERA daily data and create clims
# 4. Load OISST data and create clims
# 5. Load AVISO data and create clims
############################################################################


# 1. Load all libraries and functions used in this script  ----------------
source("func/load.reanalyses.R")
source("func/load.remote.R")
#

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


# 3. Load ERA daily data and create clims ---------------------------------

# The ERA-Interim data were downloaded using the providers web interface
# These data are not stored on this GitHub page as they total 650 MB

# Create date range index to set the standard for all datasets
date_range <- seq(as.Date("1993-01-01"), as.Date("2016-12-31"), by = "day")

# Load all ERA daily values
  # ERA1 not loaded as these dates precede any available BRAN data
# system.time(ERA1 <- ERA.daily("~/data/ERA/ERA_1979_1989.nc")) # 42 seconds
system.time(ERA2 <- ERA.daily("~/data/ERA/ERA_1990_1998.nc")) # 42 seconds
system.time(ERA3 <- ERA.daily("~/data/ERA/ERA_1999_2007.nc")) # 41 seconds
system.time(ERA4 <- ERA.daily("~/data/ERA/ERA_2008_2016.nc")) # 40 seconds

# Create single dataframe for subsetting
ERA_all_daily <- rbind(ERA2, ERA3, ERA4)
ERA_all_daily <- filter(ERA_all_daily, date %in% date_range)
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


# 4. Load OISST data and create clims -------------------------------------

# These data are not stored on this GitHub page as they total 107 GB

# Indices for loading
OISST_idx <- data.frame(files = dir("~/data/OISST/netCDF", pattern = "avhrr-only", full.names = TRUE), 
                       x = 1:length(dir("~/data/OISST/netCDF", pattern = "avhrr-only")))[4141:12775,]

# Daily temperature climatologies
# This is not much slower but much more stable on only one core...
system.time(OISST_temp_daily <- plyr::ddply(OISST_idx, c("files"), OISST.daily, .progress = "text")) # 2495 seconds
OISST_temp_daily$files <- NULL
system.time(OISST_temp_clim <- plyr::ddply(OISST_temp_daily, c("x","y"), grid.clim, .progress = "text")) # 3117 seconds
colnames(OISST_temp_clim)[4] <- "temp"
save(OISST_temp_clim, file = "data/OISST/OISST_temp_clim.Rdata")
rm(OISST_temp_daily, OISST_temp_clim)


# 6. Load AVISO data and create clims -------------------------------------

# These data are not stored on this GitHub page as they total 22 GB

# Indices for loading
AVISO_idx <- data.frame(files = dir("~/data/AVISO", pattern = "madt", full.names = TRUE), 
                        x = 1:length(dir("~/data/AVISO", pattern = "madt")))#[4141:12775,]

# Daily U and V values
# This is not much slower but much more stable on only one core...
system.time(AVISO_temp_daily <- plyr::ddply(AVISO_idx, c("files"), AVISO.daily, .progress = "text")) # 2495 seconds
AVISO_temp_daily$files <- NULL

# Split into three different dataframes
AVISO_u_daily <- AVISO_all_daily[,c(1,2,4,3)]
AVISO_v_daily <- AVISO_all_daily[,c(1,2,5,3)]
rm(AVISO_all_daily)

# Daily U climatologies
system.time(AVISO_u_clim <- plyr::ddply(AVISO_u_daily, c("x","y"), grid.clim, .progress = "text")) # 912 seconds
colnames(AVISO_u_clim)[4] <- "u"
save(AVISO_u_clim, file = "data/AVISO/AVISO_u_clim.Rdata")
rm(AVISO_u_daily, AVISO_u_clim)

# Daily V climatologies
system.time(AVISO_v_clim <- plyr::ddply(AVISO_v_daily, c("x","y"), grid.clim, .progress = "text")) # 889 seconds
colnames(AVISO_v_clim)[4] <- "v"
save(AVISO_v_clim, file = "data/AVISO/AVISO_v_clim.Rdata")
rm(AVISO_v_daily, AVISO_v_clim)

