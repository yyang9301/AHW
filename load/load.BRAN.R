#############################################################################
###"load/load.BRAN.R"
## This script does:
# 1. Create a function for loading the BRAN data
# 2. Load the BRAN data
# 3. Merge and save
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(plyr)
library(dplyr)
library(reshape2)
# source("func/expand.gaps.R")
## USED BY:
# Nothing
## CREATES:
# All of the BRAN dataframes
#############################################################################


# 1. Create a function for loading the BRAN data --------------------------

# The .Rdata format
# x <- files_list[1,] # tester...
BRAN.Rdata <- function(x){
  BRAN_file <- as.character(x$files)
  load(BRAN_file)
  stor_length <- nrow(as.data.frame(stor.nc$var[1,1,]))
  # Unfortunately the date value is not saved directly in the list
  # It is necessary to use a for loop to construct the date while subsetting
  BRAN_data <- data.frame()
  for(i in 1:stor_length){
    # Subset and melt one day of data
    stor2 <- stor.nc$var[,,i]
    rownames(stor2) <- stor.nc$x
    colnames(stor2) <- stor.nc$y
    stor3 <- melt(stor2)
    stor_var <- sapply(strsplit(basename(BRAN_file), "_"), "[[", 2)
    colnames(stor3) <- c("x", "y", stor_var)
    stor_year <- sapply(strsplit(basename(BRAN_file), "_"), "[[", 3)
    stor_month <- sapply(strsplit(basename(BRAN_file), "_"), "[[", 4)
    stor_month <- sapply(strsplit(stor_month, ".Rdata"), "[[", 1)
    stor_day <- i
    stor3$date <- as.Date(paste0(stor_year,"-",stor_month,"-",stor_day))
    BRAN_data <- rbind(BRAN_data, stor3)
  }
  return(BRAN_data)
}


# 2. Load the BRAN data ---------------------------------------------------

# The file lists
# list_all <- data.frame(index = 1:length(dir("~/data/BRAN", full.names = TRUE)), files = dir("~/data/BRAN", full.names = TRUE))
# list_temp <- data.frame(index = 1:length(dir("~/data/BRAN", full.names = TRUE, pattern = "*ocean_temp_*")), files = dir("~/data/BRAN", full.names = TRUE, pattern = "*ocean_temp_*"))
# list_u <- data.frame(index = 1:length(dir("~/data/BRAN", full.names = TRUE, pattern = "*ocean_u_*")), files = dir("~/data/BRAN", full.names = TRUE, pattern = "*ocean_u_*"))
# list_v <- data.frame(index = 1:length(dir("~/data/BRAN", full.names = TRUE, pattern = "*ocean_v_*")), files = dir("~/data/BRAN", full.names = TRUE, pattern = "*ocean_v_*"))
# list_w <- data.frame(index = 1:length(dir("~/data/BRAN", full.names = TRUE, pattern = "*ocean_w_*")), files = dir("~/data/BRAN", full.names = TRUE, pattern = "*ocean_w_*"))

### NB: No longer loading the entire dataset as it crashes my computer ###
  ### Only the required month(s) is loaded when creating the figures for publication

## Load the data
# Temp
# system.time(BRAN_temp1 <- ddply(list_temp[1:68,], .(index), BRAN.Rdata, .progress = "text")) # xxx seconds
# system.time(BRAN_temp2 <- ddply(list_temp[69:136,], .(index), BRAN.Rdata, .parallel = T)) # xxx seconds
# system.time(BRAN_temp3 <- ddply(list_temp[137:204,], .(index), BRAN.Rdata, .parallel = T))
# system.time(BRAN_temp4 <- ddply(list_temp[205:272,], .(index), BRAN.Rdata, .parallel = T))
# 
# BRAN_u <- ddply(list_u, .(index), BRAN.Rdata, .parallel = T)
# BRAN_v <- ddply(list_v, .(index), BRAN.Rdata, .parallel = T)
# BRAN_w <- ddply(list_w, .(index), BRAN.Rdata, .parallel = T)

# 3. Merge and save -------------------------------------------------------

# Temp
# Bran_temp <- rbind(BRAN_temp1, BRAN_temp2, BRAN_temp3, BRAN_temp4)
# save(BRAN_temp, file = "data/BRAN/BRAN_temp.Rdata")
