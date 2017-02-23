#############################################################################
###"prep/air.sea.state.R"
## This script does:
# 1. Load and create mean sea state
# 2. Load and create mean air state
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(data.table)
library(zoo)
library(tidyr)
library(purrr)
library(broom)
source("func/load.reanalyses.R")
## USED BY:
# "graph/figures3.R"
## CREATES:
# "data/sea_state.Rdata"
# "data/air_state.Rdata"
#############################################################################


# 1. Load and create mean sea state ---------------------------------------

# Indices for loading
temp_idx <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_temp", full.names = TRUE), 
                       x = 1:length(dir("~/data/BRAN", pattern = "ocean_temp")))
u_idx <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_u", full.names = TRUE), 
                       x = 1:length(dir("~/data/BRAN", pattern = "ocean_u")))
v_idx <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_v", full.names = TRUE), 
                       x = 1:length(dir("~/data/BRAN", pattern = "ocean_v")))

# Function for loading and creating monthly means
# idx <- temp_idx[1,]
BRAN.monthly <- function(idx){
  # print(paste0("Now loading ",basename(as.character(df$files)))) # Doesn't appear to work with dplyr
  system.time(BRAN_var <- ddply(idx, .(files), BRAN.Rdata, .progress = "text")) # ~16 seconds for one file
  BRAN_var$files <- NULL
  colnames(BRAN_var) <- c("x","y","var","date")
  BRAN_var <- data.table(BRAN_var)
  system.time(BRAN_var <- BRAN_var[, .(var = mean(var, na.rm = TRUE)), by = .(x,y)]) # 1 seconds
  return(BRAN_var)
}

## NB: THe monthly values have already been created
  ## No need to run again
# Load all temp, create total mean, and save monthly and total
# system.time(BRAN_temp <- ddply(temp_idx, .(files), BRAN.monthly, .parallel = T)) ## 3606 seconds
# BRAN_temp_monthly <- BRAN_temp
# save(BRAN_temp_monthly, file = "data/BRAN_temp_monthly.Rdata")
# BRAN_temp2 <- data.table(BRAN_temp)
# system.time(BRAN_temp2 <- BRAN_temp2[, .(var = mean(var, na.rm = TRUE)), by = .(x,y)]) # 6 seconds
# BRAN_temp_all <- BRAN_temp2
# save(BRAN_temp_all, file = "data/BRAN_temp_all.Rdata")

### Create daily climatologies by month to save processing power
## The following code is intentionally repetitive in order to allow it to be run piecemeal
## Flushing the RAM between steps as required
# var1 <- "temp"; mon <- "01" # testing...
BRAN.daily <- function(var1, mon){
  idx_mon <- data.frame(files = dir("~/data/BRAN", pattern = paste0("ocean_",var1,"_*.*_", mon), full.names = TRUE), 
                        x = 1:length(dir("~/data/BRAN", pattern = paste0("ocean_",var1,"_*.*_", mon))))
  system.time(BRAN_var <- ddply(idx_mon, .(files), BRAN.Rdata, .progress = "text")) # ~16 seconds for one file
  BRAN_var$files <- NULL
  colnames(BRAN_var) <- c("x","y","var","date")
  system.time(BRAN_var$date <- format(BRAN_var$date, "%m-%d")) # 34 seconds
  BRAN_var <- data.table(BRAN_var)
  system.time(BRAN_var <- BRAN_var[, .(var = mean(var, na.rm = TRUE)), by = .(x,y,date)]) # 1 seconds
  return(BRAN_var)
}

## Temperature
# January
system.time(BRAN_temp_jan <- BRAN.daily("temp", "01")) # 413 seconds
save(BRAN_temp_jan, file = "data/BRAN_temp_jan.Rdata")



temp_idx_jan <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_temp_*.*_01", full.names = TRUE), 
                       x = 1:length(dir("~/data/BRAN", pattern = "ocean_temp_*.*_01")))
system.time(BRAN_temp_jan <- ddply(temp_idx_jan, .(files), BRAN.Rdata, .progress = "text")) # 300 seconds
system.time(BRAN_temp_jan$date2 <- format(BRAN_temp_jan$date, "%m-%d")) # 34 seconds

test <- BRAN_temp_jan[c(1:300, 1395001:1395300),]
test <- data.table(test)
test$date2 <- as.factor(test$date2)
system.time(test <- test[, .(var = mean(var, na.rm = TRUE)), by = .(x,y)]) # 215 seconds
save(BRAN_temp_jan, file = "data/BRAN_temp_jan.Rdata")


BRAN_temp_daily <- data.frame()
for(i in 1:length(temp_idx$files)){
  print(paste0("Now loading ",basename(as.character(temp_idx[i,1]))))
  df1 <- BRAN.Rdata(temp_idx[i,])
  df1$date2 <- format(df1$date, "%m-%d")
  BRAN_temp_daily <- rbind(BRAN_temp_daily, df1)
  BRAN_temp_daily <- data.table(BRAN_temp_daily)
  system.time(BRAN_temp_daily <- BRAN_temp_daily[, .(var = mean(var, na.rm = TRUE)), by = .(x,y,date2)]) # 6 seconds
}


# 2. Load and create mean air state ---------------------------------------

