# Load packages etc. ------------------------------------------------------
setwd("AHW/")
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(FNN)
library(FedData)
library(doMC); doMC::registerDoMC(cores = 4)
source("func/metaTemp.R") # Afunction that calculates meta-deta/ stats of time series
source("func/scaleBarFunc.R") # A custom ggplot function that creates a very snazy scale bar


# Load and visualise data ----------------------

# NOAA files list
files_list <- dir("~/AHW/data/NOAA/DAILY/", full.names = FALSE)
files_list <- sapply(strsplit(files_list, ".dly"), "[[", 1) # Remove .dly file suffix

# Function to extract data
make_ghcn_list <- function(df){
  get_ghcn_daily_station(df, elements = NULL, "data/NOAA/", standardize = F, force.redo = F)
}

# All NOAA GHCN data in a list
system.time(NOAA_ls_1 <- alply(files_list[1:4], 1, make_ghcn_list, .progress = "text", .parallel = TRUE)) # .parallel is twice as fast

# Convert to data frame for further use
NOAA_df_1 <- ldply(NOAA_ls_1, station_to_data_frame)



# Calculate relationship of DT_perc with length for all DTs and precisions
mod_lm <- dlply(gls_df_natural_no_0, .(DT, prec), .progress = "text", .parallel = FALSE, lm_fun)
lm_df <- ldply(mod_lm, data.frame, .progress = "text")




station_to_data_frame("data/NOAA/DAILY/SF003670660.dly")

get_ghcn_daily(template = NULL, label = NULL, elements = c("TMIN","TMAX"),
               raw.dir = "./RAW/GHCN/", extraction.dir = "./EXTRACTIONS/GHCN/",
               standardize = F, force.redo = F)

get_ghcn_daily_station("SF004041770", elements = c('tmin','tmax'), "data/NOAA/", standardize = F, force.redo = F)

test <- get_ghcn_daily_station("SF004041770", elements = NULL, "data/NOAA/", standardize = F, force.redo = F)

test2 <- station_to_data_frame(test)

read.table("data/NOAA/DAILY/SF003670660.dly")
