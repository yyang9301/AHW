#############################################################################
###"load/load.SAWS.R"
## This script does:
# 1. Create a function for loading the homogenised SAWS data
# 2. Load the SAWS data
# 3. Save for use in "prep/SAWS.meta.R"
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(plyr)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(purrr)
library(broom)
library(readr)
library(RmarineHeatWaves)
source("func/expand.gaps.R")
## USED BY:
# Nothing
## CREATES:
# "data/SAWS_homogenised.Rdata"
#############################################################################


# 1. Create a function for loading the homogenised SAWS data -----------

# The .csv format
SAWS.csv <- function(x){
  y <- read_csv(x, skip = 0, col_types = "cnnnnn",
           col_names = c("year","month","day","precip", "tmax", "tmin"))
  # Correct to date format
  y$date <- as.Date(paste(y$year, y$month, y$day, sep = "-"), "%Y-%m-%d", tz = "Africa/Johannesburg")
  # Assign site
  y$site <- sapply(strsplit(as.character(x), "/"), "[[", 5)
  y$site <- sapply(strsplit(y$site, ".csv"), "[[", 1)
  # Correct NA values
  y$tmax[y$tmax == -99.9] <- NA
  y$tmin[y$tmin == -99.9] <- NA
  # Create mean temperature column
  y$temp <- (y$tmax+y$tmin)/2
  # Select appropriate columns
  y <- y[,c(8,7,5,6,9)]
  return(y)
}

# The load function
load.SAWS <- function(directory){
  files_list <- dir(directory, full.names = TRUE)
  dat <- ldply(files_list, SAWS.csv, .parallel = T)
  return(dat)
}


# 2. Load the SAWS data -------------------------------------------------

SAWS_homogenised <- load.SAWS("data/SAWS/homogenised/csv") # 341506
SAWS_homogenised <- ddply(SAWS_homogenised, .(site), fast.gaps.air, .parallel = T) # 329279

# 3. Save for use in "prep/SAWS.meta.R" -------------------------------

save(SAWS_homogenised, file = "data/SAWS/homogenised/SAWS_homogenised.Rdata")
