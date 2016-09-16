#############################################################################
###"load/load.SAWS.R"
## This script does:
# 1. Create a function for loading the homogenised SAWS data
# 2. Load the SAWS data
# 3. Save for use in "prep/SAWS.sitelist.R"
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(plyr)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(purrr)
library(broom)
source("func/expand.gaps.R")
## USED BY:
# Nothing
## CREATES:
# "data/SAWS_homogenised.Rdata"
#############################################################################


# 1. Create a function for loading the homogenised SAWS data -----------

# The .csv format
SAWS.csv <- function(x){
  y <- read.csv(x, header = FALSE, skip = 0, sep = ",",
           col.names = c("year","month","day","precip", "tmax", "tmin"))
  y$site <- as.character(x)
  y$site <- sapply(strsplit(y$site, "/"), "[[", 5)
  y$site <- sapply(strsplit(y$site, ".csv"), "[[", 1)
  y$site <- as.factor(y$site)
  return(y)
}

# The load function
load.SAWS <- function(directory){
  files_list <- dir(directory, full.names = TRUE)
  # Load data
  dat <- ldply(files_list, SAWS.csv)
  # Correct to date format
  dat$date <- as.Date(paste(dat$year, dat$month, dat$day, sep = "-"), "%Y-%m-%d", tz = "Africa/Johannesburg")
  # Select appropriate columns
  dat <- dat[,c(7,8,5,6)]
  # Trim leading and trailing NA values, remove duplicate and fill gaps with NAs
  ## NB: Thiscauses immediate crashes of R
  # dat <- dat %>%
  #   group_by(site) %>%
  #   mutate(temp = na.trim(temp)) %>%
  #   nest() %>%
  #   mutate(mod = data %>% map(expand.gaps))
  # dat <- unnest(dat, mod)
  # Correct NA values
  dat$tmax[dat$tmax == -99.9] <- NA
  dat$tmin[dat$tmin == -99.9] <- NA
  # Create mean temperature column
  dat$temp <- (dat$tmax+dat$tmin)/2
  return(dat)
}


# 2. Load the SAWS data -------------------------------------------------

SAWS_homogenised <- load.SAWS("data/SAWS/homogenised/csv")
SAWS_homogenised <- ddply(SAWS_homogenised, .(site), expand.gaps) 
# No apparent gaps... but there appear to be later on in the workflow...
SAWS_homogenised <- ddply(SAWS_homogenised, .(site), na.trim) 
# Apparently there were still leading or trailing NA's
## 341506 -> 329279
SAWS_homogenised <- SAWS_homogenised[,c(2,1,3:5)]

# 3. Save for use in "prep/SAWS.sitelist.R" -------------------------------

save(SAWS_homogenised, file = "data/SAWS/homogenised/SAWS_homogenised.Rdata")
