#############################################################################
###"proc/SAWS.RMarineHeatwaves.R"
## This script does:
# 1. Load the SAWS data and site lists
# 2. Prep different SAWS temperature columns for use
# 3. Calculate extreme SAWS events against alll SACTN analysis periods
# 4. Save results for analysis in "proc/coocurrence.R"
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(stringr)
library(zoo)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(xtable)
library(tibble)
library(RmarineHeatWaves) # Load this last to prevent purrr from replacing detect()
source("func/detect.full.R")
## USED BY:
# Nothing
## CREATES:
# 
#############################################################################


# 1. Load the SAWS data and site lists ---------------

# SAWS
load("data/SAWS/homogenised/SAWS_homogenised.Rdata")
colnames(SAWS_homogenised)[2] <- "t"
load("setupParams/SAWS_site_list.Rdata")

# 2. Prep different SAWS temperature columns for use ----------------------

SAWS_tmean <- SAWS_homogenised[,c(1,2,5)]
SAWS_tmax <- SAWS_homogenised[,c(1,2,3)]
colnames(SAWS_tmax)[3] <-"temp" 
SAWS_tmin <- SAWS_homogenised[,c(1,2,4)]
colnames(SAWS_tmin)[3] <-"temp"


# 3. Calculate extreme SAWS events ----------------------------------------

# test.sites <- levels(SAWS_tmean$site)[1] # No issues
# test.sites <- levels(SAWS_tmean$site)[1:5] # No issues
# test.sites <- levels(SAWS_tmean$site)[6:11] # No issues
# test.sites <- levels(SAWS_tmean$site)[1:7] # No issues
# test.sites <- levels(SAWS_tmean$site)[1:9] # No issues
# test.sites <- levels(SAWS_tmean$site) # No issues. Not sure what caused the error with the full data.frame. Perhaps a RAM issue
# 
# test <- droplevels(subset(SAWS_tmean, site %in% test.sites))
# 
# system.time(events <- ddply(test, .(site), detect.SAWS, .parallel = TRUE)) ## 687 seconds

# Using the SAWS analysis period of 1981 - 2010 for each time series
system.time(SAWS_SAWS_events_tmean <- ddply(SAWS_tmean, .(site), detect.SAWS, clim = "SAWS", .parallel = TRUE)) ## 18 seconds
system.time(SAWS_SAWS_events_tmax <- ddply(SAWS_tmax, .(site), detect.SAWS, clim = "SAWS", .parallel = TRUE)) ## 17 seconds
system.time(SAWS_SAWS_events_tmin <- ddply(SAWS_tmin, .(site), detect.SAWS, clim = "SAWS", .parallel = TRUE)) ## 17 seconds

system.time(SAWS_SACTN_events_tmean <- ddply(SAWS_tmean, .(site), detect.SAWS, clim = "SACTN", .parallel = TRUE)) ## 533 seconds
system.time(SAWS_SACTN_events_tmax <- ddply(SAWS_tmax, .(site), detect.SAWS, clim = "SACTN", .parallel = TRUE)) ## 558 seconds
system.time(SAWS_SACTN_events_tmin <- ddply(SAWS_tmin, .(site), detect.SAWS, clim = "SACTN", .parallel = TRUE)) ## 672 seconds


# 3. Save the results -----------------------------------------------------

# SAWS - SAWS
save(SAWS_SAWS_events_tmean, file = "data/SAWS_SAWS_events_tmean.Rdata")
save(SAWS_SAWS_events_tmax, file = "data/SAWS_SAWS_events_tmax.Rdata")
save(SAWS_SAWS_events_tmin, file = "data/SAWS_SAWS_events_tmin.Rdata")

# SAWS - SACTN
save(SAWS_SACTN_events_tmean, file = "data/SAWS_SACTN_events_tmean.Rdata")
save(SAWS_SACTN_events_tmax, file = "data/SAWS_SACTN_events_tmax.Rdata")
save(SAWS_SACTN_events_tmin, file = "data/SAWS_SACTN_events_tmin.Rdata")

