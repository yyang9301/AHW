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
load("data/SAWS_grown.Rdata")
colnames(SAWS_grown)[2] <- "t"
load("setupParams/SAWS_site_list.Rdata")

# 2. Prep different SAWS temperature columns for use ----------------------

SAWS_tmean <- SAWS_grown[,c(1,2,5,6:10)]
SAWS_tmax <- SAWS_grown[,c(1,2,3,6:10)]
colnames(SAWS_tmax)[3] <-"temp" 
SAWS_tmin <- SAWS_grown[,c(1,2,4,6:10)]
colnames(SAWS_tmin)[3] <-"temp"


# 3. Calculate extreme SAWS events ----------------------------------------

system.time(SAWS_events_tmean <- ddply(SAWS_tmean, .(index), detect.SAWS, .parallel = TRUE)) ## 735 seconds
system.time(SAWS_events_tmax <- ddply(SAWS_tmax, .(index), detect.SAWS, .parallel = TRUE)) ## 607 seconds
system.time(SAWS_events_tmin <- ddply(SAWS_tmin, .(index), detect.SAWS, .parallel = TRUE)) ## 672 seconds


# 3. Save the results -----------------------------------------------------

save(SAWS_events_tmean, file = "data/SAWS_events_tmean.Rdata")
save(SAWS_events_tmax, file = "data/SAWS_events_tmax.Rdata")
save(SAWS_events_tmin, file = "data/SAWS_events_tmin.Rdata")

