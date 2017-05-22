#############################################################################
###"proc/SACTN.RMarineHeatwaves.R"
## This script does:
# 1. Load the SACTN data and site list
# 2. Calculate the extreme SACTN events based on the full years of data
# 3. Save results for analysis in "proc/coocurrence.R"
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
# "data/SACTN_events.Rdata"
#############################################################################


# 1. Load the SACTN data and site list ---------------

# SACTN
load("data/SACTN/SACTN_cropped.Rdata")
SACTN_cropped <- SACTN_cropped[,c(1,4:5)]
colnames(SACTN_cropped)[2] <- "t"
load("setupParams/SACTN_site_list.Rdata")
load("setupParams/SACTN_analysis_period.Rdata")
SACTN_cropped <- SACTN_cropped %>% 
  group_by(site) %>% 
  mutate(start = SACTN_analysis_period$start[SACTN_analysis_period$site == site][1]) %>% 
  mutate(end = SACTN_analysis_period$end[SACTN_analysis_period$site == site][1])
SACTN_cropped <- data.frame(SACTN_cropped)

SACTN_cropped$idx <- as.numeric(SACTN_cropped$site) # Use numeric values as site names introduce problems

# SACTN_cropped <- filter(SACTN_cropped, site %in% levels(SACTN_cropped$site)[1:4]) # Testing

# 2. Calculate the extreme SACTN events -----------------------------------

system.time(SACTN_all <- dlply(SACTN_cropped, .(idx), detect.SACTN, .parallel = TRUE)) ## 54 seconds


# 3. Save results for analysis in "proc/coocurrence.R" --------------------

# Events
SACTN_events <- data.frame()
for(i in 1:max(SACTN_cropped$idx)){
  events <- SACTN_all[[i]]$event
  SACTN_events <- rbind(SACTN_events, events)
}
save(SACTN_events, file = "data/events/SACTN_events.Rdata")

# Climatologies
SACTN_clims <- data.frame()
for(i in 1:max(SACTN_cropped$idx)){
  clims <- SACTN_all[[i]]$clim
  SACTN_clims <- rbind(SACTN_clims, clims)
}
save(SACTN_clims, file = "data/events/SACTN_clims.Rdata")
