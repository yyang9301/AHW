#############################################################################
###"proc/ALL.RMarineHeatwaves.R"
## This script does:
# 1. Load the SAWS and SACTN data and site lists
# 2. Create function to pair each SACTN and SAWS time series and calculate shared events based on analysis period defined as full length of the SACTN time series
# 3. Compute and save results for analysis in "proc/coocurrence.R"
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(stringr)
library(zoo)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
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
SAWS_homogenised$dataset <- "SAWS"
load("setupParams/SAWS_site_list.Rdata")

# SACTN
load("data/SACTN/SACTN_cropped.Rdata")
SACTN_cropped$dataset <- "SACTN"
SACTN_cropped <- SACTN_cropped[,c(1,4:6)]
load("setupParams/SACTN_site_list.Rdata")

# Distance matrices
load("setupParams/SACTN_SAWS_nearest.Rdata")
load("setupParams/distances_SACTN.Rdata")
distances_SACTN$SACTN <- as.character(distances_SACTN$SACTN)


# 2. Pair each SACTN and SAWS time series to calculate shared events --------

# Prep differetn SAWS temperatures for use
SAWS_tmean <- SAWS_homogenised[,c(1,2,5,6)]
SAWS_tmax <- SAWS_homogenised[,c(1,2,3,6)]
colnames(SAWS_tmax)[3] <-"temp" 
SAWS_tmin <- SAWS_homogenised[,c(1,2,4,6)]
colnames(SAWS_tmin)[3] <-"temp"

## For testing
# SAWS <- SAWS_tmean; SACTN <- SACTN_cropped
# i <- 1
# j <- 1
# j <- 2

event.pair <- function(SAWS, SACTN){
  SAWS_SACTN_pair_event <- data.frame()
  SAWS_SACTN_pair_clim <- data.frame()
  for(i in 1:length(levels(SACTN$site))){
    SACTN1 <- droplevels(SACTN[SACTN$site == levels(as.factor(SACTN$site))[i],])
    SACTN1$site <- as.character(SACTN1$site)
    for(j in 1:length(levels(SAWS$site))){
      SAWS1 <- droplevels(SAWS[SAWS$site == levels(as.factor(SAWS$site))[j],])
      SAWS1$site <- as.character(SAWS1$site)
      dist <- distances_SACTN[distances_SACTN$SACTN == SACTN1$site[1],]
      dist <- round_any(as.numeric(dist[colnames(dist) == SAWS1$site[1]]), 0.01)
      # Find common dates
      SAWS2 <- SAWS1[SAWS1$date %in% SACTN1$date,]
      SACTN2 <- SACTN1[SACTN1$date %in% SAWS1$date,]
      if(nrow(SACTN2) != nrow(SAWS2)){
        stop(paste(SACTN1$site[1], "(SACTN) is not the same length as", SAWS1$site[1], "(SAWS).", sep = " "))
      }
      start <- year(SACTN2$date[1])+1
      end <- year(SACTN2$date[nrow(SACTN2)])-1
      # Calculate extreme events
      ## SACTN
      SACTN_hw <- detect.full(dat = SACTN1, start = start, end = end, dur = 5, gap = 2, cold_spell = FALSE)
      SACTN_hw_event <- SACTN_hw$event
      SACTN_hw_event$dataset <- "SACTN"
      SACTN_hw_event$type <- "hw"
      SACTN_hw_clim <- SACTN_hw$clim
      SACTN_hw_clim$dataset <- "SACTN"
      SACTN_hw_clim$type <- "hw"
      rm(SACTN_hw)
      SACTN_cs <- detect.full(dat = SACTN1, start = start, end = end, dur = 5, gap = 2, cold_spell = TRUE)
      SACTN_cs_event <- SACTN_cs$event
      SACTN_cs_event$dataset <- "SACTN"
      SACTN_cs_event$type <- "cs"
      SACTN_cs_clim <- SACTN_cs$clim
      SACTN_cs_clim$dataset <- "SACTN"
      SACTN_cs_clim$type <- "cs"
      rm(SACTN_cs)
      ## SAWS
      SAWS_hw <- detect.full(dat = SAWS1, start = start, end = end, dur = 3, gap = 0, cold_spell = FALSE)
      SAWS_hw_event <- SAWS_hw$event
      SAWS_hw_event$dataset <- "SAWS"
      SAWS_hw_event$type <- "hw"
      SAWS_hw_clim <- SAWS_hw$clim
      SAWS_hw_clim$dataset <- "SAWS"
      SAWS_hw_clim$type <- "hw"
      rm(SAWS_hw)
      SAWS_cs <- detect.full(dat = SAWS1, start = start, end = end, dur = 3, gap = 0, cold_spell = TRUE)
      SAWS_cs_event <- SAWS_cs$event
      SAWS_cs_event$dataset <- "SAWS"
      SAWS_cs_event$type <- "cs"
      SAWS_cs_clim <- SAWS_cs$clim
      SAWS_cs_clim$dataset <- "SAWS"
      SAWS_cs_clim$type <- "cs"
      rm(SAWS_cs)
      # Combine results and add columns for use with later analyses
      pair_event <- rbind(SACTN_hw_event, SACTN_cs_event, SAWS_hw_event, SAWS_cs_event)
      pair_event <- data.frame(pair_event)
      pair_event$facet <- paste(SACTN1$site[1], SAWS1$site[1], sep = " - ")
      pair_event$distance <- dist
      pair_clim <- rbind(SACTN_hw_clim, SACTN_cs_clim, SAWS_hw_clim, SAWS_cs_clim)
      pair_clim <- data.frame(pair_clim)
      pair_clim$facet <- paste(SACTN1$site[1], SAWS1$site[1], sep = " - ")
      pair_clim$distance <- dist
      # Calculate stats and values based on shared dates
      pair_event$r <-  round_any(cor(SACTN2$temp, SAWS2$temp, use = "pairwise.complete.obs"), 0.01)
      # Wrap it up
      SAWS_SACTN_pair_event <- rbind(SAWS_SACTN_pair_event, pair_event)
      SAWS_SACTN_pair_clim <- rbind(SAWS_SACTN_pair_clim, pair_clim)
    }
  }
  results <- list(events = SAWS_SACTN_pair_event, clim = SAWS_SACTN_pair_clim)
  return(results)
}


# 3. Save results for analysis in "proc/coocurrence.R" --------------------

system.time(SAWS_SACTN_pair_tmean <- event.pair(SAWS_tmean, SACTN_cropped)) ## 1987 seconds
save(SAWS_SACTN_pair_tmean, file = "data/SAWS_SACTN_pair_tmean.Rdata")
system.time(SAWS_SACTN_pair_tmax <- event.pair(SAWS_tmax, SACTN_cropped)) ## 1903 seconds
save(SAWS_SACTN_pair_tmax, file = "data/SAWS_SACTN_pair_tmax.Rdata")
system.time(SAWS_SACTN_pair_tmin <- event.pair(SAWS_tmin, SACTN_cropped)) ## xx seconds
save(SAWS_SACTN_pair_tmin, file = "data/SAWS_SACTN_pair_tmin.Rdata")

