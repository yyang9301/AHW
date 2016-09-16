#############################################################################
###"prep/SAWS.SACTN.pair.R"
## This script does:
# 1. Load the SAWS and SACTN data and site lists
# 2. Create dataframe of each possible pairing between SACTN and SAWS datasets
# 3. Save results for analysis in "proc/coocurrence.R"
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(stringr)
library(zoo)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(broom)
library(tibble)
library(purrr)
library(RmarineHeatWaves) # Load this last to prevent purrr from replacing detect()
## USED BY:
# Nothing
## CREATES:
# "data/SAWS_SACTN_pair.Rdata"
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


# 2. Create dataframe of each possible pairing between SACTN and SAWS -----

## NB: This script was not run as ultimately it takes too long and is unneccesary

dataset.pair <- function(SAWS, SACTN){
  SAWS_SACTN_pair <- data.frame()
  for(i in 1:length(levels(SACTN$site))){
    SACTN1 <- droplevels(SACTN[SACTN$site == levels(as.factor(SACTN$site))[i],])
    SACTN1$site <- as.character(SACTN1$site)
    for(j in 1:length(levels(SAWS$site))){
      SAWS1 <- droplevels(SAWS[SAWS$site == levels(as.factor(SAWS$site))[j],])
      SAWS1$site <- as.character(SAWS1$site)
      pair <- rbind(SACTN1, SAWS1)
      pair$facet <- paste(SACTN1$site[1], SAWS1$site[1], sep = " - ")
      dist <- distances_SACTN[distances_SACTN$SACTN == SACTN1$site[1],]
      dist <- round_any(as.numeric(dist[colnames(dist) == SAWS1$site[1]]), 0.01)
      pair$distance <- dist
      # Find common dates
      SAWS2 <- SAWS1[SAWS1$date %in% SACTN1$date,]
      SACTN2 <- SACTN1[SACTN1$date %in% SAWS1$date,]
      if(nrow(SACTN2) != nrow(SAWS2)){
        stop(paste(SACTN1$site[1], "(SACTN) is not the same length as", SAWS1$site[1], "(SAWS).", sep = " "))
      }
      # Calculate stats and values based on shared dates
      pair$r <-  round_any(cor(SACTN2$temp, SAWS2$temp, use = "pairwise.complete.obs"), 0.01)
      pair$a.start <- year(SACTN2$date[1])+1
      pair$a.end <- year(SACTN2$date[nrow(SACTN2)])-1
      pair$period <- pair$a.end - pair$a.start
      # Wrap it up
      SAWS_SACTN_pair <- rbind(SAWS_SACTN_pair, pair)
    }
  }
  return(SAWS_SACTN_pair)
}

SAWS_tmean <- SAWS_homogenised[,c(1,2,5,6)]
SAWS_tmax <- SAWS_homogenised[,c(1,2,3,6)]
colnames(SAWS_tmax)[3] <-"temp" 
SAWS_tmin <- SAWS_homogenised[,c(1,2,4,6)]
colnames(SAWS_tmin)[3] <-"temp"

# Create paired dataframes
system.time(SAWS_SACTN_pair_tmean <- dataset.pair(SAWS_tmean, SACTN_cropped))
system.time(SAWS_SACTN_pair_tmax <- dataset.pair(SAWS_tmax, SACTN_cropped))
system.time(SAWS_SACTN_pair_tmin <- dataset.pair(SAWS_tmin, SACTN_cropped))

# 3. Save as "data/SACTN_SAWS_match.Rdata" --------------------------------

# save(SAWS_SACTN_match, file = "data/SAWS_SACTN_match.Rdata")
