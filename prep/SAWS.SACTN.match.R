#############################################################################
###"prep/SAWS.SACTN.match.R"
## This script does:
# 1. Load SAWS and SACTN data, site lists and matching info
# 2. Subset SAWS data to be same length as matching SACTN time series
# 3. Save as "data/SACTN_SAWS_match.Rdata"
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
source("func/expand.gaps.R")
## USED BY:
# "graph/SACTN.SAWS.line.R"
## CREATES:
# "data/SACTN_SAWS_match.Rdata"
#############################################################################


# 1. Load SAWS and SACTN data and site list---------------------------------

# SAWS
load("data/SAWS/homogenised/SAWS_homogenised.Rdata")
load("setupParams/SAWS_site_list.Rdata")

# SACTN
load("data/SACTN/SACTN_cropped.Rdata")
load("setupParams/SACTN_site_list.Rdata")

# matching
load("setupParams/SACTN_SAWS_nearest.Rdata")


# 2. Subset SAWS data to be same length as matching SACTN time series -----

SAWS_SACTN_match <- data.frame()
for(i in 1:length(levels(SACTN_SAWS_nearest$SACTN))){
  # Subset nearest time series
  SACTN <- droplevels(SACTN_cropped[as.character(SACTN_cropped$site) == as.character(SACTN_SAWS_nearest$SACTN)[i],])
  SAWS <- droplevels(SAWS_homogenised[as.character(SAWS_homogenised$site) == as.character(SACTN_SAWS_nearest$SAWS)[i],])
  # Correct columns to match
  # SACTN$depth <- -SACTN$depth
  SACTN <- SACTN[,c(1,4,5)]
  SACTN$dataset <- "SACTN"
  # colnames(SACTN)[4] <- "elev"
  SAWS <- SAWS[,c(1,2,5)]
  SAWS$dataset <- "SAWS"
  SAWS <- SAWS[SAWS$date %in% SACTN$date,]
  SACTN <- SACTN[SACTN$date %in% SAWS$date,]
  match <- rbind(SACTN, SAWS)
  match$facet <- paste(SACTN$site[1], SAWS$site[1], sep = " - ")
  match$R2 <-  round(coef(lm(SACTN$temp~SAWS$temp))[2],2)
  match$R22 <- paste0("R^2 ==", format(match$R2, digits=2))
  SAWS_SACTN_match  <- rbind(SAWS_SACTN_match , match)
}


# 3. Save as "data/SACTN_SAWS_match.Rdata" --------------------------------

save(SAWS_SACTN_match, file = "data/SAWS_SACTN_match.Rdata")
