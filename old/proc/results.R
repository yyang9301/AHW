#############################################################################
###"proc/results.R"
## This script does:
# 1. Load co-occurrence results
# 2. Calculates metrics from co-occurrence results
# 3. Calculate change in co-occurrence over distance
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(stringr)
library(zoo)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(broom)
library(tibble)
library(purrr)
## USED BY:
# "graph/figures.R"
## CREATES:
# 
#############################################################################


# 1. Load co-occurrence results -------------------------------------------

# 1) SACTN - SACTN
# Heat waves
load("data/cooccurrence/SACTN_SACTN_hw_tmean_CO.Rdata")
# Cold-spells
load("data/cooccurrence/SACTN_SACTN_cs_tmean_CO.Rdata")


# 2) SAWS - SAWS
# Heat waves
load("data/cooccurrence/SAWS_SAWS_hw_tmean_CO.Rdata")
load("data/cooccurrence/SAWS_SAWS_hw_tmax_CO.Rdata")
load("data/cooccurrence/SAWS_SAWS_hw_tmin_CO.Rdata")
# Cold-spells
load("data/cooccurrence/SAWS_SAWS_cs_tmean_CO.Rdata")
load("data/cooccurrence/SAWS_SAWS_cs_tmax_CO.Rdata")
load("data/cooccurrence/SAWS_SAWS_cs_tmin_CO.Rdata")

# 3) SACTN - SAWS
# Heat waves
load("data/cooccurrence/SACTN_SAWS_hw_tmean_CO.Rdata")
load("data/cooccurrence/SACTN_SAWS_hw_tmax_CO.Rdata")
load("data/cooccurrence/SACTN_SAWS_hw_tmin_CO.Rdata")
# Cold-spells
load("data/cooccurrence/SACTN_SAWS_cs_tmean_CO.Rdata")
load("data/cooccurrence/SACTN_SAWS_cs_tmax_CO.Rdata")
load("data/cooccurrence/SACTN_SAWS_cs_tmin_CO.Rdata")


# 2. Calculates metrics from co-occurrence results ------------------------

# Function for calculating stats from co-occurrence results
x <- hw_tmean_CO
stats.CO <- function(x){
  x1 <- ddply(x)
}

# Highest co-occurrence rates per percentile
hw_tmax_stats <- hw_tmax_CO

