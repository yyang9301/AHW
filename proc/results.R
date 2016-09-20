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

# Heat waves
load("data/hw_tmean_CO.Rdata")
load("data/hw_tmax_CO.Rdata")
load("data/hw_tmin_CO.Rdata")
# Cold-spells
load("data/cs_tmean_CO.Rdata")
load("data/cs_tmax_CO.Rdata")
load("data/cs_tmin_CO.Rdata")


# 2. Calculates metrics from co-occurrence results ------------------------

# Function for calculating stats from co-occurrence results
x <- hw_tmean_CO
stats.CO <- function(x){
  x1 <- ddply(x)
}

# Highest co-occurrence rates per percentile
hw_tmax_stats <- hw_tmax_CO

