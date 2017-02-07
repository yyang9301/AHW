#############################################################################
###"load/load.BRAN.R"
## This script does:
# 1. Create a function for loading the BRAN data
# 2. Load the BRAN data
# 3. Merge and save
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

cbind(rep(lat, each=length(lon)), rep(lon,length(lat)), c(myVariable))