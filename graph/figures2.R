#############################################################################
###"graph/figures2.R"
## This script does:
# 1. Load co-occurrence results
# 2. Create data frames for specific conditions
# 3. Create figures
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(stringr)
library(zoo)
library(lubridate)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(broom)
library(tibble)
library(purrr)
source("setupParams/theme.R")
## USED BY:
# 
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


# # 2. Create density plots -----------------------------------------------

ggplot(SACTN_SAWS_hw_tmean_CO, aes(x = latest)) + bw_update + # Lay plot foundation
  geom_density(fill = "grey", colour = "black", alpha = 0.6) +
  labs(x = "Days between latest events", y = "Frequency")

ggplot(SACTN_SAWS_hw_tmean_CO[abs(SACTN_SAWS_hw_tmean_CO$latest) <= 730,], aes(x = latest)) + bw_update + # Lay plot foundation
  geom_density(fill = "grey", colour = "black", alpha = 0.6) +
  labs(x = "Days between latest events", y = "Frequency")

ggplot(SACTN_SAWS_hw_tmean_CO[abs(SACTN_SAWS_hw_tmean_CO$latest) <= 365,], aes(x = latest)) + bw_update + # Lay plot foundation
  geom_density(fill = "brown", colour = "black", alpha = 0.6) +
  labs(x = "Days between latest events", y = "Frequency")

ggplot(SACTN_SAWS_hw_tmean_CO[abs(SACTN_SAWS_hw_tmean_CO$latest) <= 31,], aes(x = latest)) + bw_update + # Lay plot foundation
  geom_density(fill = "salmon", colour = "black", alpha = 0.6) +
  labs(x = "Days between latest events", y = "Frequency")

ggplot(SACTN_SAWS_hw_tmean_CO[abs(SACTN_SAWS_hw_tmean_CO$latest) <= 7,], aes(x = latest)) + bw_update + # Lay plot foundation
  geom_density(fill = "goldenrod", colour = "black", alpha = 0.6) +
  labs(x = "Days between latest events", y = "Frequency")
