#############################################################################
###"proc/coocurrence.R"
## This script does:
# 1. Load events
# 2. Compare rates of co-occurrence between datasets and save
# 3. Compare rates of co-occurrence within datasets and save
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(FNN)
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
source("func/cooccurrence.func.R")
## USED BY:
# "proc/results.R"
## CREATES:
# "data/SAWS_SACTN_hw_tmean_CO.Rdata"
# "data/SAWS_SACTN_hw_tmax_CO.Rdata"
# "data/SAWS_SACTN_hw_tmin_CO.Rdata"
# "data/SAWS_SACTN_cs_tmean_CO.Rdata"
# "data/SAWS_SACTN_cs_tmax_CO.Rdata"
# "data/SAWS_SACTN_cs_tmin_CO.Rdata"
#############################################################################


# 1. Load events -----------------------------------------


# SAWS_SAWS events
load("data/events/SAWS_SAWS_events_tmean.Rdata")
load("data/events/SAWS_SAWS_events_tmax.Rdata")
load("data/events/SAWS_SAWS_events_tmin.Rdata")


# SAWS_SACTN events
load("data/events/SAWS_SACTN_events_tmean.Rdata")
SAWS_SACTN_events_tmean$SACTN <- sapply(strsplit(as.character(SAWS_SACTN_events_tmean$index), " - "), "[[", 2)
load("data/events/SAWS_SACTN_events_tmax.Rdata")
SAWS_SACTN_events_tmax$SACTN <- sapply(strsplit(as.character(SAWS_SACTN_events_tmax$index), " - "), "[[", 2)
load("data/events/SAWS_SACTN_events_tmin.Rdata")
SAWS_SACTN_events_tmin$SACTN <- sapply(strsplit(as.character(SAWS_SACTN_events_tmin$index), " - "), "[[", 2)


# SACTN events
load("data/events/SACTN_events.Rdata")


# 2. Compare rates of co-occurrence between datasets ----------------------


## NB: Only run one co-occurrence analysis at a time
## Due to the parallel nature of the functions, queing up several to run may cause ones computer to hang/ freeze


# 1) Subset data
mhw <- filter(SACTN_events, type == "MHW")
mcs <- filter(SACTN_events, type == "MCS")
ahw_tmean <- filter(SAWS_SACTN_events_tmean, type == "AHW")
acs_tmean <- filter(SAWS_SACTN_events_tmean, type == "ACS")
ahw_tmax <- filter(SAWS_SACTN_events_tmax, type == "AHW")
acs_tmax <- filter(SAWS_SACTN_events_tmax, type == "ACS")
ahw_tmin <- filter(SAWS_SACTN_events_tmin, type == "AHW")
acs_tmin <- filter(SAWS_SACTN_events_tmin, type == "ACS")

# 2) Calculate co-occurrence

# Heat waves
    ## NB: If comparing events between datsets the primary data frame must be the SACTN data
system.time(SACTN_SAWS_hw_tmean_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmean, compare = "between", .parallel = TRUE)) ## ~70 seconds
save(SACTN_SAWS_hw_tmean_CO, file = "data/cooccurrence/SACTN_SAWS_hw_tmean_CO.Rdata")
system.time(SACTN_SAWS_hw_tmax_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmax, compare = "between", .parallel = TRUE))
save(SACTN_SAWS_hw_tmax_CO, file = "data/cooccurrence/SACTN_SAWS_hw_tmax_CO.Rdata")
system.time(SACTN_SAWS_hw_tmin_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmin, compare = "between", .parallel = TRUE))
save(SACTN_SAWS_hw_tmin_CO, file = "data/cooccurrence/SACTN_SAWS_hw_tmin_CO.Rdata")
# Cold spells
system.time(SACTN_SAWS_cs_tmean_CO <- ddply(mcs, .(site), cooccurrence, acs_tmean, compare = "between", .parallel = TRUE)) ## ~120 seconds
save(SACTN_SAWS_cs_tmean_CO, file = "data/cooccurrence/SACTN_SAWS_cs_tmean_CO.Rdata")
system.time(SACTN_SAWS_cs_tmax_CO <- ddply(mcs, .(site), cooccurrence, acs_tmax, compare = "between", .parallel = TRUE))
save(SACTN_SAWS_cs_tmax_CO, file = "data/cooccurrence/SACTN_SAWS_cs_tmax_CO.Rdata")
system.time(SACTN_SAWS_cs_tmin_CO <- ddply(mcs, .(site), cooccurrence, acs_tmin, compare = "between", .parallel = TRUE))
save(SACTN_SAWS_cs_tmin_CO, file = "data/cooccurrence/SACTN_SAWS_cs_tmin_CO.Rdata")


# 3. Compare rates of co-occurrence within datasets --------------------------


## NB: Only run one co-occurrence analysis at a time
## Due to the parallel nature of the functions, queing up several to run may cause ones computer to hang/ freeze


# 1) Subset data
mhw <- filter(SACTN_events, type == "MHW")
mcs <- filter(SACTN_events, type == "MCS")
ahw_tmean <- filter(SAWS_SAWS_events_tmean, type == "AHW")
acs_tmean <- filter(SAWS_SAWS_events_tmean, type == "ACS")
ahw_tmax <- filter(SAWS_SAWS_events_tmax, type == "AHW")
acs_tmax <- filter(SAWS_SAWS_events_tmax, type == "ACS")
ahw_tmin <- filter(SAWS_SAWS_events_tmin, type == "AHW")
acs_tmin <- filter(SAWS_SAWS_events_tmin, type == "ACS")

# 2) Calculate co-occurrence

# 2.1) SACTN
# Heatwaves
system.time(SACTN_SACTN_hw_tmean_CO <- ddply(mhw, .(site), cooccurrence, mhw, compare = "within", .parallel = TRUE)) ## ~60 seconds
save(SACTN_SACTN_hw_tmean_CO, file = "data/cooccurrence/SACTN_SACTN_hw_tmean_CO.Rdata")
# Cold-spells
system.time(SACTN_SACTN_cs_tmean_CO <- ddply(mcs, .(site), cooccurrence, mcs, compare = "within", .parallel = TRUE)) ## ~50 seconds
save(SACTN_SACTN_cs_tmean_CO, file = "data/cooccurrence/SACTN_SACTN_cs_tmean_CO.Rdata")

# 2.2) SAWS
# Heatwaves
system.time(SAWS_SAWS_hw_tmean_CO <- ddply(ahw_tmean, .(site), cooccurrence, ahw_tmean, compare = "within", .parallel = TRUE)) ## ~60 seconds
save(SAWS_SAWS_hw_tmean_CO, file = "data/cooccurrence/SAWS_SAWS_hw_tmean_CO.Rdata")
system.time(SAWS_SAWS_hw_tmax_CO <- ddply(ahw_tmax, .(site), cooccurrence, ahw_tmax, compare = "within", .parallel = TRUE))
save(SAWS_SAWS_hw_tmax_CO, file = "data/cooccurrence/SAWS_SAWS_hw_tmax_CO.Rdata")
system.time(SAWS_SAWS_hw_tmin_CO <- ddply(ahw_tmin, .(site), cooccurrence, ahw_tmin, compare = "within", .parallel = TRUE))
save(SAWS_SAWS_hw_tmin_CO, file = "data/cooccurrence/SAWS_SAWS_hw_tmin_CO.Rdata")
# Cold-spells
system.time(SAWS_SAWS_cs_tmean_CO <- ddply(acs_tmean, .(site), cooccurrence, acs_tmean, compare = "within", .parallel = TRUE)) ## ~140 seconds
save(SAWS_SAWS_cs_tmean_CO, file = "data/cooccurrence/SAWS_SAWS_cs_tmean_CO.Rdata")
system.time(SAWS_SAWS_cs_tmax_CO <- ddply(acs_tmax, .(site), cooccurrence, acs_tmax, compare = "within", .parallel = TRUE))
save(SAWS_SAWS_cs_tmax_CO, file = "data/cooccurrence/SAWS_SAWS_cs_tmax_CO.Rdata")
system.time(SAWS_SAWS_cs_tmin_CO <- ddply(acs_tmin, .(site), cooccurrence, acs_tmin, compare = "within", .parallel = TRUE))
save(SAWS_SAWS_cs_tmin_CO, file = "data/cooccurrence/SAWS_SAWS_cs_tmin_CO.Rdata")

