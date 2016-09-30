#############################################################################
###"proc/coocurrence.R"
## This script does:
# 1. Load events and indices
# 2. Compare rates of co-occurrence between datasets
# 3. Compare rates of co-occurrence within datasets
# 4. Add distance, bearing and coast columns to results and save
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
# "data/hw_tmean_CO.Rdata"
# "data/hw_tmax_CO.Rdata"
# "data/hw_tmin_CO.Rdata"
# "data/cs_tmean_CO.Rdata"
# "data/cs_tmax_CO.Rdata"
# "data/cs_tmin_CO.Rdata"
#############################################################################


# 1. Load events and indices -----------------------------------------


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

# Other indices
load("setupParams/distances_bearings.Rdata")
# distances_bearings$site1 <- sapply(strsplit(as.character(distances_bearings$index), " - "), "[[", 1)
# distances_bearings$site2 <- sapply(strsplit(as.character(distances_bearings$index), " - "), "[[", 2)
load("setupParams/distances_SACTN.Rdata")
load("setupParams/SACTN_site_list.Rdata")
load("setupParams/SAWS_site_list.Rdata")


# Manually add coast values to SAWS site list
# NB: THese have been decided based on location and are questionable
# Sites around Cape Point could be either coast but are labeled here as "wc" for balance
SAWS_site_list$coast <- c("wc","wc","wc","sc","sc","sc","sc","sc","ec","ec","ec")
SACTN_site_list$coast <- as.character(SACTN_site_list$coast)


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
system.time(SACTN_SAWS_hw_tmean_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmean, compare = "between", .parallel = TRUE)) ## ~60 seconds
system.time(SACTN_SAWS_hw_tmax_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmax, compare = "between", .parallel = TRUE))
system.time(SACTN_SAWS_hw_tmin_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmin, compare = "between", .parallel = TRUE))
# Cold spells
system.time(SACTN_SAWS_cs_tmean_CO <- ddply(mcs, .(site), cooccurrence, acs_tmean, compare = "between", .parallel = TRUE)) ## ~100 seconds
system.time(SACTN_SAWS_cs_tmax_CO <- ddply(mcs, .(site), cooccurrence, acs_tmax, compare = "between", .parallel = TRUE))
system.time(SACTN_SAWS_cs_tmin_CO <- ddply(mcs, .(site), cooccurrence, acs_tmin, compare = "between", .parallel = TRUE))


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
# Cold-spells
system.time(SACTN_SACTN_cs_tmean_CO <- ddply(mcs, .(site), cooccurrence, mcs, compare = "within", .parallel = TRUE)) ## ~xx seconds

# 2.2) SAWS
# Heatwaves
system.time(SAWS_SAWS_hw_tmean_CO <- ddply(ahw_tmean, .(site), cooccurrence, ahw_tmean, compare = "within", .parallel = TRUE)) ## ~xx seconds
system.time(SAWS_SAWS_hw_tmax_CO <- ddply(ahw_tmax, .(site), cooccurrence, ahw_tmax, compare = "within", .parallel = TRUE))
system.time(SAWS_SAWS_hw_tmin_CO <- ddply(ahw_tmin, .(site), cooccurrence, ahw_tmin, compare = "within", .parallel = TRUE))
# Cold-spells
system.time(SAWS_SAWS_cs_tmean_CO <- ddply(acs_tmean, .(site), cooccurrence, acs_tmean, compare = "within", .parallel = TRUE)) ## ~xx seconds
system.time(SAWS_SAWS_cs_tmax_CO <- ddply(acs_tmax, .(site), cooccurrence, acs_tmax, compare = "within", .parallel = TRUE))
system.time(SAWS_SAWS_cs_tmin_CO <- ddply(acs_tmin, .(site), cooccurrence, acs_tmin, compare = "within", .parallel = TRUE))


# 4. Add distance, bearing and coast columns to results ---------------

# Melt and prep distance matrix
# distances_SACTN_melt <- melt(distances_SACTN)
# distances_SACTN_melt$index <- paste(distances_SACTN_melt$variable, distances_SACTN_melt$SACTN, sep = " - ")
# distances_SACTN_melt$value <- round_any(distances_SACTN_melt$value, 0.01)

# Function to add distance and coast columns
# x <- SACTN_SAWS_hw_tmean_CO ## Tester
add.indices <- function(x){ # Ignore warnings... Upvote hypno toad
  x$site <- NULL # Remove the artifact left over from ddply
  x <- x %>% 
    group_by(index.2) %>% 
    mutate(dist = distances_bearings$dist[distances_bearings$index == index.2][1]) %>% 
    mutate(bear = distances_bearings$bear[distances_bearings$index == index.2][1])
  x <- x %>% 
    group_by(site) %>%
    mutate(site1_coast = SAWS_site_list$coast[SAWS_site_list$site == SAWS[1]])
  x <- x %>% 
    group_by(SACTN) %>%
    mutate(SACTN_coast = SACTN_site_list$coast[SACTN_site_list$site == SACTN[1]])
  x <- data.frame(x)
  x$coast_index <- paste(x$SAWS_coast, x$SACTN_coast, sep = " - ")
  return(x)
}
# test <- add.indices(hw_tmean_CO)


hw_tmean_CO <- add.indices(hw_tmean_CO)
hw_tmax_CO <- add.indices(hw_tmax_CO)
hw_tmin_CO <- add.indices(hw_tmin_CO)
cs_tmean_CO <- add.indices(cs_tmean_CO)
cs_tmax_CO <- add.indices(cs_tmax_CO)
cs_tmin_CO <- add.indices(cs_tmin_CO)

# # 5. Save ---------------------------------------------------------------

# 1) Between datast co-occurrence results

# Heat waves

save(hw_tmean_CO, file = "data/hw_tmean_CO.Rdata")
##

save(hw_tmax_CO, file = "data/hw_tmax_CO.Rdata")
##

save(hw_tmin_CO, file = "data/hw_tmin_CO.Rdata")

# Cold-spells

save(cs_tmean_CO, file = "data/cs_tmean_CO.Rdata")
##

save(cs_tmax_CO, file = "data/cs_tmax_CO.Rdata")
##

save(cs_tmin_CO, file = "data/cs_tmin_CO.Rdata")
