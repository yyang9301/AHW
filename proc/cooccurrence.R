#############################################################################
###"proc/coocurrence.R"
## This script does:
# 1. Load events and indices
# 2. Compare rates of co-occurrence for matched sites
# 3. Add distance and coast column to results and save
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

# SAWS events
load("data/SAWS_events_tmean.Rdata")
SAWS_events_tmean$SACTN <- sapply(strsplit(SAWS_events_tmean$index, " - "), "[[", 2)
load("data/SAWS_events_tmax.Rdata")
SAWS_events_tmax$SACTN <- sapply(strsplit(SAWS_events_tmax$index, " - "), "[[", 2)
load("data/SAWS_events_tmin.Rdata")
SAWS_events_tmin$SACTN <- sapply(strsplit(SAWS_events_tmin$index, " - "), "[[", 2)

# SACTN events
load("data/SACTN_events.Rdata")

# Other indices
load("setupParams/distances_SACTN.Rdata")
load("setupParams/SACTN_site_list.Rdata")
load("setupParams/SAWS_site_list.Rdata")

# Manually add coast values to SAWS site list
# NB: THese have been decided based on location and are questionable
# Sites around Cape Point could be either coast but are labeled here as "wc" for balance
SAWS_site_list$coast <- c("wc","wc","wc","sc","sc","sc","sc","sc","ec","ec","ec")
SACTN_site_list$coast <- as.character(SACTN_site_list$coast)

# 2. Compare rates of co-occurrence for matched sites ---------------------

## NB: It has been decided that all of these functions must be fed either heat wave or cold spell data frames
## Not data frames containing both types of events

# x <- SACTN_events[SACTN_events$site == levels(as.factor(SACTN_events$site))[5],]
# x <- x[x$type == "MHW",]
# y <- SAWS_events_tmean[SAWS_events_tmean$SACTN == x$site[1],]
# y <- y[y$type == "AHW",]

# Function that takes a dataframe (x) and removes any non-overlapping years found in another dataframe (y)
year.crop <- function(x,y){
  x$yearStrt <- year(x$date_start)
  y_yearStrt <- unique(year(y$date_start))
  x <- x[x$yearStrt >= min(y_yearStrt), ]
  x <- x[x$yearStrt <= max(y_yearStrt), ]
  x$yearStrt <- NULL
  return(x)
}

# test_x <- year.crop(x,y)
# test_y <- year.crop(y, test_x)

# Function that finds the size (i.e. percentile) of events (q) in a dataframe (x) and subsets accordingly
size.crop <- function(x,q){ # 0.0 = all events, 0.5 = half of the events, 1.0 = only the largest event
  x <- x[x$int_cum >= quantile(x$int_cum, probs = q),]
}

# test_x <- size.crop(test_x, 0.5)
# test_y <- size.crop(test_y, 0.5)


# Function that labels the percentile of the event (x = int_cum)
# Designed to be run with group_by(site)
percentile.label <- function(x){
  quants <- quantile(x) # Can add the "probs =" argument here to change the percentiles calculated but must change the other labels accordingly
  y <- rep(NA, length(x))
  y[x >= quants[1]] <- 0 # Change these manual labels if you change the quantiles to be calculated
  y[x >= quants[2]] <- 25
  y[x >= quants[3]] <- 50
  y[x >= quants[4]] <- 75
  y[x >= quants[5]] <- 100
  return(y)
}

# x <- filter(SACTN_events, site == levels(as.factor(SACTN_events$site))[1:2], type == "MHW")
# x <- x %>% 
#   group_by(site) %>%
#   mutate(percentile = percentile.label(int_cum))


# Function that finds events overlapping within a certain period of time for two dataframes (x, y)
# It is expected that (x) is marine and (y) is atmosphere
# It is also expected that (y) has been subsetted to match (x)
# Meaning x$site == y$SACTN

# x <- SACTN_events[SACTN_events$site == levels(as.factor(SACTN_events$site))[7],]
# y <- SAWS_events_tmean[SAWS_events_tmean$SACTN == x$site[1],]
# x <- x[x$type == "MHW",]
# y <- SAWS_events_tmean[SAWS_events_tmean$SACTN == x$site[1],]
# y <- y[y$type == "AHW",]
# 
# x <- year.crop(x,y)
# y <- year.crop(y,x)
# 
# x <- size.crop(x, 0.5)
# y <- size.crop(y, 0.5)

event.match <- function(x,y){
  y <- ddply(y, .(index), mutate, n = length(index)) # The total number of events being compared
  y$ply_index <- seq(1:length(y$index)) # Allows ddply to analyse each individual SAWS event against the SACTN dataframe
  y$percentile <- min(y$percentile, na.rm = T)
  y <- ddply(y, .(ply_index), mutate,
             b_14 = length(x$date_start[x$date_start %in% seq((date_start-14), date_start, 1)]), # Does the SACTN event occur within 14 days BEFORE the SAWS event?
             b_10 = length(x$date_start[x$date_start %in% seq((date_start-10), date_start, 1)]),
             b_06 = length(x$date_start[x$date_start %in% seq((date_start-6), date_start, 1)]),
             b_02 = length(x$date_start[x$date_start %in% seq((date_start-2), date_start, 1)]),
             a_02 = length(x$date_start[x$date_start %in% seq(date_start, (date_start+2),  1)]),
             a_06 = length(x$date_start[x$date_start %in% seq(date_start, (date_start+6), 1)]),
             a_10 = length(x$date_start[x$date_start %in% seq(date_start, (date_start+10), 1)]),
             a_14 = length(x$date_start[x$date_start %in% seq(date_start, (date_start+14), 1)])) # Does the SACTN event occur within 14 days AFTER the SAWS event?
  results <- ddply(y, .(index, site, SACTN, n, percentile), summarise,
             b_14 = sum(b_14),
             b_10 = sum(b_10),
             b_06 = sum(b_06),
             b_02 = sum(b_02),
             a_02 = sum(a_02),
             a_06 = sum(a_06),
             a_10 = sum(a_10),
             a_14 = sum(a_14))
  colnames(results)[2] <- "SAWS"
  return(results)
}



# Function that extracts the correct SAWS sites based on the SACTN site
# It then runs all of the functions created above, calculates and returns the results
# NB: This function draws on an "ahw" variable from outside of this function
# This is generated manually so as to allow a choice of tmean, tmin or tmax

# mhw <- filter(SACTN_events, type == "MHW")
# ahw <- filter(SAWS_events_tmean, type == "AHW")
# 
x <- filter(SACTN_events, site == levels(as.factor(SACTN_events$site))[1], type == "MHW")
y <- filter(SAWS_events_tmean, type == "AHW")

cooccurrence <- function(x, y){
  # 1) Subset SAWS data to match SACTN site
  y <- filter(y, SACTN == x$site[1]) # NB: Must manually set "ahw" or "acs" outside of this function
  # 2) Crop the years to have the same time span
  x <- year.crop(x,y)
  y <- year.crop(y,x)
  # 3) Add percentile labels
  x$percentile <- percentile.label(x$int_cum)
  y <- y %>% 
    group_by(index) %>%
    mutate(percentile = percentile.label(int_cum))
  # 4) Match events based on percentile labels
  event_00 <- event.match(x,y)
  event_25 <- event.match(filter(x, percentile >= 25), filter(y, percentile >= 25))
  event_50 <- event.match(filter(x, percentile >= 50), filter(y, percentile >= 50))
  event_75 <- event.match(filter(x, percentile >= 75), filter(y, percentile >= 75))
  event_100 <- event.match(filter(x, percentile >= 100), filter(y, percentile >= 100))
  events <- rbind(event_00, event_25, event_50, event_75, event_100)
  return(events)
}

# mhw <- filter(SACTN_events, type == "MHW")
# x <- filter(mhw, site == "Bordjies Deep")
# test <- cooccurrence(x)

# 1) Subset data
mhw <- filter(SACTN_events, type == "MHW")
mcs <- filter(SACTN_events, type == "MCS")
ahw_tmean <- filter(SAWS_events_tmean, type == "AHW")
acs_tmean <- filter(SAWS_events_tmean, type == "ACS")
ahw_tmax <- filter(SAWS_events_tmax, type == "AHW")
acs_tmax <- filter(SAWS_events_tmax, type == "ACS")
ahw_tmin <- filter(SAWS_events_tmin, type == "AHW")
acs_tmin <- filter(SAWS_events_tmin, type == "ACS")

# 2) Run it all and save

# Heat waves
system.time(hw_tmean_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmean, .parallel = TRUE)) ## ~130 seconds
system.time(hw_tmax_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmax, .parallel = TRUE))
system.time(hw_tmin_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmin, .parallel = TRUE))
# Cold spells
system.time(cs_tmean_CO <- ddply(mcs, .(site), cooccurrence, acs_tmean, .parallel = TRUE)) ## ~154 seconds
system.time(cs_tmax_CO <- ddply(mcs, .(site), cooccurrence, acs_tmax, .parallel = TRUE))
system.time(cs_tmin_CO <- ddply(mcs, .(site), cooccurrence, acs_tmin, .parallel = TRUE))


# 3. Add distance and coast column to results and save --------

# Melt and prep distance matrix
distances_SACTN_melt <- melt(distances_SACTN)
distances_SACTN_melt$index <- paste(distances_SACTN_melt$variable, distances_SACTN_melt$SACTN, sep = " - ")
distances_SACTN_melt$value <- round_any(distances_SACTN_melt$value, 0.01)

# Function to add distance and coast columns
x <- hw_tmean_CO
add.indices <- function(x){ # Ignore warnings... Upvote hypno toad
  x$site <- NULL # Remove the artifact left over from ddply
  x <- x %>% 
    group_by(index) %>% 
    mutate(dist = distances_SACTN_melt$value[distances_SACTN_melt$index == index][1])
  x <- x %>% 
    group_by(SAWS) %>%
    mutate(SAWS_coast = SAWS_site_list$coast[SAWS_site_list$site == SAWS[1]])
  x <- x %>% 
    group_by(SACTN) %>%
    mutate(SACTN_coast = SACTN_site_list$coast[SACTN_site_list$site == SACTN[1]])
  x <- data.frame(x)
  x$coast_index <- paste(x$SAWS_coast, x$SACTN_coast, sep = " - ")
  return(x)
}
test <- add.indices(hw_tmean_CO)


# Heat waves
hw_tmean_CO <- add.indices(hw_tmean_CO)
save(hw_tmean_CO, file = "data/hw_tmean_CO.Rdata")
##
hw_tmax_CO <- add.indices(hw_tmax_CO)
save(hw_tmax_CO, file = "data/hw_tmax_CO.Rdata")
##
hw_tmin_CO <- add.indices(hw_tmin_CO)
save(hw_tmin_CO, file = "data/hw_tmin_CO.Rdata")

# Cold-spells
cs_tmean_CO <- add.indices(cs_tmean_CO)
save(cs_tmean_CO, file = "data/cs_tmean_CO.Rdata")
##
cs_tmax_CO <- add.indices(cs_tmax_CO)
save(cs_tmax_CO, file = "data/cs_tmax_CO.Rdata")
##
cs_tmin_CO <- add.indices(cs_tmin_CO)
save(cs_tmin_CO, file = "data/cs_tmin_CO.Rdata")

