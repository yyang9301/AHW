#############################################################################
###"proc/coocurrence.R"
## This script does:
# 1. Load events
# 2. Compare rates of co-occurrence for matched sites
# 3. Compute other metrics of comparison such as change over distance
# 4. Create figures showing results
# 5. Save
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
source("setupParams/theme.R")
## USED BY:
# 
## CREATES:
# 
#############################################################################


# 1. Load events and iinices -----------------------------------------

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

# 2. Create index of co-occurrence variables ------------------------------

## A data frame that contains all possible comparison criteria
# Direction
# Lag
# Size

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
  quants <- quantile(x) # Can add the "probs =" argument here to change the percentiles calculated 
  y <- rep(NA, length(x))
  y[x >= quants[1]] <- 0
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
  y$ply_index <- seq(1:length(y$index))
  y$percentile <- min(y$percentile, na.rm = T)
  y <- ddply(y, .(ply_index), mutate,
             b_14 = length(x$date_start[x$date_start %in% seq((date_start-14), date_start, 1)]),
             b_10 = length(x$date_start[x$date_start %in% seq((date_start-10), date_start, 1)]),
             b_06 = length(x$date_start[x$date_start %in% seq((date_start-6), date_start, 1)]),
             b_02 = length(x$date_start[x$date_start %in% seq((date_start-2), date_start, 1)]),
             a_02 = length(x$date_start[x$date_start %in% seq(date_start, (date_start+2),  1)]),
             a_06 = length(x$date_start[x$date_start %in% seq(date_start, (date_start+6), 1)]),
             a_10 = length(x$date_start[x$date_start %in% seq(date_start, (date_start+10), 1)]),
             a_14 = length(x$date_start[x$date_start %in% seq(date_start, (date_start+14), 1)]))
  y <- ddply(y, .(index), mutate,
             b_14 = sum(b_14)/length(b_14),
             b_10 = sum(b_10)/length(b_10),
             b_06 = sum(b_06)/length(b_06),
             b_02 = sum(b_02)/length(b_02),
             a_02 = sum(a_02)/length(a_02),
             a_06 = sum(a_06)/length(a_06),
             a_10 = sum(a_10)/length(a_10),
             a_14 = sum(a_14)/length(a_14))
  results <- distinct(select(y, index, SACTN, site, percentile, b_14, b_10, b_06, b_02, a_02, a_06, a_10, a_14))
  results[5:12] <- apply(results[5:12], 1, round_any, 0.01)
  return(results)
}



# Function that extracts the correct SAWS sites based on the SACTN site
# It then runs all of the functions created above, calculates and returns the results
# NB: This function draws on an "ahw" variable from outside of this function
# This is generated manually so as to allow a choice of tmean, tmin or tmax

# mhw <- filter(SACTN_events, type == "MHW")
# ahw <- filter(SAWS_events_tmean, type == "AHW")
# 
# x <- filter(SACTN_events, site == levels(as.factor(SACTN_events$site))[3], type == "MHW")

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
system.time(hw_tmean_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmean, .parallel = TRUE)) ## ~80 seconds
save(hw_tmean_CO, file = "data/hw_tmean_CO.Rdata")
system.time(hw_tmax_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmax, .parallel = TRUE))
save(hw_tmax_CO, file = "data/hw_tmax_CO.Rdata")
system.time(hw_tmin_CO <- ddply(mhw, .(site), cooccurrence, ahw_tmin, .parallel = TRUE))
save(hw_tmin_CO, file = "data/hw_tmin_CO.Rdata")
# Cold spells
system.time(cs_tmean_CO <- ddply(mcs, .(site), cooccurrence, acs_tmean, .parallel = TRUE)) ## ~154 seconds
save(cs_tmean_CO, file = "data/cs_tmean_CO.Rdata")
system.time(cs_tmax_CO <- ddply(mcs, .(site), cooccurrence, acs_tmax, .parallel = TRUE))
save(cs_tmax_CO, file = "data/cs_tmax_CO.Rdata")
system.time(cs_tmin_CO <- ddply(mcs, .(site), cooccurrence, acs_tmin, .parallel = TRUE))
save(cs_tmin_CO, file = "data/cs_tmin_CO.Rdata")


# 3. Compute other metrics of comparison such as change over distance --------


# 4. Create figures showing results ------------------------------------------

#Prepares data for plotting

# mhwCO$site <- factor(mhwCO$site, levels = siteOrder)
mhw_CO_5_2$direction <- factor(mhw_CO_5_2$direction, levels = c("b", "x", "a"))
mhw_CO_5_2$index <- paste(mhw_CO_5_2$lag, mhw_CO_5_2$direction, sep = "_")
mhw_CO_3_1$direction <- factor(mhw_CO_3_1$direction, levels = c("b", "x", "a"))
mhw_CO_3_1$index <- paste(mhw_CO_3_1$lag, mhw_CO_3_1$direction, sep = "_")
# mcsCO$site <- factor(mcsCO$site, levels = siteOrder)
mcs_CO_5_2$direction <- factor(mcs_CO_5_2$direction, levels = c("b", "x", "a"))
mcs_CO_5_2$index <- paste(mcs_CO_5_2$lag, mcs_CO_5_2$direction, sep = "_")
mcs_CO_3_1$direction <- factor(mcs_CO_3_1$direction, levels = c("b", "x", "a"))
mcs_CO_3_1$index <- paste(mcs_CO_3_1$lag, mcs_CO_3_1$direction, sep = "_")

cooccurrenceQuantFigure <- function(dat){
  p2 <- ggplot(data = dat, aes(x = quantile, y = proportion)) + bw_update +
    geom_line(aes(colour = as.factor(index))) +
    geom_point(aes(colour = as.factor(index))) +
    facet_grid(facet ~ direction) +
    ylab("proportion") + xlab("percentile (%)") #+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  p2
}

mhw_CO_5_2_fig <- cooccurrenceQuantFigure(mhw_CO_5_2)
ggsave("graph/mhw_CO_5_2.pdf", width = 16, height = 30)
mhw_CO_3_1_fig <- cooccurrenceQuantFigure(mhw_CO_3_1)
ggsave("graph/mhw_CO_3_1.pdf", width = 16, height = 30)
mcs_CO_5_2_fig <- cooccurrenceQuantFigure(mcs_CO_5_2)
ggsave("graph/mcs_CO_5_2.pdf", width = 16, height = 30)
mcs_CO_3_1_fig <- cooccurrenceQuantFigure(mcs_CO_3_1)
ggsave("graph/mcs_CO_3_1.pdf", width = 16, height = 30)


# 5. Save -----------------------------------------------------------------


