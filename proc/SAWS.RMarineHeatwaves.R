#############################################################################
###"proc/SAWS.RMarineHeatwaves.R"
## This script does:
# 1. Load the SAWS data and site lists
# 2. Calculate heatwaves/ cold-spells for the SAWS data
# 3. Save the results
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
# 
#############################################################################


# 1. Load the SAWS data and site lists ---------------

# SAWS
load("data/SAWS/homogenised/SAWS_homogenised.Rdata")
load("setupParams/SAWS_site_list.Rdata")


# 2. Calculate heatwaves/ cold-spells for the SAWS data and load t --------
## This function must be given t = time and temp = temperature columns, same as make_whole
## It then does all of the necessary calculations dynamically for the time series given
## This is done so it can be run in a for loop
## Dplyr would be ideal but the output of detect() complicates this
detect.full <- function(dat, dur, gap, pctile){
  site <- dat$site[1]
  dat2 <- dat
  dat2$site <- NULL
  whole <- make_whole(dat2)
  # full_year_start <- year(whole$date[1])+1
  # full_year_end <- year(whole$date[length(whole$date)])-1
  results <- detect(whole, climatology_start = 1981, climatology_end = 2010,
                    min_duration = dur, max_gap = gap, pctile = pctile)
  results$clim$site <- site
  results$event$site <- site
  return(results)
}

levels(SAWS_homogenised$site)

# A tester to ensure the function functions as intended
test <- droplevels(SAWS_homogenised[as.character(SAWS_homogenised$site) == "Cape Agulhas",])
test <- test[,c(1,2,5)]
colnames(test) <- c("site", "t", "temp")
test2 <- detect.full(test, 3, 1, 90)
lolli_plot(test2)
ggsave("~/Desktop/Kirstenbosch_lolli.jpg")

SAWS_prepped <- SAWS_homogenised[,c(1,2,5)]
colnames(SAWS_prepped) <- c("site", "t", "temp")

## Heat waves
ahw_events_5day_2gap <- data.frame()
for(i in 1:length(levels(SAWS_prepped$site))){
  data1 <- droplevels(subset(SAWS_prepped, site == levels(SAWS_prepped$site)[i]))
  data2 <- detect.full(data1, 5, 2, 90)
  data3 <- data.frame(data2$event)
  ahw_events_5day_2gap <- rbind(ahw_events_5day_2gap, data3)
}

ahw_events_3day_1gap <- data.frame()
for(i in 1:length(levels(SAWS_prepped$site))){
  data1 <- droplevels(subset(SAWS_prepped, site == levels(SAWS_prepped$site)[i]))
  data2 <- detect.full(data1, 3, 1, 90)
  data3 <- data.frame(data2$event)
  ahw_events_3day_1gap <- rbind(ahw_events_3day_1gap, data3)
}

## Cold spells
acs_events_5day_2gap <- data.frame()
for(i in 1:length(levels(SAWS_prepped$site))){
  data1 <- droplevels(subset(SAWS_prepped, site == levels(SAWS_prepped$site)[i]))
  data2 <- detect.full(data1, 5, 2, 10)
  data3 <- data.frame(data2$event)
  acs_events_5day_2gap <- rbind(acs_events_5day_2gap, data3)
}

acs_events_3day_1gap <- data.frame()
for(i in 1:length(levels(SAWS_prepped$site))){
  data1 <- droplevels(subset(SAWS_prepped, site == levels(SAWS_prepped$site)[i]))
  data2 <- detect.full(data1, 3, 1, 10)
  data3 <- data.frame(data2$event)
  acs_events_3day_1gap <- rbind(acs_events_3day_1gap, data3)
}

# SAWS_prepped <- SAWS_prepped %>%  ## Not working
#   group_by(site) %>%
#   nest() %>% 
#   mutate(events = data %>% map(detect.full))
# SAWS_prepped <- unnest(SAWS_prepped, events)


# 3. Save the results -----------------------------------------------------

# Heat waves
save(ahw_events_5day_2gap, file = "data/SAWS/events/ahw_events_5day_2gap.Rdata")
save(ahw_events_3day_1gap, file = "data/SAWS/events/ahw_events_3day_1gap.Rdata")
# Cold-spells
save(acs_events_5day_2gap, file = "data/SAWS/events/acs_events_5day_2gap.Rdata")
save(acs_events_3day_1gap, file = "data/SAWS/events/acs_events_3day_1gap.Rdata")
