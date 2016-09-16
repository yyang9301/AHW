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
source("func/detect.full.R")
## USED BY:
# Nothing
## CREATES:
# 
#############################################################################


# 1. Load the SAWS data and site lists ---------------

# SAWS
load("data/SAWS/homogenised/SAWS_homogenised.Rdata")
load("setupParams/SAWS_site_list.Rdata")


# 2. Calculate heatwaves/ cold-spells for the SAWS data --------

levels(SAWS_homogenised$site)

# A tester to ensure the function functions as intended
test <- droplevels(SAWS_homogenised[as.character(SAWS_homogenised$site) == "Cape Agulhas",])
test <- test[,c(1,2,5)]
colnames(test) <- c("site", "t", "temp")
test2 <- detect.full(dat = test, start = 1981, end = 2010, dur = 3, gap = 0, cold_spell = TRUE)
lolli_plot(test2)
ggsave("~/Desktop/Kirstenbosch_lolli.jpg")

SAWS_prepped <- SAWS_homogenised[,c(1,2,5)]
colnames(SAWS_prepped) <- c("site", "t", "temp")

## Heat waves
ahw_events_5day_2gap <- data.frame()
for(i in 1:length(levels(SAWS_prepped$site))){
  data1 <- droplevels(subset(SAWS_prepped, site == levels(SAWS_prepped$site)[i]))
  data2 <- detect.full(dat = data1, start = 1981, end = 2010, dur = 5, gap = 2, cold_spell = FALSE)
  data3 <- data.frame(data2$event)
  ahw_events_5day_2gap <- rbind(ahw_events_5day_2gap, data3)
}

ahw_events_3day_0gap <- data.frame()
for(i in 1:length(levels(SAWS_prepped$site))){
  data1 <- droplevels(subset(SAWS_prepped, site == levels(SAWS_prepped$site)[i]))
  data2 <- detect.full(dat = data1, start = 1981, end = 2010, dur = 3, gap = 0, cold_spell = FALSE)
  data3 <- data.frame(data2$event)
  ahw_events_3day_0gap <- rbind(ahw_events_3day_0gap, data3)
}

## Cold spells
acs_events_5day_2gap <- data.frame()
for(i in 1:length(levels(SAWS_prepped$site))){
  data1 <- droplevels(subset(SAWS_prepped, site == levels(SAWS_prepped$site)[i]))
  data2 <- detect.full(dat = data1, start = 1981, end = 2010, dur = 5, gap = 2, cold_spell = TRUE)
  data3 <- data.frame(data2$event)
  acs_events_5day_2gap <- rbind(acs_events_5day_2gap, data3)
}

acs_events_3day_0gap <- data.frame()
for(i in 1:length(levels(SAWS_prepped$site))){
  data1 <- droplevels(subset(SAWS_prepped, site == levels(SAWS_prepped$site)[i]))
  data2 <- detect.full(dat = data1, start = 1981, end = 2010, dur = 3, gap = 0, cold_spell = TRUE)
  data3 <- data.frame(data2$event)
  acs_events_3day_0gap <- rbind(acs_events_3day_0gap, data3)
}

# SAWS_prepped <- SAWS_prepped %>%  ## Not working
#   group_by(site) %>%
#   nest() %>% 
#   mutate(events = data %>% map(detect.full))
# SAWS_prepped <- unnest(SAWS_prepped, events)


# 3. Save the results -----------------------------------------------------

# Heat waves
save(ahw_events_5day_2gap, file = "data/SAWS/events/ahw_events_5day_2gap.Rdata")
save(ahw_events_3day_0gap, file = "data/SAWS/events/ahw_events_3day_0gap.Rdata")
# Cold-spells
save(acs_events_5day_2gap, file = "data/SAWS/events/acs_events_5day_2gap.Rdata")
save(acs_events_3day_0gap, file = "data/SAWS/events/acs_events_3day_0gap.Rdata")
