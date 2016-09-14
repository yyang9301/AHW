## This script identifies heatwaves and cold spells in SAWS atmospheric data using RMarineHeatwaves

# Load packages -----------------------------------------------------------
library(stringr)
library(zoo)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(broom)
library(tibble)
library(doMC); doMC::registerDoMC(cores = 4)
library(purrr)
library(RmarineHeatWaves)


# Load cropped data ---------------------------------------------------------------

load("data/SAWS_data_cropped.Rdata")

SAWS_meta_cropped <- read.csv("data/SAWS_meta_data_cropped.csv")

# Calculate heatwaves -----------------------------------------------------
## This function must be given t = time and temp = temperature columns, same as make_whole
## It then does all of the necessary calculations dynamically for the dtime series given
## This is done so it can be used with purrr::map in one easy nested run
detect.full <- function(dat, dur, gap, pctile){
  site <- dat$site[1]
  dat2 <- dat
  dat2$site <- NULL
  whole <- make_whole(dat2)
  full_year_start <- year(whole$date[1])+1
  full_year_end <- year(whole$date[length(whole$date)])-1
  results <- detect(whole, climatology_start = full_year_start, climatology_end = full_year_end,
                    min_duration = dur, max_gap = gap, pctile = pctile)
  results$clim$site <- site
  results$event$site <- site
  return(results)
}

levels(SAWS_data_cropped$station)

# A tester to ensure the function functions as intended
test <- droplevels(SAWS_data_clean[as.character(SAWS_data_cropped$station) == "KIRSTENBOSCH",])
test <- test[,c(2,5,8)]
colnames(test) <- c("site", "t", "temp")
test2 <- detect.full(test, 5, 2, 90)
lolli_plot(test2)
ggsave("~/Desktop/Kirstenbosch_lolli.jpg")

SAWS_prepped <- SAWS_data_cropped[,c(2,5,8)]
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


# Create lolli_plots ---------------------------------------------------





# Load marine results -----------------------------------------------------
## NB: This chunk is copied from "~/MHW/proc/results2.R"

#####
colNames <- c(
  "eventNo", # Event number,
  "yearStrt", # Start year,
  "monthStrt", # Start month,
  "dayStrt", # Start day,
  "yearPk", # Peak year,
  "monthPk", # Peak month,
  "dayPk", # Peak day,
  "yearEnd", # End year,
  "monthEnd", # End month,
  "dayEnd", # End day,
  "duration", # Duration [days],
  "intMax", # Maximum intensity [deg C],
  "intMean", # Mean intensity [deg C],
  "intCum", # Cumulative intensity [deg C x days],
  "intVar", # Intensity variability [deg C],
  "onsetRt", # Rate of onset [deg C / days],
  "declRt", # Rate of decline [deg C / days],
  "relThreshMax", # Maximum intensity (rel. thresh.) [deg C],
  "relThreshMean", # Mean intensity (rel. thresh.) [deg C],
  "relThreshCum", # Cumulative intensity (rel. thresh.) [deg C x days],
  "relThreshVar", # Intensity variability (rel. thresh.) [deg C],
  "absIntMax", # Maximum intensity (absolute) [deg C],
  "absIntMean", # Mean intensity (absolute) [deg C],
  "absIntCum", # Cumulative intensity (absolute) [deg C x days],
  "absIntVar", # Intensity variability (absolute) [deg C],
  "normIntMax", # Maximum intensity (normalized) [unitless],
  "normIntMean" # Mean intensity (normalized) [unitless]
)
#####

# Load metadata
load("~/MHW/data/metaData2.Rdata")

eventLoad <- function(dir) {
  fname1 <-  dir(dir, full.names = TRUE)
  fname2 <-  dir(dir, full.names = FALSE)
  pf <- str_sub(fname2, -20, -1)
  siteNames1 <-  unlist(strsplit(dir(dir, pattern = "data.events", full.names = FALSE), pf[1]))
  siteNames1 <-  str_replace_all(siteNames1, "_", " ") # parse names for site column
  dat <- data.frame()
  for(i in 1:length(fname1)) { # A shameful for loop... in order to label sites correctly
    x <- read.csv(fname1[i], header = FALSE, skip = 2, sep = ",",
                  col.names = colNames) # Not currenty cleaning up the column names
    x$site <-  siteNames1[i]
    x$coast <- metaData2$coast[metaData2$site == x$site[1]]
    x$date <-  as.Date(paste(x$yearStrt, x$monthStrt, x$dayStrt, sep = "-"))
    x$month <- floor_date(as.Date(paste(x$yearStrt, x$monthStrt, x$dayStrt, sep = "-")), "month")
    x$lon <- metaData2$lon[metaData2$site == x$site[1]]
    x$lat <- metaData2$lat[metaData2$site == x$site[1]]
    dat <- rbind(dat, x)
  }
  return(dat)
}

#  Individual events
## NB: These pathways have been modified from the original for use in the AHW project
dir1 <- "~/MHW/data/MHW/events/"
dir2 <- "~/MHW/data/MCS/events/"
dir3 <- "~/MHW/data/MHW/SST events/"
dir4 <- "~/MHW/data/MCS/SST events/"

mhw <- eventLoad(dir1) # produce a data frame with mhw data...
mhw$type <- "insitu"
mcs <- eventLoad(dir2) # produce a data frame with mcs data...
mcs$type <- "insitu"
mhwSST <- eventLoad(dir3) # produce a data frame with SST mhw data...
mhwSST$type <- "sst"
mcsSST <- eventLoad(dir4) # produce a data frame with SST mcs data...
mcsSST$type <- "sst"

# Compare atmospheric results to marine results ---------------------------------------------

# Load nearest site index
SACTN_nearest <- read.csv("data/SACTN_nearest_SAWS_sites")

## Add Lat/ lon to data
# ahw_events_5day_2gap <- ahw_events_5day_2gap %>% ## Causes instant crash
#   group_by(site) %>%
#   mutate(lat = SAWS_meta$lat[SAWS_meta$station == site][1])
# ahw_events_5day_2gap <- data.frame(ahw_events_5day_2gap)

## NB: Adapted from "~/MHW/proc/results2.R"
# dat1 must be the marine data frame
# dat2 is the atmospheric data frame
cooccurrence <- function(dat1, dat2, lag = seq(2,14,2)){
  dat1$site <- as.character(dat1$site)
  dat2$site <- as.character(dat2$site)
  dat3 <- data.frame()
  direction <- c("b","x","a")
  for(i in 1:length(levels(as.factor(dat1$site)))) {
    x1 <- droplevels(subset(dat1, site == levels(as.factor(dat1$site))[i]))
    meta1 <- droplevels(subset(SACTN_nearest, site == x1$site[1]))
    x2 <- droplevels(subset(dat2, site == meta1$station))
    x2$yearStrt <- year(x2$date_start)
    x1 <- x1[x1$yearStrt >= min(x2$yearStrt), ] # Subset x so that dates match up
    x1 <- x1[x1$yearStrt <= max(x2$yearStrt), ]
    x2 <- x2[x2$yearStrt >= min(x1$yearStrt), ]
    x2 <- x2[x2$yearStrt <= max(x1$yearStrt), ]
    for(j in 1:length(lag)){
      for(k in 1:length(seq(0.0,1,0.1))){
        for(l in 1:length(direction)){
          x1.1 <- x1[x1$intCum >= quantile(x1$intCum, probs = seq(0.0,1,0.1)[k]),]
          x2.1 <- x2[x2$intCum >= quantile(x2$intCum, probs = seq(0.0,1,0.1)[k]),]
          y <- 0
          #x3 <- data.frame() # For test purposes to see which events match up
          for(m in 1:nrow(x1.1)) {
            x1.2 <- x1.1$date[m]
            if(direction[l] == "b"){
              x1.3 <- seq((x1.2 - days(lag[j])), x1.2, 1)
            } else if(direction[l] == "x"){
              x1.3 <- seq((x1.2 - days(lag[j])), (x1.2 + days(lag[j])), 1)
            } else if (direction[l] == "a") {
              x1.3 <- seq(x1.2, (x1.2 + days(lag[j])), 1)
            }
            if(length(x2.1$site) == 0){
              y <- y + 0
            } else {
              x2.2 <- droplevels(subset(x2.1, date %in% x1.3))
              y <- y + nrow(x2.2)
            }
          }
          z <- data.frame(site = x1$site[1], station = x2$site[1], lon = x1$lon[1], lat = x1$lat[1],
                          lag = lag[j], quantile = seq(0.0,1,0.1)[k], direction = direction[l],
                          insitu = nrow(x1.1), OISST = nrow(x2.1),
                          cooccurrence= y, proportion = y/nrow(x1.1))
          dat3 <- rbind(dat3, z)
        }
      }
    }
  }
  return(dat3)
}

# MHW
mhw_CO_5_2 <- cooccurrence(mhw, ahw_events_5day_2gap) # This takes several minutes to run... cursed for loops...
write.csv(mhw_CO_5_2, "data/mhw_CO_5_2.csv", row.names = F)
mhw_CO_3_1 <- cooccurrence(mhw, ahw_events_3day_1gap)
write.csv(mhw_CO_3_1, "data/mhw_CO_3_1.csv", row.names = F)
# MCS
mcs_CO_5_2 <- cooccurrence(mcs, acs_events_5day_2gap)
write.csv(mcs_CO_5_2, "data/mcs_CO_5_2.csv", row.names = F)
mcs_CO_3_1 <- cooccurrence(mcs, acs_events_3day_1gap)
write.csv(mcs_CO_3_1, "data/mcs_CO_3_1.csv", row.names = F)


# Create figures showing results ------------------------------------------

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
    facet_grid(site ~ direction) +
    ylab("proportion") + xlab("quantile (%)") #+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  p2
}

mhw_CO_5_2_fig <- cooccurrenceQuantFigure(mhw_CO_5_2)
ggsave("graph/mhw_CO_5_2_fig.pdf", width = 16, height = 18)
mhw_CO_3_1_fig <- cooccurrenceQuantFigure(mhw_CO_3_1)
ggsave("graph/mhw_CO_3_1_fig.pdf", width = 16, height = 18)
mcs_CO_5_2_fig <- cooccurrenceQuantFigure(mcs_CO_5_2)
ggsave("graph/mcs_CO_5_2_fig.pdf", width = 16, height = 18)
mcs_CO_3_1_fig <- cooccurrenceQuantFigure(mcs_CO_3_1)
ggsave("graph/mcs_CO_3_1_fig.pdf", width = 16, height = 18)
