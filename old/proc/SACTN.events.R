#############################################################################
###"prep/SACTN.events.R"
## This script does:
# 1. Load SACTN events
# 2. Save
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(ggplot2)
library(stringr)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(tidyr)
library(purrr)
library(broom)
## USED BY:
# "graph/SACTN.SAWS.line.R"
## CREATES:
# 
#############################################################################


# 1. Load SACTN events ----------------------------------------------------

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

mhw_events_5day_2gap <- eventLoad(dir1) # produce a data frame with mhw data...
save(mhw_events_5day_2gap, file = "data/SACTN/events/mhw_events_5day_2gap.Rdata")
mcs_events_5day_2gap <- eventLoad(dir2) # produce a data frame with mcs data...
save(mcs_events_5day_2gap, file = "data/SACTN/events/mcs_events_5day_2gap.Rdata")
