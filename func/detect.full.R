#############################################################################
###"func/detect.full.R"
## This script does:
# 1. Run make_whole() on a time series to prep for event detection
# 2. Run detect()
# 3. Return event or annual results
## DEPENDS ON:
# (dat) must have a "site" = site, "t" = date, "temp" = temperature columns
# The function will attempt to correct if these are missing
# It will tell you what is missing if so
## USED BY:
# Many "proc/" scripts
## CREATES:
# nothing
#############################################################################

library(RmarineHeatWaves)

## For testing purposes
# load("data/SACTN/SACTN_cropped.Rdata")
# dat <- SACTN_cropped[SACTN_cropped$site == levels(SACTN_cropped$site)[4],]
# start <- year(dat$date[1])+1
# end <- year(dat$date[nrow(dat)])-1
# pctile <- 90
# dur <- 5
# gap <- 2
# cold_spell <- TRUE

#  ------------------------------------------------------------------------
## This function must be given site = site, t = time and temp = temperature columns, same as make_whole
## It then does all of the necessary calculations dynamically for the time series given
## This is done so it can be run in a for loop
## Dplyr would be ideal but the output of detect() complicates this
detect.full <- function(dat, start, end, dur, gap, cold_spell){
  col.index <- c("site", "t", "temp")
  colnames(dat)[colnames(dat) == "date"] <- "t"
  dat <- dat[colnames(dat) %in% col.index]
  if(ncol(dat) < 3){
    stop(paste("Your data.frame is missing", colnames(dat[!(colnames(dat) %in% col.index)]), sep = " "))
  }
  site <- as.character(dat$site[1])
  dat2 <- dat
  dat2$site <- NULL
  whole <- make_whole(dat2)
  results <- detect(whole, climatology_start = start, climatology_end = end,
                  min_duration = dur, max_gap = gap, cold_spells = cold_spell)
  results$clim$site <- site
  results$event$site <- site
  return(results)
}


#  ------------------------------------------------------------------------
## This function is designed to be used with step 2 in "proc/SACTN.RMarienHeatwaves.R"

# dat <- SACTN_cropped[SACTN_cropped$site == levels(SACTN_cropped$site)[5],]

detect.SACTN <- function(dat){
  site <- as.character(dat$site[1])
  start <- dat$start[1]
  end <- dat$end[1]
  dat <- dat[,2:3]
  whole <- make_whole(dat)
  # MHW
  mhw <- detect(whole, climatology_start = start, climatology_end = end,
                    min_duration = 5, max_gap = 2, cold_spells = FALSE)
  mhw$event$type <- "MHW"
  mhw$event$site <- site
  mhw$clim$type <- "MHW"
  mhw$clim$site <- site
  # mhw <- mhw$event
  # mhw$type <- "MHW"
  # mhw$site <- site
  #MCS
  mcs <- detect(whole, climatology_start = start, climatology_end = end,
                min_duration = 5, max_gap = 2, cold_spells = TRUE)
  mcs$event$type <- "MCS"
  mcs$event$site <- site
  mcs$clim$type <- "MCS"
  mcs$clim$site <- site
  # Combine
  event <- data.frame(rbind(data.frame(mhw$event), data.frame(mcs$event)))
  clim <- data.frame(rbind(data.frame(mhw$clim), data.frame(mcs$clim)))
  res <- list(event = event, clim =  clim)
  return(res)
}


#  ------------------------------------------------------------------------
## These two functions are designed to be used with step 3 in "proc/SAWS.RMarienHeatwaves.R"

# dat <- SAWS_tmean[SAWS_tmean$site == levels(SAWS_tmean$site)[3],] ## For testing purposes
# df <- SACTN_analysis_period[3,]

# This function calculates the events for a single SAWS site using the the analysis period of a single SACTN site
detect.SAWS.SACTN <- function(df, dat){
  site <- as.character(dat$site[1])
  SACTN <- as.character(df$site[1])
  index <- paste(site, SACTN, sep = " - ")
  start <- df$start[1]
  end <- df$end[1]
  dat <- dat[,2:3]
  whole <- make_whole(dat)
  ahw <- detect(whole, climatology_start = start, climatology_end = end,
                min_duration = 3, max_gap = 0, cold_spells = FALSE)
  ahw <- ahw$event
  ahw$type <- "AHW"
  acs <- detect(whole, climatology_start = start, climatology_end = end,
                min_duration = 3, max_gap = 0, cold_spells = TRUE)
  acs <- acs$event
  acs$type <- "ACS"
  events <- rbind(ahw, acs)
  events$site <- site
  events <- data.frame(index, events)
  return(events)
}

# This function calculates the extreme events for a time series
# Choose to either use the "SAWS" analysis period or the "SACTN" analysis period
# "SAWS" = 1981 - 2010 as per Kruger et al. (2016)
# "SACTN" = first - last full year of data for the SACTN time series the SAWS time series is being compared against
load("setupParams/SACTN_analysis_period.Rdata")
detect.SAWS <- function(dat, clim = "SAWS"){
  if(clim == "SAWS"){
    site <- as.character(dat$site[1])
    start <- 1981
    end <- 2010
    dat <- dat[,2:3]
    whole <- make_whole(dat)
    ahw <- detect(whole, climatology_start = start, climatology_end = end,
                  min_duration = 3, max_gap = 0, cold_spells = FALSE)
    ahw <- ahw$event
    ahw$type <- "AHW"
    acs <- detect(whole, climatology_start = start, climatology_end = end,
                  min_duration = 3, max_gap = 0, cold_spells = TRUE)
    acs <- acs$event
    acs$type <- "ACS"
    results <- rbind(ahw, acs)
    results$site <- site
  } else if(clim == "SACTN"){
    results <- ddply(SACTN_analysis_period, .(site), detect.SAWS.SACTN, dat = dat, .parallel = TRUE) ## ~60 seconds
  } else {
    stop("You must set 'clim' to either 'SAWS' or 'SACTN'.")
  }
  return(results)
}

