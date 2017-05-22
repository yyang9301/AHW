#############################################################################
###"graph/wind.R"
## This script does:
# 1. Load necessary data
# 2. Create figures showing wind time series
# 3. Calculate wind values during co-occurrence
# 4. Calculate wind values during extreme events in air and sea independently
# 5. Redundency analysis
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(ggplot2)
library(plyr)
library(dplyr)
library(weatherData)
library(mgcv)
library(scales)
library(reshape2)
library(gridExtra)
library(lubridate)
library(weathermetrics)
source("setupParams/theme.R")
source("func/detect.full.R")
## USED BY:
# 
## CREATES:
# "wind.pdf"
#############################################################################


# 1. Load necessary data --------------------------------------------------

# 1) Wind
load("data/wind/wind_PN.Rdata")

# 2) Sea temperatures
load("data/SACTN/SACTN_cropped.Rdata")
temp_PN <- filter(SACTN_cropped, site == "Port Nolloth")
# Change to standard for ggplot

# Use this to check the nearest sites etc.
load("setupParams/distances_bearings.Rdata")

# 3) Air temperatures
load("data/SAWS/homogenised/SAWS_homogenised.Rdata")
temp_CC <- filter(SAWS_homogenised, site == "Cape Columbine")

# 4) Combine temperature data frames

# 5) Extreme events
# SACTN
load("data/events/SACTN_events.Rdata")
SACTN_events_PN <- filter(SACTN_events, site == "Port Nolloth")
# OISST
load("data/events/SAWS_SAWS_events_tmean.Rdata")
SAWS_SAWS_events_tmean_CC <- filter(SAWS_SAWS_events_tmean, site == "Cape Columbine")

# 6) Co-occurrence
# Heat waves
load("data/cooccurrence/SACTN_SAWS_hw_tmean_CO.Rdata")
hw_tmean_CC_PN <- filter(SACTN_SAWS_hw_tmean_CO, index == "Cape Columbine - Port Nolloth")
load("data/cooccurrence/SACTN_SAWS_hw_tmax_CO.Rdata")
hw_tmax_CC_PN <- filter(SACTN_SAWS_hw_tmax_CO, index == "Cape Columbine - Port Nolloth")
load("data/cooccurrence/SACTN_SAWS_hw_tmin_CO.Rdata")
hw_tmin_CC_PN <- filter(SACTN_SAWS_hw_tmin_CO, index == "Cape Columbine - Port Nolloth")
# Cold-spells
load("data/cooccurrence/SACTN_SAWS_cs_tmean_CO.Rdata")
cs_tmean_CC_PN <- filter(SACTN_SAWS_cs_tmean_CO, index == "Cape Columbine - Port Nolloth")
load("data/cooccurrence/SACTN_SAWS_cs_tmax_CO.Rdata")
cs_tmax_CC_PN <- filter(SACTN_SAWS_cs_tmax_CO, index == "Cape Columbine - Port Nolloth")
load("data/cooccurrence/SACTN_SAWS_cs_tmin_CO.Rdata")
cs_tmin_CC_PN <- filter(SACTN_SAWS_cs_tmin_CO, index == "Cape Columbine - Port Nolloth")


# 2. Create figures showing wind time series ------------------------------

# Prep wind data frame for use with ggplot
# wind_PN$x.end <- wind_PN$date + 1 # Lol... this is one cool thing about R for sure
dates <- seq(as.Date("1996-01-01"), as.Date("1996-06-01"), by = "day")
temp_CC_sub <- filter(temp_CC, date %in% dates)
temp_PN_sub <- filter(temp_PN, date %in% dates)
wd <- filter(wind_PN, date %in% dates)
wd$bearing[wd$bearing == 0] <- 360
# wd$bearing <- wd$bearing-180 # Correct to show direction where wind is coming from

wd$u <- (1 * wd$speed) * sin((wd$bearing * pi / 180.0))
wd$v <- (1 * wd$speed) * cos((wd$bearing * pi / 180.0))
# dw <- subset(wd, u != 0 & v != 0)
dw <- wd

v_breaks = pretty_breaks(n = 5)(min(dw$v, na.rm = T):max(dw$v, na.rm = T))
v_labels = abs(v_breaks)

dwd_p <-  ggplot(data = temp_PN_sub, aes(x = date, y = temp)) + bw_update +
  geom_line(linetype = "solid") +
  geom_line(data = temp_CC_sub, aes(x = date, y = temp), linetype = "dashed") +
  geom_segment(data = dw, aes(x = date, xend = date + u*10, y = 0, yend = v), arrow = arrow(length = unit(0.15, "cm")), size = 0.5) +
  geom_point(data = dw, aes(x = date, y = 0), alpha = 0.5, size=1) +
  geom_point(data = dw[is.na(dw$speed),], aes(x = date, y = 0), alpha = 0.5, size = 4, colour = "grey") +
  scale_x_date(name="Date",labels = date_format("%Y-%m"),breaks = date_breaks("1 month")) #+
  # scale_y_continuous(name = "Wind vectors (kn/h)", labels = v_labels, breaks = v_breaks)
dwd_p


# 3. Calculate wind values during co-occurrence ---------------------------

# 1) Subset temperature data to match wind data dates
temp_CC_sub <- filter(temp_CC, date %in% wind_PN$date)
temp_PN_sub <- filter(temp_PN, date %in% wind_PN$date)

# 2) Subset co-occurrence results to match wind data dates
# Heat waves
hw_tmean_CC_PN_sub <- filter(hw_tmean_CC_PN, date_start %in% wind_PN$date & abs(latest) <= 7)
hw_tmax_CC_PN_sub <- filter(hw_tmax_CC_PN, date_start %in% wind_PN$date & abs(latest) <= 7)
hw_tmin_CC_PN_sub <- filter(hw_tmin_CC_PN, date_start %in% wind_PN$date & abs(latest) <= 7)
# Cold spells
cs_tmean_CC_PN_sub <- filter(cs_tmean_CC_PN, date_start %in% wind_PN$date & abs(latest) <= 7)
cs_tmax_CC_PN_sub <- filter(cs_tmax_CC_PN, date_start %in% wind_PN$date & abs(latest) <= 7)
cs_tmin_CC_PN_sub <- filter(cs_tmin_CC_PN, date_start %in% wind_PN$date & abs(latest) <= 7)

# This figure is designed to have a co-occurrence data frame fed into it via ddply by event_no AND percentile.idx
  # Note that there may be duplicates within the dataframes due to the way these were originally sampled
# The function then takes the week before and after the start of whichever event came first
# The air and sea temps, and wind data are then subsetted and a figure is made so one can visually inspect possible relationships
# Options for the 'stat' variable are: "temp", "tmax", "tmin"
# df <- hw_tmean_CC_PN_sub[1,]
wind.figure <- function(df, stat){
  # QC
  if(nrow(df) > 1){
    stop(paste("There are multiple events being subsetted! See:", df$index_start, sep = " "))
  }
  # Create date range based around first event for subsetting
  if(df$latest >= 0){
    dates <- seq(df$date_start.1-7, df$date_stop.1+7, by = "day") 
  } else {
    dates <- seq(df$date_start-7, df$date_stop+7, by = "day")
  }
  # Subset air temperatures and select correct column to match co-occurrence data frame
  air1 <- filter(temp_CC_sub, date %in% dates)
  air2 <- air1[colnames(air1) == stat]
  air3 <- cbind(air1[,2], air2)
  colnames(air3) <- c("date","temp")
  air3$type <- "air"
  # Subset sea temperatures
  sea1 <- filter(temp_PN_sub, date %in% dates)
  sea2 <- sea1[,4:5]
  sea2$type <- "sea"
  # Subset wind data and prep for plotting
  wind <- filter(wind_PN, date %in% dates)
  wind$u <- (1 * wind$speed) * sin((wind$bearing * pi/180))
  wind$v <- (1 * wind$speed) * cos((wind$bearing * pi/180))
  # Combine air and sea temps for better plotting
  temps <- rbind(sea2, air3)
  # Prep start and end dates of events for plotting
  event_dates <- data.frame(dates = c(df$date_start, df$date_stop, df$date_start.1, df$date_stop.1), 
                            type = c("air", "air", "sea", "sea"),
                            range = c("begin", "end", "begin", "end"))
  # Create file name of figure for saving
  file_name <- paste(df$type, stat, dates[8], df$percentile.idx, sep = "_")
  # Create figure
  ggplot(data = temps, aes(x = date, y = temp)) + bw_update +
    geom_line(aes(colour = type)) +
    geom_segment(data = wind, aes(x = date, xend = date + u, y = 0, yend = v), arrow = arrow(length = unit(0.15, "cm")), size = 0.5) +
    geom_point(data = wind, aes(x = date, y = 0), alpha = 0.5, size=1) +
    geom_vline(data = event_dates, aes(xintercept = as.numeric(dates), colour = type, linetype = range), size = 1, alpha = 0.7) +
    scale_x_date(name = "Date", labels = date_format("%Y-%m-%d"), breaks = date_breaks("3 days")) +
    theme(axis.text.x = element_text(angle = 20, hjust = 0.4, vjust = 0.5, size = 8),
          axis.text.y = element_text(size = 8))
  ggsave(filename = paste("graph/all_wind/", file_name, ".jpg", sep = ""))
}
# ddply(cs_tmin_CC_PN_sub, .(event_no, percentile.idx), wind.figure, stat = "tmin")
## Create the desired figures
# heat waves
ddply(hw_tmean_CC_PN_sub, .(event_no, percentile.idx), wind.figure, stat = "temp")
ddply(hw_tmax_CC_PN_sub, .(event_no, percentile.idx), wind.figure, stat = "tmax")
ddply(hw_tmin_CC_PN_sub, .(event_no, percentile.idx), wind.figure, stat = "tmin")
# cold-spells
ddply(cs_tmean_CC_PN_sub, .(event_no, percentile.idx), wind.figure, stat = "temp")
ddply(cs_tmax_CC_PN_sub, .(event_no, percentile.idx), wind.figure, stat = "tmax")
ddply(cs_tmin_CC_PN_sub, .(event_no, percentile.idx), wind.figure, stat = "tmin")

# This function differs from the one above with the same name in that it is designed to be fully automated for use with all time series
# It must be subsetted by: index, index_start, percentile.idx
df <- hw_tmean_CC_PN_sub[1,]
wind.figure <- function(df, stat){
  # QC
  if(nrow(df) > 1){
    stop(paste("There are multiple events being subsetted! See:", df$index_start, sep = " "))
  }
  # Create date range for wind data based on widest possible date range
  if(df$date_start <= df$date_start.1){
    start <- df$date_start
  } else {
    start <- df$date_start.1
  }
  if(df$date_stop >= df$date_stop.1){
    stop <- df$date_stop
  } else {
    stop <- df$date_stop.1
  }
  dates <- seq(start-7, stop+7, by = "day")
  # Ascertain what type of event is being calculated
  if(df$type == "AHW"){
    cold_spells = FALSE
  } else if (df$type == "ACS"){
    cold_spells = FALSE
  } else {
    stop("No event type detected.")
  }
  # Calculate events for sea temps
  sea <- SACTN_cropped %>% 
    select(site, date, temp) %>% 
    filter(site == df$site.1[1]) %>% 
    mutate(start = SACTN_analysis_period$start[SACTN_analysis_period$site == site][1]) %>% 
    mutate(end = SACTN_analysis_period$end[SACTN_analysis_period$site == site][1])
  sea <- droplevels(sea)
  colnames(sea)[2] <- "t"
  whole <- make_whole(sea[,2:3])
  marine <- detect(whole, climatology_start = sea$start[1], climatology_end = sea$end[1],
                min_duration = 5, max_gap = 2, cold_spells = cold_spells)
  marine_event <- marine$event
  marine_event <- marine_event[marine_event$event_no == df$event_no.1,]
  marine$event <- marine_event
  # Calculate events for air temps
  air1 <- SAWS_homogenised[colnames(SAWS_homogenised) == stat]
  air2 <- cbind(SAWS_homogenised[,1:2], air1)
  air3 <- air2 %>% 
    filter(site == df$site[1]) %>% 
    mutate(start = SACTN_analysis_period$start[SACTN_analysis_period$site == df$site.1][1]) %>% 
    mutate(end = SACTN_analysis_period$end[SACTN_analysis_period$site == df$site.1][1])
  air3 <- droplevels(air3)
  colnames(air3)[2] <- "t"
  whole <- make_whole(air3[,2:3])
  atmosphere <- detect(whole, climatology_start = air3$start[1], climatology_end = air3$end[1],
                min_duration = 3, max_gap = 0, cold_spells = cold_spells)
  atmosphere_event <- atmosphere$event
  atmosphere_event <- atmosphere_event[atmosphere_event$event_no == df$event_no,]
  atmosphere$event <- atmosphere_event
  # Subset wind data and prep for plotting
  wind <- filter(wind_PN, date %in% dates)
  wind$u <- (1 * wind$speed) * sin((wind$bearing * pi/180))
  wind$v <- (1 * wind$speed) * cos((wind$bearing * pi/180))
  # Create the event_line and wind vector figures
  m_fig <- event_line(marine, spread = 15, start_date = marine$event$date_start, end_date = marine$event$date_stop)
  a_fig <- event_line(atmosphere, spread = 15, start_date = atmosphere$event$date_start, end_date = atmosphere$event$date_stop)
  w_fig <- ggplot() + bw_update +
    geom_segment(data = wind, aes(x = date, xend = date + u, y = 0, yend = v), arrow = arrow(length = unit(0.15, "cm")), size = 0.5) +
    geom_point(data = wind, aes(x = date, y = 0), alpha = 0.5, size=1) +
    scale_x_date(name = "Date", labels = date_format("%Y-%m-%d"), breaks = date_breaks("3 days"))
  # Create file name of figure for saving
  file_name <- paste(df$site[1], df$site.1[1], df$type, stat, dates[8], df$percentile.idx, sep = "_")
  # Mosh it all together
  jpeg(paste("graph/all_wind/", file_name, ".jpg", sep = ""), width = 900, height = 900, pointsize = 12) # Set PDF dimensions
  vp1 <- viewport(x = 0.5, y = 1.0, w = 1.0, h = 0.33, just = "top") # marine
  vp2 <- viewport(x = 0.5, y = 0.66, w = 1.0, h = 0.33, just = "top")  # atmosphere
  vp3 <- viewport(x = 0.5, y = 0.33, w = 1.0, h = 0.2, just = "top")  # wind
  print(m_fig, vp = vp1)
  print(a_fig, vp = vp2)
  print(w_fig, vp = vp3)
  dev.off()
}


# 4. Calculate wind values during extreme events in air and sea -----------

PN_events_wind <- filter(SACTN_events_PN, date_start %in% wind_PN$date)


# 5. Redundency analysis

