#############################################################################
###"graph/wind.R"
## This script does:
# 1. Load necessary data
# 2. Create figures showing wind time series
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
  scale_x_date(name="Date",labels = date_format("%Y-%m"),breaks = date_breaks("1 month")) #+
  # scale_y_continuous(name = "Wind vectors (kn/h)\n", labels = v_labels, breaks = v_breaks)
dwd_p

