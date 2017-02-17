#############################################################################
###"graph/figures3.R"
## This script does:
# 1. Load SACTN event data
# 2. Prepare graphing function for sea/ air state during events
# 3. Create figure for each event
## DEPENDS ON:
library(grid)
library(ggplot2)
library(viridis)
library(stringr)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(lubridate)
library(zoo)
library(tidyr)
library(purrr)
library(broom)
library(RmarineHeatWaves)
library(doMC); registerDoMC(cores = 4)
source("setupParams/theme.R")
source("func/load.reanalyses.R")
## USED BY:
# 
## CREATES:
# Figures similar to Eric's figures in the MHW atlas
#############################################################################


# 1. Load SACTN event data ------------------------------------------------

load("data/events/SACTN_events.Rdata")
SACTN_events <- filter(SACTN_events, type == "MHW")
load("data/events/SACTN_clims.Rdata")
SACTN_clims <- filter(SACTN_clims, type == "MHW")
load("setupParams/SACTN_site_list.Rdata")

# Frequency ploygons of event duration
# ggplot(data = SACTN_events, aes(x = duration, group = site)) +
#   geom_freqpoly(binwidth = 5)

# Load SA map data
load("graph/southern_africa_coast.RData") # Lowres
names(southern_africa_coast)[1] <- "lon"
load("graph/sa_shore.Rdata") # Hires
names(sa_shore)[4:5] <- c("lon","lat")

# Load SA bathymetry
# load("~/SA_map/bathy.RData") # HiRes for 200m isobath
# load("~/SA_map/sa_bathy.RData") # LowRes for deeper

# ERA Interim file indices
file_1_dates <- seq(as.Date("1979-01-01"), as.Date("1989-01-01"), by = "day")
file_2_dates <- seq(as.Date("1990-01-01"), as.Date("1998-01-01"), by = "day")
file_3_dates <- seq(as.Date("1999-01-01"), as.Date("2007-01-01"), by = "day")
file_4_dates <- seq(as.Date("2008-01-01"), as.Date("2016-01-01"), by = "day")

# The lon/ lat ranges
wlon <- 10
elon <- 40
nlat <- -25
slat <- -40

sa_lons <- c(10, 40); sa_lats <- c(-40, -25)

# 2. Prepare graphing function for sea/ air state during events -----------

# Create the base map figure
sa <- ggplot() + bw_update +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), fill = "grey30", colour = "black", size = 0.1, show.legend = FALSE) +
  # geom_contour(data = bathy[bathy$depth >= -200,], aes(x = lon, y = lat, z = depth),
  #              colour = "grey20", alpha = 0.7, size = 0.6, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
  # stat_contour(data = sa_bathy[sa_bathy$depth < -200,], aes(x = lon, y = lat, z = depth, alpha = ..level..),
  #              colour = "grey20", size = 0.5, binwidth = 1000, na.rm = TRUE, show.legend = FALSE) +
  coord_equal() +
  scale_x_continuous(limits = sa_lons, expand = c(0, 0), breaks = seq(15, 35, 5)) +
  scale_y_continuous(limits = sa_lats, expand = c(0, 0), breaks = seq(-35, -30, 5)) +
  xlab("") +
  ylab("")
# sa

# First test run

# Extract longest event
# event <- SACTN_events[SACTN_events$duration == max(SACTN_events$duration),]
# event$lat <- SACTN_site_list$lat[SACTN_site_list$site == event$site]
# event$lon <- SACTN_site_list$lon[SACTN_site_list$site == event$site]

# Extract smallest event
event <- SACTN_events[SACTN_events$duration == min(SACTN_events$duration),][1,]
event$lat <- SACTN_site_list$lat[SACTN_site_list$site == event$site]
event$lon <- SACTN_site_list$lon[SACTN_site_list$site == event$site]

### Extract BRAN data during this event
## The index of months to load
date_idx <- seq(event$date_start, event$date_stop, by = "day")
temp_idx <- data.frame(files = paste0("~/data/BRAN/ocean_temp_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"), 
                       x = length(seq(event$date_start, event$date_stop, by = "month")))
u_idx <- data.frame(files = paste0("~/data/BRAN/ocean_u_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"), 
                    x = length(seq(event$date_start, event$date_stop, by = "month")))
v_idx <- data.frame(files = paste0("~/data/BRAN/ocean_v_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"), 
                    x = length(seq(event$date_start, event$date_stop, by = "month")))

## Load the data
# Temperature
system.time(BRAN_temp <- ddply(temp_idx, .(files), BRAN.Rdata, .progress = "text")) # ~21 seconds for one file
# This is not faster in parallel...
  # Ideally a data.table command could replace this use of ddply
# Create mean of temperatures
BRAN_temp <- filter(BRAN_temp, date %in% date_idx)
BRAN_temp <- data.table(BRAN_temp)
system.time(BRAN_temp <- BRAN_temp[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]) # 1 seconds
# BRAN_temp$stat <- "temp"

# U value
system.time(BRAN_u <- ddply(u_idx, .(files), BRAN.Rdata, .parallel = T)) # ~18 seconds for one file
# Create mean u values
BRAN_u <- filter(BRAN_u, date %in% date_idx)
BRAN_u <- data.table(BRAN_u)
system.time(BRAN_u <- BRAN_u[, .(u = mean(u, na.rm = TRUE)), by = .(x,y)]) # 1 seconds
# BRAN_u$stat <- "u"

# V value
system.time(BRAN_v <- ddply(v_idx, .(files), BRAN.Rdata, .parallel = T)) # ~21 seconds for one file
# Create mean of v values
BRAN_v <- filter(BRAN_v, date %in% date_idx)
BRAN_v <- data.table(BRAN_v)
system.time(BRAN_v <- BRAN_v[, .(v = mean(v, na.rm = TRUE)), by = .(x,y)]) # 1 seconds
# BRAN_v$stat <- "v"

# Create wind data frame
BRAN_wind <- merge(BRAN_u, BRAN_v, by = c("x", "y")); rm(BRAN_u, BRAN_v)
BRAN_scaler <- 1


### Extract ERA Interim data during this event
## Run the function as necesary on the following files
# NB: There is a slight possibility that more than one file will be used
if(length(date_idx[date_idx %in% file_1_dates]) > 0){
  nc.file <- "~/data/ERA/ERA_1979_1989.nc" 
  ERA1 <- ERA.ncdf(nc.file, date_idx)
}
if(length(date_idx[date_idx %in% file_2_dates]) > 0){
  nc.file <- "~/data/ERA/ERA_1990_1998.nc"
  ERA2 <- ERA.ncdf(nc.file, date_idx)
} 
if(length(date_idx[date_idx %in% file_3_dates]) > 0){
  nc.file <- "~/data/ERA/ERA_1999_2007.nc"
  ERA3 <- ERA.ncdf(nc.file, date_idx)
}
if(length(date_idx[date_idx %in% file_4_dates]) > 0){
  nc.file <- "~/data/ERA/ERA_2008_2016.nc"
  ERA4 <- ERA.ncdf(nc.file, date_idx)
}
# Combine multiple possible dataframes produced
# Messy but a quick enough fix
ERA_all <- data.frame()
if(exists("ERA1")) ERA_all <- rbind(ERA_all, ERA1)
if(exists("ERA2")) ERA_all <- rbind(ERA_all, ERA2)
if(exists("ERA3")) ERA_all <- rbind(ERA_all, ERA3)
if(exists("ERA4")) ERA_all <- rbind(ERA_all, ERA4)

# Test visualisation
ERA_temp <- ERA_all[ERA_all$stat == "temp", ]
colnames(ERA_temp)[3] <- "temp"
ERA_wind <- merge(ERA_all[ERA_all$stat == "u", ], ERA_all[ERA_all$stat == "v", ], by = c("x", "y"))[,c(1,2,3,5)]
ERA_scaler <- 1  # Use this to change unit. E.g., from meters per minute to meters per second.
colnames(ERA_wind) <- c("x","y","u","v")
rm(ERA_all)

## Combine data and create air-sea state figure
BRAN_temp$type <- "BRAN"
BRAN_wind$type <- "BRAN"
ERA_temp$type <- "ERA"
ERA_temp$stat <- NULL
ERA_wind$type <- "ERA"

RE_temp <- rbind(BRAN_temp, ERA_temp)
RE_wind <- rbind(BRAN_wind, ERA_wind)


all_reanalyses <- sa + geom_raster(data = RE_temp, aes(x = x, y = y, fill = temp)) +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
               fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
  geom_segment(data = RE_wind, aes(x = x, y = y, xend = x + u * BRAN_scaler, yend = y + v * BRAN_scaler),
               arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
  scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
  facet_grid(.~type) +
  labs(caption = paste0("Average air-sea state during ", event$site[1], " event #", event$event_no[1], 
                 " (", format(event$date_start, "%d %b %Y"), " - ", format(event$date_stop, "%d %b %Y"), ")")) +
  theme(plot.caption = element_text(hjust = 0.5))
all_reanalyses

## The event figure
spread <- 31
spread_clim <- filter(SACTN_clims, site == event$site & date %in% ((date_idx[1]-spread):(date_idx[length(date_idx)]+spread)))
event_clim <- filter(SACTN_clims, site == event$site & date %in% ((date_idx[1]-1):(date_idx[length(date_idx)]+1)))

event_flame <- ggplot(data = spread_clim, aes(x = date, y = temp, y2 = thresh_clim_year)) +
  geom_flame(aes(y = temp, y2 = thresh_clim_year, fill = "other"), show.legend = T) +
  geom_flame(data = event_clim, aes(y = temp, y2 = thresh_clim_year, fill = "main"), show.legend = T) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh_clim_year, colour = "thresh")) +
  geom_line(aes(y = seas_clim_year, colour = "seas")) +
  scale_colour_manual(name = "Line Colour", values = c("temp" = "black", "thresh" = "forestgreen", "seas" = "grey80")) +
  scale_fill_manual(name = "Event Colour", values = c("other" = "salmon", "main" = "red")) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  xlab("Date") + ylab("Temperature [degrees C]")
event_flame

## The info box
# All events at the chosen site
site_events <- filter(SACTN_events, site == event$site)
# The properties of the event
properties <- data.frame(txt = c(paste0("Total number of events"),
                         paste0("Duration: ", event$duration, " days"),
                         paste0("Max. intens.: ", round(event$int_max[1],1), "°C"),
                         paste0("Mean intens.: ", round(event$int_mean[1],1), "°C"),
                         paste0("Cum. intens.: ", round(event$int_cum[1],0), "°C·days"),
                         paste0("Onset rate: ", round(event$rate_onset[1],2), "°C/day"),
                         paste0("Decl. rate: ", round(event$rate_decline[1],2), "°C/day")),
                         y = rev(c(1,2,3,4,5,6,7)),
                         x = 0)
prop_text <- ggplot(data = properties, aes(x = x, y = y)) + theme_void() +
  geom_text(aes(x = x, y = y, label = txt), hjust = "left", size = 3) +
  scale_x_continuous(expand = c(0,0), limits = c(-0.01, 0.10)) +
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank())
prop_text
# The event ranked against other events from the same site
site_ranks <- data.frame(txt = c(paste0("Site: ", nrow(site_events)),
                                 paste0(which(event$duration == unique(site_events$duration[order(site_events$duration, decreasing = T)])),
                                        "/", length(unique(site_events$duration))),
                                 paste0(which(event$int_max == unique(site_events$int_max[order(site_events$int_max, decreasing = T)])),
                                        "/", length(unique(site_events$int_max))),
                                 paste0(which(event$int_mean == unique(site_events$int_mean[order(site_events$int_mean, decreasing = T)])),
                                        "/", length(unique(site_events$int_mean))),
                                 paste0(which(event$int_cum == unique(site_events$int_cum[order(site_events$int_cum, decreasing = T)])),
                                        "/", length(unique(site_events$int_cum))),
                                 paste0(which(event$rate_onset == unique(site_events$rate_onset[order(site_events$rate_onset, decreasing = T)])),
                                        "/", length(unique(site_events$rate_onset))),
                                 paste0(which(event$rate_decline == unique(site_events$rate_decline[order(site_events$rate_decline, decreasing = T)])),
                                        "/", length(unique(site_events$rate_decline)))),
                         y = rev(c(1,2,3,4,5,6,7)),
                         x = 0)
site_rank_text <- ggplot(data = site_ranks, aes(x = x, y = y)) + theme_void() +
  geom_text(aes(x = x, y = y, label = txt), hjust = "left", size = 3) +
  scale_x_continuous(expand = c(0,0), limits = c(-0.01, 0.10)) +
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank())
site_rank_text
# The event ranked against other events from the same site
all_ranks <- data.frame(txt = c(paste0("All: ", nrow(SACTN_events)),
                                 paste0(which(event$duration == unique(SACTN_events$duration[order(SACTN_events$duration, decreasing = T)])),
                                        "/", length(unique(SACTN_events$duration))),
                                 paste0(which(event$int_max == unique(SACTN_events$int_max[order(SACTN_events$int_max, decreasing = T)])),
                                        "/", length(unique(SACTN_events$int_max))),
                                 paste0(which(event$int_mean == unique(SACTN_events$int_mean[order(SACTN_events$int_mean, decreasing = T)])),
                                        "/", length(unique(SACTN_events$int_mean))),
                                 paste0(which(event$int_cum == unique(SACTN_events$int_cum[order(SACTN_events$int_cum, decreasing = T)])),
                                        "/", length(unique(SACTN_events$int_cum))),
                                 paste0(which(event$rate_onset == unique(SACTN_events$rate_onset[order(SACTN_events$rate_onset, decreasing = T)])),
                                        "/", length(unique(SACTN_events$rate_onset))),
                                 paste0(which(event$rate_decline == unique(SACTN_events$rate_decline[order(SACTN_events$rate_decline, decreasing = T)])),
                                        "/", length(unique(SACTN_events$rate_decline)))),
                         y = rev(c(1,2,3,4,5,6,7)),
                         x = 0)
all_rank_text <- ggplot(data = all_ranks, aes(x = x, y = y)) + theme_void() +
  geom_text(aes(x = x, y = y, label = txt), hjust = "left", size = 3) +
  scale_x_continuous(expand = c(0,0), limits = c(-0.01, 0.10)) +
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank())
all_rank_text

## Combine figures and save
pdf("~/Desktop/third_concept.pdf", width = 10, height = 7, pointsize = 10) # Set PDF dimensions
vp1 <- viewport(x = 0.5, y = 1.4, w = 1.00, h = 1.40, just = "top")  # Air/ Sea
vp2 <- viewport(x = 0.05, y = 0.05, w = 0.60, h = 0.35, just = c("left", "bottom")) # Flame
vp3 <- viewport(x = 0.81, y = 0.1, w = 0.17, h = 0.25, just = c("right", "bottom"))  # Event metrics
vp4 <- viewport(x = 0.87, y = 0.1, w = 0.07, h = 0.25, just = c("right", "bottom"))  # Site rank
vp5 <- viewport(x = 0.93, y = 0.1, w = 0.07, h = 0.25, just = c("right", "bottom"))  # All rank
print(all_reanalyses, vp = vp1)
print(event_flame, vp = vp2)
print(prop_text, vp = vp3)
print(site_rank_text, vp = vp4)
print(all_rank_text, vp = vp5)
dev.off()

