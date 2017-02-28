#############################################################################
###"graph/figures3.R"
## This script does:
# 1. Load required data
# 2. Load data for sea/ air state during event
# 3. Create sea state figure
# 4. Create sea state anomaly figure
# 5. Create air state figure
# 6. Create air state anomaly figure
# 7. The event figure
# 8. The info boxes
# 9. Combine figures and save
## DEPENDS ON:
library(PBSmapping)
library(marmap)
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


# 1. Load required data  --------------------------------------------------

load("data/events/SACTN_events.Rdata")
SACTN_events <- filter(SACTN_events, type == "MHW")
load("data/events/SACTN_clims.Rdata")
SACTN_clims <- filter(SACTN_clims, type == "MHW")
load("setupParams/SACTN_site_list.Rdata")

# Frequency ploygons of event duration
# ggplot(data = SACTN_events, aes(x = duration, group = site)) +
#   geom_freqpoly(binwidth = 5)
# SACTN_events2 <- filter(SACTN_events, duration >= 15)
# levels(as.factor(SACTN_events2$site))

# Load SA map data
load("graph/southern_africa_coast.RData") # Lowres
names(southern_africa_coast)[1] <- "lon"
load("graph/sa_shore.Rdata") # Hires
names(sa_shore)[4:5] <- c("lon","lat")

# Daily air-sea state clims
load("data/BRAN/BRAN_temp_daily.Rdata")
load("data/BRAN/BRAN_uv_daily.Rdata")
load("data/ERA/ERA_all_daily.Rdata")
# ERA_temp_daily <- ERA_all_daily[,1:4]
# ERA_uv_daily <-  ERA_all_daily[,c(1:3,5:6)]

#ERA Interim file indices
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

# Load SA bathymetry
# sa_bathy <- as.xyz(getNOAA.bathy(lon1 = sa_lons[1], lon2 = sa_lons[2], lat1 = sa_lats[1], lat2 = sa_lats[2], resolution =  4))
# colnames(sa_bathy) <- c("lon", "lat", "depth")
# save(sa_bathy, file = "data/sa_bathy.Rdata")
load("data/sa_bathy.Rdata")

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


# 2. Load data for sea/ air state during event ----------------------------

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
date_idx_2 <- format(date_idx, "%m-%d")

temp_idx <- data.frame(files = paste0("~/data/BRAN/ocean_temp_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"), 
                       x = length(seq(event$date_start, event$date_stop, by = "month")))
u_idx <- data.frame(files = paste0("~/data/BRAN/ocean_u_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"), 
                    x = length(seq(event$date_start, event$date_stop, by = "month")))
v_idx <- data.frame(files = paste0("~/data/BRAN/ocean_v_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"), 
                    x = length(seq(event$date_start, event$date_stop, by = "month")))

### Load the BRAN data
## Temperature
system.time(BRAN_temp <- ddply(temp_idx, .(files), BRAN.Rdata, .parallel = T)) # ~5 seconds for one file
BRAN_temp <- filter(BRAN_temp, date %in% date_idx)
BRAN_temp$files <- NULL
colnames(BRAN_temp)[3] <- "temp"
BRAN_temp <- data.table(BRAN_temp)
BRAN_temp <- BRAN_temp[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]
# Anomaly
BRAN_temp_anom <- filter(BRAN_temp_daily, date %in% date_idx_2)
BRAN_temp_anom <- data.table(BRAN_temp_anom)
BRAN_temp_anom <- BRAN_temp_anom[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]
BRAN_temp_anom$temp <- BRAN_temp$temp-BRAN_temp_anom$temp

# U value
system.time(BRAN_u <- ddply(u_idx, .(files), BRAN.Rdata, .parallel = T)) # ~5 seconds for one file
BRAN_u <- filter(BRAN_u, date %in% date_idx)
BRAN_u$files <- NULL
colnames(BRAN_u)[3] <- "u"
BRAN_u <- data.table(BRAN_u)
BRAN_u <- BRAN_u[, .(u = mean(u, na.rm = TRUE)), by = .(x,y)]

# V value
system.time(BRAN_v <- ddply(v_idx, .(files), BRAN.Rdata, .parallel = T)) # ~5 seconds for one file
BRAN_v <- filter(BRAN_v, date %in% date_idx)
BRAN_v$files <- NULL
colnames(BRAN_v)[3] <- "v"
BRAN_v <- data.table(BRAN_v)
BRAN_v <- BRAN_v[, .(v = mean(v, na.rm = TRUE)), by = .(x,y)]

# Create uv data frame
BRAN_uv <- merge(BRAN_u, BRAN_v, by = c("x", "y"))#; rm(BRAN_u, BRAN_v)
# Anomaly
BRAN_uv_anom <- filter(BRAN_uv_daily, date %in% date_idx_2)
BRAN_uv_anom <- data.table(BRAN_uv_anom)
BRAN_uv_anom <- BRAN_uv_anom[, .(u = mean(u, na.rm = TRUE),
                                   v = mean(v, na.rm = TRUE)), by = .(x,y)]
BRAN_uv_anom$u <- BRAN_uv$u-BRAN_uv_anom$u
BRAN_uv_anom$v <- BRAN_uv$v-BRAN_uv_anom$v


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

ERA_all <- ERA_all[, .(temp = mean(temp, na.rm = TRUE),
                       u = mean(u, na.rm = TRUE),
                       v = mean(v, na.rm = TRUE)), by = .(x,y)]
# # Temperature
ERA_temp <- ERA_all[,c(1:3)]
# # UV
ERA_uv <- ERA_all[,c(1:2,4:5)]
# rm(ERA_all)
# Anomalies
ERA_all_anom <- filter(ERA_all_daily, date %in% date_idx_2)
ERA_all_anom <- data.table(ERA_all_anom)
ERA_all_anom <- ERA_all_anom[, .(temp = mean(temp, na.rm = TRUE),
                                 u = mean(u, na.rm = TRUE),
                                 v = mean(v, na.rm = TRUE)), by = .(x,y)]
ERA_all_anom$temp <- ERA_all$temp-ERA_all_anom$temp
ERA_all_anom$u <- ERA_all$u-ERA_all_anom$u
ERA_all_anom$v <- ERA_all$v-ERA_all_anom$v
# It is necessary to split temp from uv so that the winds may be subsetted further
ERA_temp_anom <- ERA_all_anom[,c(1:3)]
ERA_uv_anom <- ERA_all_anom[,c(1:2,4:5)]


# 3. Create sea state figure ----------------------------------------------

# Double up temperatures for plotting
BRAN_temp2 <- rbind(BRAN_temp, BRAN_temp)
BRAN_temp2$type <- rep(c("BRAN", "BRAN_c"), each = nrow(BRAN_temp))

# Remove some current rows for clearer plotting
BRAN_uv2 <- BRAN_uv
BRAN_uv2$u <- BRAN_uv2$u*rep(c(1,NA), each = 1) # Thes following warnings may be disregarded
BRAN_uv2$v <- BRAN_uv2$v*rep(c(1,NA), each = 1)
BRAN_uv2 <- BRAN_uv2[complete.cases(BRAN_uv2$u)]
# Repeat to reduce vectors further
BRAN_uv2$u <- BRAN_uv2$u*rep(c(1,NA), each = 1)
BRAN_uv2$v <- BRAN_uv2$v*rep(c(1,NA), each = 1)
BRAN_uv2 <- BRAN_uv2[complete.cases(BRAN_uv2$u)]
# Repeat to reduce vectors further
BRAN_uv2$u <- BRAN_uv2$u*rep(c(1,NA), each = 1)
BRAN_uv2$v <- BRAN_uv2$v*rep(c(1,NA), each = 1)
BRAN_uv2 <- BRAN_uv2[complete.cases(BRAN_uv2$u)]
BRAN_uv2$type <- "BRAN_c"

# The label dataframes
BRAN_plot_data <- data_frame(txt = c("SST + Bathy", "SST + Currents", "1.0 m/s\n"),
                             x = c(25,25,25), y = c(-28,-28,-31),
                             type = c("BRAN", "BRAN_c", "BRAN_c"))
BRAN_plot_seg <- data.frame(x = 24, y = -31.5, xend = 26, yend = -31.5, type = "BRAN_c")

# Bathy
sa_bathy$type = "BRAN"
# bathy$type = "BRAN"

# Site location
event$type = "BRAN"

# The figure
BRAN_state <- sa + geom_raster(data = BRAN_temp2, aes(x = x, y = y, fill = temp)) +
  stat_contour(data = sa_bathy[sa_bathy$depth < -200,], aes(x = lon, y = lat, z = depth, alpha = ..level..),
               colour = "white", size = 0.5, binwidth = 1000, na.rm = TRUE, show.legend = FALSE) +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
               fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
  geom_segment(data = BRAN_uv2, aes(x = x, y = y, xend = x + u * 2, yend = y + v * 2),
               arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
  geom_label(data = BRAN_plot_data, aes(x = x, y = y, label = txt)) +
  geom_segment(data = BRAN_plot_seg, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_point(data = event, aes(x = lon, y = lat), size = 2, alpha = 0.6) +
  scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
  facet_grid(.~type) +
  guides(fill = guide_legend(keyheight = 4)) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        strip.text = element_blank())
BRAN_state


# 4. Create sea state anomaly figure --------------------------------------
## NB: Currently using normal temperatures as anomalies have not yet been calculated
# Double up temperatures for plotting

BRAN_temp_anom2 <- rbind(BRAN_temp_anom, BRAN_temp_anom)
BRAN_temp_anom2$type <- rep(c("BRAN", "BRAN_c"), each = nrow(BRAN_temp_anom))

# Remove some uv rows for clearer plotting
BRAN_uv_anom2 <- BRAN_uv_anom
BRAN_uv_anom2$u <- BRAN_uv_anom2$u*rep(c(1,NA), each = 1) # Thes following warnings may be disregarded
BRAN_uv_anom2$v <- BRAN_uv_anom2$v*rep(c(1,NA), each = 1)
BRAN_uv_anom2 <- BRAN_uv_anom2[complete.cases(BRAN_uv_anom2$u)]
# Repeat to reduce vectors further
BRAN_uv_anom2$u <- BRAN_uv_anom2$u*rep(c(1,NA), each = 1)
BRAN_uv_anom2$v <- BRAN_uv_anom2$v*rep(c(1,NA), each = 1)
BRAN_uv_anom2 <- BRAN_uv_anom2[complete.cases(BRAN_uv_anom2$u)]
# Repeat to reduce vectors further
BRAN_uv_anom2$u <- BRAN_uv_anom2$u*rep(c(1,NA), each = 1)
BRAN_uv_anom2$v <- BRAN_uv_anom2$v*rep(c(1,NA), each = 1)
BRAN_uv_anom2 <- BRAN_uv_anom2[complete.cases(BRAN_uv_anom2$u)]
BRAN_uv_anom2$type <- "BRAN_c"

# The label dataframes
BRAN_plot_anom_data <- data_frame(txt = c("SST + Bathy\nAnomaly", "SST + currents\nAnomaly", "1.0 m/s\n"),
                             x = c(25,25,25), y = c(-28,-28,-31),
                             type = c("BRAN", "BRAN_c", "BRAN_c"))
BRAN_plot_anom_seg <- data.frame(x = 24, y = -31.5, xend = 26, yend = -31.5, type = "BRAN_c")

# Bathy
sa_bathy$type = "BRAN"
# bathy$type = "BRAN"

# Site location
event$type = "BRAN"

# The figure
BRAN_state_anom <- sa + geom_raster(data = BRAN_temp_anom2, aes(x = x, y = y, fill = temp)) +
  stat_contour(data = sa_bathy[sa_bathy$depth < -200,], aes(x = lon, y = lat, z = depth, alpha = ..level..),
               colour = "white", size = 0.5, binwidth = 1000, na.rm = TRUE, show.legend = FALSE) +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
               fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
  geom_segment(data = BRAN_uv_anom2, aes(x = x, y = y, xend = x + u * 2, yend = y + v * 2),
               arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
  geom_label(data = BRAN_plot_anom_data, aes(x = x, y = y, label = txt)) +
  geom_segment(data = BRAN_plot_anom_seg, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_point(data = event, aes(x = lon, y = lat), size = 2, alpha = 0.6) +
  scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
  facet_grid(.~type) +
  guides(fill = guide_legend(keyheight = 4)) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        strip.text = element_blank())
BRAN_state_anom


# 5. Create air state figure ----------------------------------------------

ERA_temp2 <- ERA_temp

# Remove some uv rows for clearer plotting
ERA_uv2 <- ERA_uv
ERA_uv2$u <- ERA_uv2$u*rep(c(1,NA), each = 1)
ERA_uv2$v <- ERA_uv2$v*rep(c(1,NA), each = 1)
ERA_uv2 <- ERA_uv2[complete.cases(ERA_uv2$u)]
# Repeat to reduce vectors further
# ERA_uv2$u <- ERA_uv2$u*rep(c(1,NA), each = 1)
# ERA_uv2$v <- ERA_uv2$v*rep(c(1,NA), each = 1)
# ERA_uv2 <- ERA_uv2[complete.cases(ERA_uv2$u)]
# ERA_uv2$type <- "ERA_c"

# The label dataframes
ERA_plot_data <- data_frame(txt = c("Air temp + winds", "4.0 m/s\n"),
                             x = c(25,25), y = c(-28,-31))
ERA_plot_seg <- data.frame(x = 24, y = -31.5, xend = 26, yend = -31.5)

# The figure
ERA_state <- sa + geom_raster(data = ERA_temp2, aes(x = x, y = y, fill = temp)) +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
               fill = NA, colour = "black", size = 0.1, show.legend = FALSE) +
  geom_segment(data = ERA_uv2, aes(x = x, y = y, xend = x + u * 0.5, yend = y + v * 0.5),
               arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
  geom_label(data = ERA_plot_data, aes(x = x, y = y, label = txt)) +
  geom_segment(data = ERA_plot_seg, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
  guides(fill = guide_legend(keyheight = 4)) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        strip.text = element_blank())
ERA_state


# 6. Create air state anomaly figure --------------------------------------

ERA_temp_anom2 <- ERA_temp_anom

# Remove some wind rows for clearer plotting
ERA_uv_anom2 <- ERA_uv_anom
ERA_uv_anom2$u <- ERA_uv_anom2$u*rep(c(1,NA), each = 1)
ERA_uv_anom2$v <- ERA_uv_anom2$v*rep(c(1,NA), each = 1)
ERA_uv_anom2 <- ERA_uv_anom2[complete.cases(ERA_uv_anom2$u)]
# Repeat to reduce vectors further
# ERA_uv_anom2$u <- ERA_uv_anom2$u*rep(c(1,NA), each = 1)
# ERA_uv_anom2$v <- ERA_uv_anom2$v*rep(c(1,NA), each = 1)
# ERA_uv_anom2 <- ERA_uv_anom2[complete.cases(ERA_uv_anom2$u)]
# ERA_uv_anom2$type <- "ERA_c"

# The label dataframes
ERA_plot_anom_data <- data_frame(txt = c("Air temp + Winds\nAnomaly", "4.0 m/s\n"),
                            x = c(25,25), y = c(-28,-31))
ERA_plot_anom_seg <- data.frame(x = 24, y = -31.5, xend = 26, yend = -31.5)

# The figure
ERA_state_anom <- sa + geom_raster(data = ERA_temp_anom2, aes(x = x, y = y, fill = temp)) +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
               fill = NA, colour = "black", size = 0.1, show.legend = FALSE) +
  geom_segment(data = ERA_uv_anom2, aes(x = x, y = y, xend = x + u * 0.5, yend = y + v * 0.5),
               arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
  geom_label(data = ERA_plot_anom_data, aes(x = x, y = y, label = txt)) +
  geom_segment(data = ERA_plot_anom_seg, aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
  guides(fill = guide_legend(keyheight = 4)) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        strip.text = element_blank())
ERA_state_anom


# 7. The event figure -----------------------------------------------------

# Determine spread and subset days
spread <- 31
spread_clim <- filter(SACTN_clims, site == event$site & date %in% ((date_idx[1]-spread):(date_idx[length(date_idx)]+spread)))
event_clim <- filter(SACTN_clims, site == event$site & date %in% ((date_idx[1]-1):(date_idx[length(date_idx)]+1)))

# The figure
event_flame <- ggplot(data = spread_clim, aes(x = date, y = temp, y2 = thresh_clim_year)) +
  geom_flame(aes(y = temp, y2 = thresh_clim_year, fill = "other"), show.legend = T) +
  geom_flame(data = event_clim, aes(y = temp, y2 = thresh_clim_year, fill = "main"), show.legend = T) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh_clim_year, colour = "thresh")) +
  geom_line(aes(y = seas_clim_year, colour = "seas")) +
  scale_colour_manual(name = "Line Colour", values = c("temp" = "black", "thresh" = "forestgreen", "seas" = "grey80")) +
  scale_fill_manual(name = "Event Colour", values = c("other" = "salmon", "main" = "red")) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  xlab("Date") + ylab("Temperature [degrees C]") +
  ggtitle(paste0("Average air-sea state during ", event$site[1], " event #", event$event_no[1], 
                        " (", format(event$date_start, "%d %b %Y"), " - ", format(event$date_stop, "%d %b %Y"), ")")) +
  theme(plot.title = element_text(hjust = 0.5))
event_flame


# 8. The info boxes -------------------------------------------------------

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


# 9. Combine figures and save ---------------------------------------------

pdf("~/Desktop/fifth_concept.pdf", width = 17, height = 12, pointsize = 10) # Set PDF dimensions
vp1 <- viewport(x = 0.02, y = 1.3, w = 0.60, h = 1.0, just = c("left", "top"))  # Sea
vp2 <- viewport(x = 0.02, y = 1.05, w = 0.60, h = 1.0, just = c("left", "top"))  # Sea anomaly
vp3 <- viewport(x = 0.98, y = 1.15, w = 0.35, h = 0.7, just = c("right", "top"))  # Air
vp4 <- viewport(x = 0.98, y = 0.90, w = 0.35, h = 0.7, just = c("right", "top"))  # Air anomaly
vp5 <- viewport(x = 0.05, y = 0.05, w = 0.60, h = 0.35, just = c("left", "bottom")) # Flame
vp6 <- viewport(x = 0.77, y = 0.09, w = 0.12, h = 0.25, just = c("right", "bottom"))  # Event metrics
vp7 <- viewport(x = 0.82, y = 0.09, w = 0.05, h = 0.25, just = c("right", "bottom"))  # Site rank
vp8 <- viewport(x = 0.87, y = 0.09, w = 0.05, h = 0.25, just = c("right", "bottom"))  # All rank
print(BRAN_state, vp = vp1)
print(BRAN_state_anom, vp = vp2)
print(ERA_state, vp = vp3)
print(ERA_state_anom, vp = vp4)
print(event_flame, vp = vp5)
print(prop_text, vp = vp6)
print(site_rank_text, vp = vp7)
print(all_rank_text, vp = vp8)
dev.off()

