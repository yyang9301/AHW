#############################################################################
###"func/synoptic.fig.R"
## This script does:
# 1. Load required data
# 2. The complete function for creating synoptic figures

# 2. Load data for air-sea state during event
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
# "graph/figures3.R"
## CREATES:
# Figures similar to Eric's figures in the MHW atlas
#############################################################################


# 1. Load required data  --------------------------------------------------

# SACTN event data
load("data/events/SACTN_events.Rdata")
SACTN_events <- filter(SACTN_events, type == "MHW")
load("data/events/SACTN_clims.Rdata")
SACTN_clims <- filter(SACTN_clims, type == "MHW")
load("setupParams/SACTN_site_list.Rdata")

# Load SA map data
load("graph/southern_africa_coast.RData") # Lowres
names(southern_africa_coast)[1] <- "lon"
load("graph/sa_shore.Rdata") # Hires
names(sa_shore)[4:5] <- c("lon","lat")

# Daily air-sea state clims
if(!(exists("BRAN_temp_daily"))){
  load("data/BRAN/BRAN_temp_daily.Rdata")
}
if(!(exists("BRAN_uv_daily"))){
  load("data/BRAN/BRAN_uv_daily.Rdata")
}
if(!(exists("ERA_all_daily"))){
  load("data/ERA/ERA_all_daily.Rdata")
}

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

# Load SA bathymetry
# sa_bathy <- as.xyz(getNOAA.bathy(lon1 = sa_lons[1], lon2 = sa_lons[2], lat1 = sa_lats[1], lat2 = sa_lats[2], resolution =  4))
# colnames(sa_bathy) <- c("lon", "lat", "depth")
# save(sa_bathy, file = "data/sa_bathy.Rdata")
load("data/sa_bathy.Rdata")
sa_bathy$type = "BRAN"

# Create the base map figure
sa <- ggplot() + bw_update + coord_equal() +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), fill = "grey30", colour = "black", size = 0.1, show.legend = FALSE) +
  scale_x_continuous(limits = sa_lons, expand = c(0, 0), breaks = seq(15, 35, 5)) +
  scale_y_continuous(limits = sa_lats, expand = c(0, 0), breaks = seq(-35, -30, 5)) +
  coord_cartesian(xlim = c(10.5, 39.5), ylim = c(-39.5, -25.5)) +
  xlab("") +
  ylab("")
# sa

# BRAN loading function for single event
# event <- SACTN_events[SACTN_events$duration == min(SACTN_events$duration),][1,] # tester...
# "var" must equal "temp", "u" or "v"
# var <- "temp"
# var <- "u"
BRAN.event <- function(event, var){
  date_idx <- seq(event$date_start, event$date_stop, by = "day")
  # date_idx_2 <- format(date_idx, "%m-%d")
  var_idx <- data.frame(files = paste0("~/data/BRAN/ocean_",var,"_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"), 
                         x = length(seq(event$date_start, event$date_stop, by = "month")))
  BRAN_var <- ddply(var_idx, .(files), BRAN.Rdata, .parallel = T)
  BRAN_var <- filter(BRAN_var, date %in% date_idx)
  BRAN_var$files <- NULL
  BRAN_var <- data.table(BRAN_var)
  BRAN_var <- BRAN_var[, .(var = mean(var, na.rm = TRUE)), by = .(x,y)]
  colnames(BRAN_var)[3] <- var
  return(BRAN_var)
}

# Function for rounding event metrics for better plotting
round.metrics <- function(df){
  df <- df %>% 
    mutate(int_max = round(int_max,1)) %>% 
    mutate(int_mean = round(int_mean,1)) %>% 
    mutate(int_cum = round(int_cum,0)) %>% 
    mutate(rate_onset = round(rate_onset,2)) %>% 
    mutate(rate_decline = round(rate_decline,2))
  return(df)
}

# 2. The complete function for creating synoptic figures ------------------

# Extract longest event
# event <- SACTN_events[SACTN_events$duration == max(SACTN_events$duration),]
# event$lat <- SACTN_site_list$lat[SACTN_site_list$site == event$site]
# event$lon <- SACTN_site_list$lon[SACTN_site_list$site == event$site]

# Extract smallest event
# event <- SACTN_events[SACTN_events$duration == min(SACTN_events$duration),][1,]
# event$lat <- SACTN_site_list$lat[SACTN_site_list$site == event$site]
# event$lon <- SACTN_site_list$lon[SACTN_site_list$site == event$site]

# event <- SACTN_events[SACTN_events$duration == min(SACTN_events$duration),][1,] # tester...

synoptic.fig <- function(event){
  
  
  ## Begin by adding lon/ lat and determining date index ##
  event$lat <- SACTN_site_list$lat[SACTN_site_list$site == event$site]
  event$lon <- SACTN_site_list$lon[SACTN_site_list$site == event$site]
  date_idx <- seq(event$date_start, event$date_stop, by = "day")
  date_idx_2 <- format(date_idx, "%m-%d")
  
  
  ## Extract BRAN data during the chosen event ##
  BRAN_temp <- BRAN.event(event, "temp")
  BRAN_u <- BRAN.event(event, "u")
  BRAN_v <- BRAN.event(event, "v")
  # Create uv data frame
  BRAN_uv <- merge(BRAN_u, BRAN_v, by = c("x", "y")); rm(BRAN_u, BRAN_v)
  # Temperature anomaly
  BRAN_temp_anom <- filter(BRAN_temp_daily, date %in% date_idx_2)
  BRAN_temp_anom <- data.table(BRAN_temp_anom)
  BRAN_temp_anom <- BRAN_temp_anom[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]
  BRAN_temp_anom$temp <- BRAN_temp$temp-BRAN_temp_anom$temp
  # uv anomaly
  BRAN_uv_anom <- filter(BRAN_uv_daily, date %in% date_idx_2)
  BRAN_uv_anom <- data.table(BRAN_uv_anom)
  BRAN_uv_anom <- BRAN_uv_anom[, .(u = mean(u, na.rm = TRUE),
                                   v = mean(v, na.rm = TRUE)), by = .(x,y)]
  BRAN_uv_anom$u <- BRAN_uv$u-BRAN_uv_anom$u
  BRAN_uv_anom$v <- BRAN_uv$v-BRAN_uv_anom$v
  
  
  ## Extract ERA Interim data during this event ##
  # Run the function as necesary on the following files
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
  # Prep for plotting
  ERA_all <- ERA_all[, .(temp = mean(temp, na.rm = TRUE),
                         u = mean(u, na.rm = TRUE),
                         v = mean(v, na.rm = TRUE)), by = .(x,y)]
  # Temperature
  ERA_temp <- ERA_all[,c(1:3)]
  # UV
  ERA_uv <- ERA_all[,c(1:2,4:5)]
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
  rm(ERA_all)
  
  
  ## Create sea state figure ##
  # Double up temperatures for plotting
  BRAN_temp2 <- rbind(BRAN_temp, BRAN_temp)
  BRAN_temp2$type <- rep(c("BRAN", "BRAN_c"), each = nrow(BRAN_temp))
  # Remove some current rows for clearer plotting
  lon_sub <- seq(10, 40, by = 0.5)
  lat_sub <- seq(-40, -15, by = 0.5)
  BRAN_uv2 <- BRAN_uv[(BRAN_uv$x %in% lon_sub & BRAN_uv$y %in% lat_sub),]
  BRAN_uv2$type <- "BRAN_c"
  # The label dataframes
  BRAN_plot_data <- data_frame(txt = c("SST + Bathy", "SST + Currents", "1.0 m/s\n"),
                               x = c(25,25,25), y = c(-28,-28,-31),
                               type = c("BRAN", "BRAN_c", "BRAN_c"))
  BRAN_plot_seg <- data.frame(x = 24.5, y = -31.5, xend = 25.5, yend = -31.5, type = "BRAN_c")
  # Site location
  event2 <- rbind(event, event)
  event2$type <- c("BRAN", "BRAN_c")
  # The figure
  BRAN_state <- sa + geom_raster(data = BRAN_temp2, aes(x = x, y = y, fill = temp)) +
    stat_contour(data = sa_bathy[sa_bathy$depth < -200,], aes(x = lon, y = lat, z = depth, alpha = ..level..),
                 colour = "white", size = 0.5, binwidth = 1000, na.rm = TRUE, show.legend = FALSE) +
    geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
                 fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
    geom_segment(data = BRAN_uv2, aes(x = x, y = y, xend = x + u * 1, yend = y + v * 1),
                 arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
    geom_label(data = BRAN_plot_data, aes(x = x, y = y, label = txt)) +
    geom_segment(data = BRAN_plot_seg, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_point(data = event2, aes(x = lon, y = lat), size = 3, alpha = 0.7) +
    scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
    facet_grid(.~type) +
    theme(legend.position = "right",
          legend.direction = "vertical",
          strip.text = element_blank(),
          legend.key.height = unit(1.3, "cm"))
  # BRAN_state
  
  
  ## Create sea state anomaly figure ##
  # Double up temperatures for plotting
  BRAN_temp_anom2 <- rbind(BRAN_temp_anom, BRAN_temp_anom)
  BRAN_temp_anom2$type <- rep(c("BRAN", "BRAN_c"), each = nrow(BRAN_temp_anom))
  # Remove some uv rows for clearer plotting
  lon_sub <- seq(10, 40, by = 0.5)
  lat_sub <- seq(-40, -15, by = 0.5)
  BRAN_uv_anom2 <- BRAN_uv_anom[(BRAN_uv_anom$x %in% lon_sub & BRAN_uv_anom$y %in% lat_sub),]
  BRAN_uv_anom2$type <- "BRAN_c"
  # The label dataframes
  BRAN_plot_anom_data <- data_frame(txt = c("SST + Bathy\nAnomaly", "SST + Currents\nAnomaly", "1.0 m/s\n"),
                                    x = c(25,25,25), y = c(-28,-28,-31),
                                    type = c("BRAN", "BRAN_c", "BRAN_c"))
  BRAN_plot_anom_seg <- data.frame(x = 24.5, y = -31.5, xend = 25.5, yend = -31.5, type = "BRAN_c")
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
    geom_point(data = event2, aes(x = lon, y = lat), size = 3, alpha = 0.7) +
    scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
    facet_grid(.~type) +
    theme(legend.position = "right",
          legend.direction = "vertical",
          strip.text = element_blank(),
          legend.key.height = unit(1.3, "cm"))
  # BRAN_state_anom
  
  ## Create air state figure ##
  # ERA_temp2 <- ERA_temp
  # Remove some uv rows for clearer plotting
  lon_sub <- seq(10, 40, by = 1)
  lat_sub <- seq(-40, -15, by = 1)
  ERA_uv2 <- ERA_uv[(ERA_uv$x %in% lon_sub & ERA_uv$y %in% lat_sub),]
  # The label dataframes
  ERA_plot_data <- data_frame(txt = c("Air temp + Winds", "4.0 m/s\n"),
                              x = c(25,25), y = c(-28,-31))
  ERA_plot_seg <- data.frame(x = 24, y = -31.5, xend = 26, yend = -31.5)
  # The figure
  ERA_state <- sa + geom_raster(data = ERA_temp, aes(x = x, y = y, fill = temp)) +
    geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
                 fill = NA, colour = "black", size = 0.1, show.legend = FALSE) +
    geom_segment(data = ERA_uv2, aes(x = x, y = y, xend = x + u * 0.5, yend = y + v * 0.5),
                 arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
    geom_label(data = ERA_plot_data, aes(x = x, y = y, label = txt)) +
    geom_segment(data = ERA_plot_seg, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_point(data = event2, aes(x = lon, y = lat), size = 3, alpha = 0.7) +
    scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
    theme(legend.position = "right",
          legend.direction = "vertical",
          strip.text = element_blank(),
          legend.key.height = unit(1.3, "cm"))
  # ERA_state
  
  
  ## Create air state anomaly figure ##
  # Remove some wind rows for clearer plotting
  lon_sub <- seq(10, 40, by = 1)
  lat_sub <- seq(-40, -15, by = 1)
  ERA_uv_anom2 <- ERA_uv_anom[(ERA_uv_anom$x %in% lon_sub & ERA_uv_anom$y %in% lat_sub),]
  # The label dataframes
  ERA_plot_anom_data <- data_frame(txt = c("Air temp + Winds\nAnomaly", "4.0 m/s\n"),
                                   x = c(25,25), y = c(-28,-31))
  ERA_plot_anom_seg <- data.frame(x = 24, y = -31.5, xend = 26, yend = -31.5)
  # The figure
  ERA_state_anom <- sa + geom_raster(data = ERA_temp_anom, aes(x = x, y = y, fill = temp)) +
    geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
                 fill = NA, colour = "black", size = 0.1, show.legend = FALSE) +
    geom_segment(data = ERA_uv_anom2, aes(x = x, y = y, xend = x + u * 0.5, yend = y + v * 0.5),
                 arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
    geom_label(data = ERA_plot_anom_data, aes(x = x, y = y, label = txt)) +
    geom_segment(data = ERA_plot_anom_seg, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_point(data = event2, aes(x = lon, y = lat), size = 3, alpha = 0.7) +
    scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
    theme(legend.position = "right",
          legend.direction = "vertical",
          strip.text = element_blank(),
          legend.key.height = unit(1.3, "cm"))
  # ERA_state_anom
  
  
  ## The event figure ##
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
    xlab("Date") + ylab(expression(paste("Temperature (",degree,"C)"))) +
    ggtitle(paste0("Average air-sea state during ", event$site[1], " event #", event$event_no[1], 
                   " (", format(event$date_start, "%d %b %Y"), " - ", format(event$date_stop, "%d %b %Y"), ")")) +
    theme(plot.title = element_text(hjust = 0.5))
  # event_flame
  
  
  ## The info boxes ##
  # All events at the chosen site
  site_events <- filter(SACTN_events, site == event$site)
  # Create data frames of rounded values for easier comparisons
  event_round <- round.metrics(event)
  site_round <- round.metrics(site_events)
  all_round <- round.metrics(SACTN_events)
  # The properties of the event
  properties <- data.frame(txt = c(paste0("Total number of events"),
                                   paste0("Duration: ", event_round$duration, " days"),
                                   paste0("Max. intens.: ", event_round$int_max[1], "°C"),
                                   paste0("Mean intens.: ", event_round$int_mean[1], "°C"),
                                   paste0("Cum. intens.: ", event_round$int_cum[1], "°C·days"),
                                   paste0("Onset rate: ", event_round$rate_onset[1], "°C/day"),
                                   paste0("Decl. rate: ", event_round$rate_decline[1], "°C/day")),
                           y = rev(c(1,2,3,4,5,6,7)),
                           x = 0)
  prop_text <- ggplot(data = properties, aes(x = x, y = y)) + theme_void() +
    geom_text(aes(x = x, y = y, label = txt), hjust = "left", size = 3) +
    scale_x_continuous(expand = c(0,0), limits = c(-0.01, 0.10)) +
    theme(axis.ticks = element_blank(),
          panel.grid.major = element_blank())
  # prop_text
  # The event ranked against other events from the same site
  site_ranks <- data.frame(txt = c(paste0("Site: ", nrow(site_events)),
                                   paste0(which(event_round$duration == unique(site_round$duration[order(site_round$duration, decreasing = T)])),
                                          "/", length(unique(site_round$duration))),
                                   paste0(which(event_round$int_max == unique(site_round$int_max[order(site_round$int_max, decreasing = T)])),
                                          "/", length(unique(site_round$int_max))),
                                   paste0(which(event_round$int_mean == unique(site_round$int_mean[order(site_round$int_mean, decreasing = T)])),
                                          "/", length(unique(site_round$int_mean))),
                                   paste0(which(event_round$int_cum == unique(site_round$int_cum[order(site_round$int_cum, decreasing = T)])),
                                          "/", length(unique(site_round$int_cum))),
                                   paste0(which(event_round$rate_onset == unique(site_round$rate_onset[order(site_round$rate_onset, decreasing = T)])),
                                          "/", length(unique(site_round$rate_onset))),
                                   paste0(which(event_round$rate_decline == unique(site_round$rate_decline[order(site_round$rate_decline, decreasing = T)])),
                                          "/", length(unique(site_round$rate_decline)))),
                           y = rev(c(1,2,3,4,5,6,7)),
                           x = 0)
  site_rank_text <- ggplot(data = site_ranks, aes(x = x, y = y)) + theme_void() +
    geom_text(aes(x = x, y = y, label = txt), hjust = "left", size = 3) +
    scale_x_continuous(expand = c(0,0), limits = c(-0.01, 0.10)) +
    theme(axis.ticks = element_blank(),
          panel.grid.major = element_blank())
  # site_rank_text
  # The event ranked against other events from the same site
  all_ranks <- data.frame(txt = c(paste0("All: ", nrow(SACTN_events)),
                                  paste0(which(event_round$duration == unique(all_round$duration[order(all_round$duration, decreasing = T)])),
                                         "/", length(unique(all_round$duration))),
                                  paste0(which(event_round$int_max == unique(all_round$int_max[order(all_round$int_max, decreasing = T)])),
                                         "/", length(unique(all_round$int_max))),
                                  paste0(which(event_round$int_mean == unique(all_round$int_mean[order(all_round$int_mean, decreasing = T)])),
                                         "/", length(unique(all_round$int_mean))),
                                  paste0(which(event_round$int_cum == unique(all_round$int_cum[order(all_round$int_cum, decreasing = T)])),
                                         "/", length(unique(all_round$int_cum))),
                                  paste0(which(event_round$rate_onset == unique(all_round$rate_onset[order(all_round$rate_onset, decreasing = T)])),
                                         "/", length(unique(all_round$rate_onset))),
                                  paste0(which(event_round$rate_decline == unique(all_round$rate_decline[order(all_round$rate_decline, decreasing = T)])),
                                         "/", length(unique(all_round$rate_decline)))),
                          y = rev(c(1,2,3,4,5,6,7)),
                          x = 0)
  all_rank_text <- ggplot(data = all_ranks, aes(x = x, y = y)) + theme_void() +
    geom_text(aes(x = x, y = y, label = txt), hjust = "left", size = 3) +
    scale_x_continuous(expand = c(0,0), limits = c(-0.01, 0.10)) +
    theme(axis.ticks = element_blank(),
          panel.grid.major = element_blank())
  # all_rank_text
  
  ## Combine figures and save ##
  # Generate file name
  file_name <- paste0("graph/synoptic/",event$site[1],"_",event$event_no[1],".pdf")
  # Compile final figure
  pdf(file_name, width = 17, height = 12, pointsize = 10) # Set PDF dimensions
  vp1 <- viewport(x = 0.02, y = 0.95, w = 0.61, h = 0.25, just = c("left", "top"))  # Sea
  vp2 <- viewport(x = 0.02, y = 0.70, w = 0.61, h = 0.25, just = c("left", "top"))  # Sea anomaly
  vp3 <- viewport(x = 0.98, y = 0.95, w = 0.35, h = 0.25, just = c("right", "top"))  # Air
  vp4 <- viewport(x = 0.98, y = 0.70, w = 0.35, h = 0.25, just = c("right", "top"))  # Air anomaly
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
  
}


