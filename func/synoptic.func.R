#############################################################################
###"func/synoptic.func.R"
## This script does:
# 1. Function that loads only necessary BRAN files
# 2. A function that creates synoptic data packets for MHWs
# 3. Function for rounding event metrics for better plotting
# 4. ggplot2 code that creates synoptic panels
# 5. Function for creating synoptic figures
## DEPENDS ON:
library(tidyverse)
library(gridExtra)
library(scales)
library(viridis)
library(cowplot)
library(ggpubr)
library(RmarineHeatWaves)
source("func/load.reanalyses.R")
source("func/load.remote.R")
## USED BY:
# "2.Model_fitting.R"
# "3.Figures.R"
## CREATES:
# A synoptic data packet for each event
# A synoptic MHW atlas figure for each event
#############################################################################

# The default lon/ lat ranges
wlon <- 10
elon <- 40
nlat <- -25
slat <- -40

sa_lons <- c(10, 40); sa_lats <- c(-40, -25)

# 1. Function that loads only necessary OISST files ------------------------

# OISST loading function for single event
# event <- SACTN_events[SACTN_events$duration == min(SACTN_events$duration),][1,] # tester...
# event <- SACTN_events[1,]
# event <- SACTN_events[58,]
OISST.event <- function(event, var){
  date_idx <- seq(event$date_start, event$date_stop, by = "day")
  file_pattern <- gsub("-", "", as.character(date_idx))
  var_idx <- data.frame(files = paste0("~/data/OISST/netCDF/avhrr-only-v2.", file_pattern,".nc"), 
                        x = 1:length(date_idx))
  ### January 14th, 2016 is missing in the OISST data ###
    ### This affects Port Nolloth 78 ###
  var_idx <- filter(var_idx, files != "~/data/OISST/netCDF/avhrr-only-v2.20160114.nc")
  ###
  OISST <- plyr::ddply(var_idx, c("files"), OISST.daily, .parallel = T) %>% 
    mutate(date = as.Date(date)) %>% 
    select(-files) %>% 
    rename(temp = sst)
  OISST <- OISST[complete.cases(OISST$temp),]
  OISST <- data.table::data.table(OISST)
  OISST <- OISST[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]
  OISST <- OISST[order(OISST$x),]
  return(OISST)
}


# 2. A function that creates synoptic data packets for MHWs ---------------

data.packet <- function(event){
  
  ## Begin by adding lon/ lat and determining date index ##
  # event <- SACTN_events[23,]
  event2 <- event
  event2$lat <- SACTN_site_list$lat[SACTN_site_list$site == event$site]
  event2$lon <- SACTN_site_list$lon[SACTN_site_list$site == event$site]
  event2$type <- NULL
  date_idx <- seq(event$date_start, event$date_stop, by = "day")
  date_idx_2 <- format(date_idx, "%m-%d")
  
  
  ## Extract OISST data during the chosen event ##
  OISST_temp <- OISST.event(event, "temp")
  OISST_temp <- OISST_temp[order(OISST_temp$x),]
  # Temperature anomaly
  OISST_temp_anom <- filter(OISST_temp_clim, date %in% date_idx_2)
  OISST_temp_anom <- data.table::data.table(OISST_temp_anom)
  OISST_temp_anom <- OISST_temp_anom[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]
  OISST_temp_anom <- OISST_temp_anom[order(OISST_temp_anom$x),]
  OISST_temp_anom$temp <- OISST_temp$temp-OISST_temp_anom$temp
  
  
  ## Extract AVISO data during the chosen event ##
  # Run the function as necesary on the following files
  # NB: There is a slight possibility that more than one file will be used
  if(length(date_idx[date_idx %in% AVISO_1_dates]) > 0){
    nc.file <- "~/data/AVISO/dataset-duacs-rep-global-merged-allsat-phy-l4-v3_19930101-19991231.nc"
    AVISO1 <- AVISO.ncdf(nc.file, date_idx)
  } 
  if(length(date_idx[date_idx %in% AVISO_2_dates]) > 0){
    nc.file <- "~/data/AVISO/dataset-duacs-rep-global-merged-allsat-phy-l4-v3_20000101-20091231.nc"
    AVISO2 <- AVISO.ncdf(nc.file, date_idx)
  }
  if(length(date_idx[date_idx %in% AVISO_3_dates]) > 0){
    nc.file <- "~/data/AVISO/dataset-duacs-rep-global-merged-allsat-phy-l4-v3_20100101-20170106.nc"
    AVISO3 <- AVISO.ncdf(nc.file, date_idx)
  }
  # Combine multiple possible dataframes produced
  AVISO_uv <- data.frame()
  # AVISO1 not loaded as these dates precede any available BRAN data
  if(exists("AVISO1")) AVISO_uv <- rbind(AVISO_uv, AVISO1)
  if(exists("AVISO2")) AVISO_uv <- rbind(AVISO_uv, AVISO2)
  if(exists("AVISO3")) AVISO_uv <- rbind(AVISO_uv, AVISO3)
  # Create mean values
  AVISO_uv <- data.table::data.table(AVISO_uv)
  AVISO_uv <- AVISO_uv[, .(u = mean(u, na.rm = TRUE),
                         v = mean(v, na.rm = TRUE)), by = .(x,y)]
  # UV
  # AVISO_uv <- AVISO_all[,c(1:2,4:5)]
  AVISO_uv <- AVISO_uv[order(AVISO_uv$x),]
  # UV anomaly
  AVISO_uv_anom <- filter(AVISO_uv_clim, date %in% date_idx_2)
  AVISO_uv_anom <- data.table::data.table(AVISO_uv_anom)
  AVISO_uv_anom <- AVISO_uv_anom[, .(u = mean(u, na.rm = TRUE),
                                 v = mean(v, na.rm = TRUE)), by = .(x,y)]
  AVISO_uv_anom <- AVISO_uv_anom[order(AVISO_uv_anom$x),]
  AVISO_uv_anom$u <- AVISO_uv$u-AVISO_uv_anom$u
  AVISO_uv_anom$v <- AVISO_uv$v-AVISO_uv_anom$v
  
  
  ## Extract ERA Interim data during this event ##
  # Run the function as necesary on the following files
  # NB: There is a slight possibility that more than one file will be used
    # ERA1 not loaded as these dates precede any available BRAN data
  if(length(date_idx[date_idx %in% ERA_2_dates]) > 0){
    nc.file <- "~/data/ERA/ERA_1990_1998.nc"
    ERA2 <- ERA.ncdf(nc.file, date_idx)
  } 
  if(length(date_idx[date_idx %in% ERA_3_dates]) > 0){
    nc.file <- "~/data/ERA/ERA_1999_2007.nc"
    ERA3 <- ERA.ncdf(nc.file, date_idx)
  }
  if(length(date_idx[date_idx %in% ERA_4_dates]) > 0){
    nc.file <- "~/data/ERA/ERA_2008_2016.nc"
    ERA4 <- ERA.ncdf(nc.file, date_idx)
  }
  # Combine multiple possible dataframes produced
  ERA_all <- data.frame()
    # ERA1 not loaded as these dates precede any available BRAN data
  if(exists("ERA2")) ERA_all <- rbind(ERA_all, ERA2)
  if(exists("ERA3")) ERA_all <- rbind(ERA_all, ERA3)
  if(exists("ERA4")) ERA_all <- rbind(ERA_all, ERA4)
  # Create mean values
  ERA_all <- data.table::data.table(ERA_all)
  ERA_all <- ERA_all[, .(temp = mean(temp, na.rm = TRUE),
                         u = mean(u, na.rm = TRUE),
                         v = mean(v, na.rm = TRUE)), by = .(x,y)]
  # Temperature
  ERA_temp <- ERA_all[,c(1:3)]
  ERA_temp <- ERA_temp[order(ERA_temp$x),]
  # UV
  ERA_uv <- ERA_all[,c(1:2,4:5)]
  ERA_uv <- ERA_uv[order(ERA_uv$x),]
  # Temperature anomaly
  ERA_temp_anom <- filter(ERA_temp_clim, date %in% date_idx_2)
  ERA_temp_anom <- data.table::data.table(ERA_temp_anom)
  ERA_temp_anom <- ERA_temp_anom[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]
  ERA_temp_anom <- ERA_temp_anom[order(ERA_temp_anom$x),]
  ERA_temp_anom$temp <- ERA_temp$temp-ERA_temp_anom$temp
  # UV anomaly
  ERA_uv_anom <- filter(ERA_uv_clim, date %in% date_idx_2)
  ERA_uv_anom <- data.table::data.table(ERA_uv_anom)
  ERA_uv_anom <- ERA_uv_anom[, .(u = mean(u, na.rm = TRUE),
                                 v = mean(v, na.rm = TRUE)), by = .(x,y)]
  ERA_uv_anom <- ERA_uv_anom[order(ERA_uv_anom$x),]
  ERA_uv_anom$u <- ERA_uv$u-ERA_uv_anom$u
  ERA_uv_anom$v <- ERA_uv$v-ERA_uv_anom$v
  rm(ERA_all)
  
  
  ## Save all of the dataframes in a list for SOMs and figures ##
  data_name <- paste0("data/SOM/",event2$site[1],"_",event2$event_no[1],".Rdata")
  data_packet <- list(event = event2, OISST_temp = OISST_temp, AVISO_uv = AVISO_uv, OISST_temp_anom = OISST_temp_anom, AVISO_uv_anom = AVISO_uv_anom,
                      ERA_temp = ERA_temp, ERA_uv = ERA_uv, ERA_temp_anom = ERA_temp_anom, ERA_uv_anom = ERA_uv_anom)
  save(data_packet, file = data_name)
}


# 3. Function for rounding event metrics for better plotting --------------

round.metrics <- function(df){
  df <- df %>% 
    mutate(int_max = round(int_max,1)) %>% 
    mutate(int_mean = round(int_mean,1)) %>% 
    mutate(int_cum = round(int_cum,0)) %>% 
    mutate(rate_onset = round(rate_onset,2)) %>% 
    mutate(rate_decline = round(rate_decline,2))
  return(df)
}


# 4. ggplot2 code that creates synoptic panels ----------------------------

# Load South Africa map data
load("graph/southern_africa_coast.Rdata") # Lowres
names(southern_africa_coast)[1] <- "lon"
load("graph/sa_shore.Rdata") # Hires
names(sa_shore)[4:5] <- c("lon","lat")

# Load SA bathymetry
  ## This commmented out code creates the bathy file ##
# library(PBSmapping)
# library(marmap)
# sa_bathy <- as.xyz(getNOAA.bathy(lon1 = sa_lons[1], lon2 = sa_lons[2], lat1 = sa_lats[1], lat2 = sa_lats[2], resolution =  4))
# colnames(sa_bathy) <- c("lon", "lat", "depth")
# save(sa_bathy, file = "graph/sa_bathy.Rdata")
  ##
load("graph/sa_bathy.Rdata")
sa_bathy$type = "OISST"

# The synoptic image creator
synoptic.panel <- function(temperature_dat, vector_dat, label_dat, segment_dat, bathy_dat, site_dat,
                           legend_title, uv_scalar, viridis_colour = "D", OISST = T, anom = F){
  sa1 <- ggplot() + #coord_equal() +
    geom_raster(data = temperature_dat, aes(x = x, y = y, fill = temp)) +
    # geom_segment(data = vector_dat, aes(x = x, y = y, xend = x + u * uv_scalar, yend = y + v * uv_scalar),
    #              arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.3) +
    geom_segment(data = vector_dat, aes(x = x, y = y, xend = x + u * uv_scalar, yend = y + v * uv_scalar),
                 arrow = arrow(length = unit(0.05, "cm"), angle = 45), alpha = 1, size = 0.2, linejoin = "mitre") +
    geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    geom_label(data = label_dat, aes(x = x, y = y, label = txt), size = 5, label.padding = unit(0.5, "lines")) +
    geom_segment(data = segment_dat, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_point(data = site_dat, aes(x = lon, y = lat), shape = 21,  size = 3, alpha = 0.7, colour = "red", fill = "white") +
    scale_x_continuous(limits = sa_lons, expand = c(0, 0), breaks = seq(15, 35, 5),
                       labels = scales::unit_format("°E", sep = "")) +
    scale_y_continuous(limits = sa_lats, expand = c(0, 0), breaks = seq(-35, -30, 5),
                       labels = c("35°S", "30°S")) +
    coord_fixed(xlim = c(10.5, 39.5), ylim = c(-39.5, -25.5), expand = F) +
    xlab("") + ylab("") +
    facet_wrap("type", scales = "free_y") +
    theme_grey() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          panel.background = element_rect(fill = "grey70"),
          panel.border = element_rect(fill = NA, colour = "black", size = 1),
          panel.grid.major = element_line(colour = "grey70"),
          panel.grid.minor = element_line(colour = "grey70"),
          legend.position = "right",
          legend.direction = "vertical",
          strip.text = element_text(size = 12),
          strip.background = element_rect(fill = NA),
          legend.key.height = unit(1.04, "cm"),
          axis.text = element_text(size = 12, colour = "black"),
          axis.ticks = element_line(colour = "black"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12))
  if(anom == F){
    sa1 <- sa1 + scale_fill_viridis(legend_title, option = viridis_colour)
  } else {
    sa1 <- sa1 + scale_fill_gradient2(legend_title, low = muted("blue"), high = muted("red"))
  }
  if(OISST){
    sa1 <- sa1 + stat_contour(data = bathy_dat[bathy_dat$depth < -200,], aes(x = lon, y = lat, z = depth, alpha = ..level..),
                              colour = "white", size = 0.5, binwidth = 1000, na.rm = TRUE, show.legend = FALSE) +
      geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
                   fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
      geom_point(data = site_dat, aes(x = lon, y = lat), shape = 21,  size = 3, alpha = 0.7, colour = "red", fill = "white")
  }
  # sa1
  return(sa1)
}


# 5. Function for creating synoptic figures ------------------

# Testers...
# data_file <- event_idx[SACTN_events$duration == min(SACTN_events$duration),][1,] # shortest...
# data_file <- event_idx[SACTN_events$duration == max(SACTN_events$duration),] # longest...
# data_file <- event_idx[1,] # lucky...

synoptic.fig <- function(data_file){
  
  ## Load the data packet
  data_file2 <- as.character(data_file$event)
  # load("data/SOM/Hermanus_25.Rdata")
  load(data_file2)
  
  ## Extract the data
  # Event
  event2 <- data_packet$event
  date_idx <- seq(event2$date_start, event2$date_stop, by = "day")
  # OISST
  OISST_temp <- data_packet$OISST_temp
  OISST_temp_anom <- data_packet$OISST_temp_anom
  # AVISO
  AVISO_uv <- data_packet$AVISO_uv
  AVISO_uv_anom <- data_packet$AVISO_uv_anom
  # ERA
  ERA_temp <- data_packet$ERA_temp
  ERA_temp_anom <- data_packet$ERA_temp_anom
  ERA_uv <- data_packet$ERA_uv
  ERA_uv_anom <- data_packet$ERA_uv_anom
  
  
  ## Create sea state figure ##
  # Double up temps for correct facet labelling
  OISST_temp2 <- rbind(OISST_temp, OISST_temp)
  # Add 'type' columns for faceting
  OISST_temp2$type <- rep(c("SST + Bathy", "SST + Current"), each = nrow(OISST_temp))
  # Label vectors
  AVISO_uv$type <- "SST + Current"
  sa_bathy$type <- "SST + Bathy"
  # The label dataframes
  OISST_plot_data <- data_frame(txt = "2.0 m/s\n",
                               x = 36, y = -37, type = "SST + Current")
  OISST_plot_seg <- data.frame(x = 35, y = -37.5, xend = 37, yend = -37.5, type = "SST + Current")
  # The figure
  remote_state <- synoptic.panel(temperature_dat = OISST_temp2, vector_dat = AVISO_uv, label_dat = OISST_plot_data, segment_dat =  OISST_plot_seg,
                               bathy_dat = sa_bathy, site_dat = event2, legend_title = "Temp.\n(°C)", uv_scalar = 1, viridis_colour = "D", OISST = T)
  # remote_state
  
  
  ## Create sea state anomaly figure ##
  # Double up temps for correct facet labelling
  OISST_temp_anom2 <- rbind(OISST_temp_anom, OISST_temp_anom)
  # Add 'type' columns for faceting
  OISST_temp2$type <- rep(c("SST Anomaly + Bathy", "SST Anomaly + Current Anomaly"), each = nrow(OISST_temp_anom))
  AVISO_uv_anom$type <- "SST Anomaly + Current Anomaly"
  sa_bathy$type <- "SST Anomaly + Bathy"
  # The label dataframes
  OISST_plot_anom_data <- data_frame(txt = "2.0 m/s\n",
                               x = 36, y = -37, type = "SST Anomaly + Current Anomaly")
  OISST_plot_anom_seg <- data.frame(x = 35, y = -37.5, xend = 37, yend = -37.5, type = "SST Anomaly + Current Anomaly")
  # The figure
  remote_state_anom <- synoptic.panel(temperature_dat = OISST_temp_anom2, vector_dat = AVISO_uv_anom, label_dat = OISST_plot_anom_data, segment_dat = OISST_plot_anom_seg,
                                    bathy_dat = sa_bathy, site_dat = event2, legend_title = "Anom.\n(°C)", uv_scalar = 1, viridis_colour = "D", OISST = T, anom = T)
  # remote_state_anom
  
  
  ## Create air state figure ##
  # Remove some uv rows for clearer plotting
  lon_sub <- seq(10, 40, by = 1)
  lat_sub <- seq(-40, -15, by = 1)
  ERA_uv2 <- ERA_uv[(ERA_uv$x %in% lon_sub & ERA_uv$y %in% lat_sub),]
  ERA_uv2$type <- "Air Temp + Wind"
  # The label dataframes
  ERA_plot_data <- data_frame(txt = "4.0 m/s\n",
                              x = 36, y = -37)
  ERA_plot_seg <- data.frame(x = 35, y = -37.5, xend = 37, yend = -37.5)
  # The figure
  ERA_state <- synoptic.panel(temperature_dat = ERA_temp, vector_dat = ERA_uv2, label_dat = ERA_plot_data, segment_dat = ERA_plot_seg,
                              bathy_dat = sa_bathy, site_dat = event2, legend_title = "Temp.\n(°C)", uv_scalar = 0.5, viridis_colour = "C", OISST = F)
  # ERA_state
  
  
  ## Create air state anomaly figure ##
  # Remove some wind rows for clearer plotting
  lon_sub <- seq(10, 40, by = 1)
  lat_sub <- seq(-40, -15, by = 1)
  ERA_uv_anom2 <- ERA_uv_anom[(ERA_uv_anom$x %in% lon_sub & ERA_uv_anom$y %in% lat_sub),]
  ERA_uv_anom2$type <- "Air Temp Anomaly + Wind Anomaly"
  # The label dataframes
  ERA_plot_anom_data <- data_frame(txt = "4.0 m/s\n",
                                   x = 36, y = -37)
  ERA_plot_anom_seg <- data.frame(x = 35, y = -37.5, xend = 37, yend = -37.5)
  # The figure
  ERA_state_anom <- synoptic.panel(temperature_dat = ERA_temp_anom, vector_dat = ERA_uv_anom2, label_dat = ERA_plot_anom_data, segment_dat = ERA_plot_anom_seg,
                                   bathy_dat = sa_bathy, site_dat = event2, legend_title = "Anom.\n(°C)", uv_scalar = 0.5, viridis_colour = "C", OISST = F, anom = T)
  # ERA_state_anom
  

  ## The event figure ##
  # Determine spread and subset days
  spread <- 31
  spread_clim <- filter(SACTN_clims, site == event2$site & date %in% ((date_idx[1]-spread):(date_idx[length(date_idx)]+spread)))
  event_clim <- filter(SACTN_clims, site == event2$site & date %in% ((date_idx[1]-1):(date_idx[length(date_idx)]+1)))
  # The figure
  event_flame <- ggplot(data = spread_clim, aes(x = date, y = temp, y2 = thresh_clim_year)) +
    geom_flame(aes(y = temp, y2 = thresh_clim_year, fill = "other"), show.legend = T) +
    geom_flame(data = event_clim, aes(y = temp, y2 = thresh_clim_year, fill = "main"), show.legend = T) +
    geom_line(aes(y = temp, colour = "temperature"), size = 1.2) +
    geom_line(aes(y = thresh_clim_year, colour = "threshold"), size = 1.2) +
    geom_line(aes(y = seas_clim_year, colour = "seasonal"), size = 1.2) +
    scale_colour_manual(name = "Line colour", values = c("temperature" = "black", "threshold" = "forestgreen", "seasonal" = "grey80")) +
    scale_fill_manual(name = "Event colour", values = c("other" = "salmon", "main" = "red")) +
    scale_y_continuous(labels = scales::unit_format("°C", sep = "")) +
    guides(colour = guide_legend(override.aes = list(fill = NA))) +
    xlab("") + ylab("") +
    ggtitle(paste0("Average air-sea state during ", event2$site[1], " event #", event2$event_no[1], 
                   " (", format(event2$date_start, "%d %b %Y"), " - ", format(event2$date_stop, "%d %b %Y"), ")")) +
    theme_grey() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text = element_text(size = 12, colour = "black"),
          axis.ticks = element_line(colour = "black"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          panel.border = element_rect(fill = NA, colour = "black", size = 1),
          legend.position = "top",
          legend.direction = "horizontal")
  # event_flame
  
  
  ## The info boxes ##
  # All events at the chosen site
  site_events <- filter(SACTN_events, site == event2$site)
  # Create data frames of rounded values for easier comparisons
  event_round <- round.metrics(event2)
  site_round <- round.metrics(site_events)
  all_round <- round.metrics(SACTN_events)
  # The properties of the event
  all_text <- data.frame(Properties = c(paste0("Event count --->"),
                                        paste0("Duration: ", event_round$duration, " days"),
                                        paste0("Max. intens.: ", event_round$int_max[1], "°C"),
                                        paste0("Mean intens.: ", event_round$int_mean[1], "°C"),
                                        paste0("Cum. intens.: ", event_round$int_cum[1], "°C·days"),
                                        paste0("Onset rate: ", event_round$rate_onset[1], "°C/day"),
                                        paste0("Decl. rate: ", event_round$rate_decline[1], "°C/day")),
                         Site = c(paste0(nrow(site_events)),
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
                         All = c(paste0(nrow(SACTN_events)),
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
                                               "/", length(unique(all_round$rate_decline)))))
  # Combine data frames
  # all_text <- cbind(properties[1], site_ranks[1], all_ranks[1])
  # tt1 <- ttheme_default(core = list(fg_params = list(hjust = 0, x = 0.05)),
  #                       rowhead = list(fg_params = list(hjust = 0, x = 0)))
  # The table
  text_table <- ggtexttable(all_text, rows = NULL, theme = ttheme("default", base_size = 12))
  # text_table

  ## Combine figures and save ##
  # Generate file name
  # file_name <- "~/Desktop/test.pdf"
  file_name <- paste0("graph/synoptic/",event2$site[1],"_",event2$event_no[1],".pdf")
  atlas_fig <- grid.arrange(remote_state, ERA_state,
                            remote_state_anom, ERA_state_anom,
                            event_flame, text_table,
                            layout_matrix = cbind(c(1,3,5), c(1,3,5), c(2,4,6)))
  # atlas_fig
  ggsave(filename = file_name, plot = atlas_fig, width = 17, height = 10, pointsize = 10)
}

