#############################################################################
###"func/synoptic.func.R"
## This script does:
# 1. Function that loads only necessary BRAN files
# 2. A function that creates synoptic data packets for MHWs
# 3. Function for rounding event metrics for better plotting
# 4. ggplot2 code that creates synoptic panels
# 5. Function for creating synoptic figures

## DEPENDS ON:
library(grid)
library(gridExtra)
library(ggplot2)
library(scales)
library(viridis)
library(RmarineHeatWaves)
# library(stringr)
# library(plyr)
# library(dplyr)
# library(data.table)
# library(reshape2)
# library(lubridate)
# library(zoo)
# library(doMC); registerDoMC(cores = 4)
source("func/load.reanalyses.R")
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

# 1. Function that loads only necessary BRAN files ------------------------

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
  BRAN_var <- plyr::ddply(var_idx, c("files"), BRAN.Rdata, .parallel = T)
  BRAN_var <- filter(BRAN_var, date %in% date_idx)
  BRAN_var$files <- NULL
  BRAN_var <- BRAN_var[complete.cases(BRAN_var$var),]
  BRAN_var <- data.table(BRAN_var)
  BRAN_var <- BRAN_var[, .(var = mean(var, na.rm = TRUE)), by = .(x,y)]
  colnames(BRAN_var)[3] <- var
  BRAN_var <- BRAN_var[order(BRAN_var$x),]
  return(BRAN_var)
}


# 2. A function that creates synoptic data packets for MHWs ---------------

data.packet <- function(event){
  
  ## Begin by adding lon/ lat and determining date index ##
  event2 <- event
  event2$lat <- SACTN_site_list$lat[SACTN_site_list$site == event$site]
  event2$lon <- SACTN_site_list$lon[SACTN_site_list$site == event$site]
  event2$type <- NULL
  date_idx <- seq(event$date_start, event$date_stop, by = "day")
  date_idx_2 <- format(date_idx, "%m-%d")
  
  
  ## Extract BRAN data during the chosen event ##
  BRAN_temp <- BRAN.event(event, "temp")
  BRAN_u <- BRAN.event(event, "u")
  BRAN_v <- BRAN.event(event, "v")
  # Create uv data frame
  BRAN_uv <- merge(BRAN_u, BRAN_v, by = c("x", "y")); rm(BRAN_u, BRAN_v)
  # Temperature anomaly
  BRAN_temp_anom <- filter(BRAN_temp_clim, date %in% date_idx_2)
  BRAN_temp_anom <- data.table(BRAN_temp_anom)
  BRAN_temp_anom <- BRAN_temp_anom[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]
  BRAN_temp_anom$temp <- BRAN_temp$temp-BRAN_temp_anom$temp
  # uv anomaly
  BRAN_uv_anom <- filter(BRAN_uv_clim, date %in% date_idx_2)
  BRAN_uv_anom <- data.table(BRAN_uv_anom)
  BRAN_uv_anom <- BRAN_uv_anom[, .(u = mean(u, na.rm = TRUE),
                                   v = mean(v, na.rm = TRUE)), by = .(x,y)]
  BRAN_uv_anom$u <- BRAN_uv$u-BRAN_uv_anom$u
  BRAN_uv_anom$v <- BRAN_uv$v-BRAN_uv_anom$v
  
  
  ## Extract ERA Interim data during this event ##
  # Run the function as necesary on the following files
  # NB: There is a slight possibility that more than one file will be used
    # ERA1 not loaded as these dates precede any available BRAN data
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
  ERA_all <- data.frame()
    # ERA1 not loaded as these dates precede any available BRAN data
  if(exists("ERA2")) ERA_all <- rbind(ERA_all, ERA2)
  if(exists("ERA3")) ERA_all <- rbind(ERA_all, ERA3)
  if(exists("ERA4")) ERA_all <- rbind(ERA_all, ERA4)
  # Create mean values
  ERA_all <- data.table(ERA_all)
  ERA_all <- ERA_all[, .(temp = mean(temp, na.rm = TRUE),
                         u = mean(u, na.rm = TRUE),
                         v = mean(v, na.rm = TRUE)), by = .(x,y)]
  # Temperature
  ERA_temp <- ERA_all[,c(1:3)]
  # UV
  ERA_uv <- ERA_all[,c(1:2,4:5)]
  # Temperature anomaly
  ERA_temp_anom <- filter(ERA_temp_clim, date %in% date_idx_2)
  ERA_temp_anom <- data.table(ERA_temp_anom)
  ERA_temp_anom <- ERA_temp_anom[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]
  ERA_temp_anom$temp <- ERA_temp$temp-ERA_temp_anom$temp
  # uv anomaly
  ERA_uv_anom <- filter(ERA_uv_clim, date %in% date_idx_2)
  ERA_uv_anom <- data.table(ERA_uv_anom)
  ERA_uv_anom <- ERA_uv_anom[, .(u = mean(u, na.rm = TRUE),
                                 v = mean(v, na.rm = TRUE)), by = .(x,y)]
  ERA_uv_anom$u <- ERA_uv$u-ERA_uv_anom$u
  ERA_uv_anom$v <- ERA_uv$v-ERA_uv_anom$v
  rm(ERA_all)
  
  
  ## Save all of the dataframes in a list for SOMs and figures ##
  data_name <- paste0("data/SOM/",event2$site[1],"_",event2$event_no[1],".Rdata")
  data_packet <- list(event = event2, BRAN_temp = BRAN_temp, BRAN_uv = BRAN_uv, BRAN_temp_anom = BRAN_temp_anom, BRAN_uv_anom = BRAN_uv_anom,
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
load("graph/southern_africa_coast.RData") # Lowres
names(southern_africa_coast)[1] <- "lon"
load("graph/sa_shore.Rdata") # Hires
names(sa_shore)[4:5] <- c("lon","lat")

# Load SA bathymetry
  ## This commmented out code creates the bathy file ##
# library(PBSmapping)
# library(marmap)
# sa_bathy <- as.xyz(getNOAA.bathy(lon1 = sa_lons[1], lon2 = sa_lons[2], lat1 = sa_lats[1], lat2 = sa_lats[2], resolution =  4))
# colnames(sa_bathy) <- c("lon", "lat", "depth")
# save(sa_bathy, file = "data/sa_bathy.Rdata")
  ##
load("data/sa_bathy.Rdata")
sa_bathy$type = "BRAN"

# The synoptic image creator
synoptic.panel <- function(temperature_dat, vector_dat, label_dat, segment_dat, bathy_dat, site_dat,
                           legend_title, uv_scalar, viridis_colour = "D", BRAN = T){
  sa1 <- ggplot() + #coord_equal() +
    geom_raster(data = temperature_dat, aes(x = x, y = y, fill = temp)) +
    geom_segment(data = vector_dat, aes(x = x, y = y, xend = x + u * uv_scalar, yend = y + v * uv_scalar),
                 arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
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
    scale_fill_viridis(legend_title, option = viridis_colour) +
    facet_wrap("type", scales = "free_y") +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          panel.background = element_rect(fill = "grey70"),
          panel.border = element_rect(fill = NA, colour = "black", size = 1),
          panel.grid.major = element_line(colour = "grey70"),
          panel.grid.minor = element_line(colour = "grey70"),
          legend.position = "right",
          legend.direction = "vertical",
          strip.text = element_text(size = 12),
          strip.background = element_rect(fill = NA),
          legend.key.height = unit(1.1, "cm"),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12))
  if(BRAN){
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
  load(data_file2)
  
  ## Extract the data
  # Event
  event2 <- data_packet$event
  date_idx <- seq(event2$date_start, event2$date_stop, by = "day")
  # BRAN
  BRAN_temp <- data_packet$BRAN_temp
  BRAN_temp_anom <- data_packet$BRAN_temp_anom
  BRAN_uv <- data_packet$BRAN_uv
  BRAN_uv_anom <- data_packet$BRAN_uv_anom
  # ERA
  ERA_temp <- data_packet$ERA_temp
  ERA_temp_anom <- data_packet$ERA_temp_anom
  ERA_uv <- data_packet$ERA_uv
  ERA_uv_anom <- data_packet$ERA_uv_anom
  
  
  ## Create sea state figure ##
  # Double up temps for correct facet labelling
  BRAN_temp2 <- rbind(BRAN_temp, BRAN_temp)
  # Add 'type' columns for faceting
  BRAN_temp2$type <- rep(c("SST + Bathy", "SST + Current"), each = nrow(BRAN_temp))
  BRAN_uv$type <- "SST + Current"
  sa_bathy$type <- "SST + Bathy"
  # The label dataframes
  BRAN_plot_data <- data_frame(txt = "1.0 m/s\n",
                               x = 36, y = -37, type = "SST + Current")
  BRAN_plot_seg <- data.frame(x = 35.5, y = -37.5, xend = 36.5, yend = -37.5, type = "SST + Current")
  # The figure
  BRAN_state <- synoptic.panel(temperature_dat = BRAN_temp2, vector_dat = BRAN_uv, label_dat = BRAN_plot_data, segment_dat =  BRAN_plot_seg,
                               bathy_dat = sa_bathy, site_dat = event2, legend_title = "Temp.\n(°C)", uv_scalar = 1, viridis_colour = "D", BRAN = T)
  # BRAN_state
  
  
  ## Create sea state anomaly figure ##
  # Double up temps for correct facet labelling
  BRAN_temp_anom2 <- rbind(BRAN_temp_anom, BRAN_temp_anom)
  # Add 'type' columns for faceting
  BRAN_temp2$type <- rep(c("SST Anomaly + Bathy", "SST Anomaly + Current Anomaly"), each = nrow(BRAN_temp_anom))
  BRAN_uv_anom$type <- "SST Anomaly + Current Anomaly"
  sa_bathy$type <- "SST Anomaly + Bathy"
  # The label dataframes
  BRAN_plot_anom_data <- data_frame(txt = "1.0 m/s\n",
                               x = 36, y = -37, type = "SST Anomaly + Current Anomaly")
  BRAN_plot_anom_seg <- data.frame(x = 35.5, y = -37.5, xend = 36.5, yend = -37.5, type = "SST Anomaly + Current Anomaly")
  # The figure
  BRAN_state_anom <- synoptic.panel(temperature_dat = BRAN_temp_anom2, vector_dat = BRAN_uv_anom, label_dat = BRAN_plot_anom_data, segment_dat =  BRAN_plot_anom_seg,
                                    bathy_dat = sa_bathy, site_dat = event2, legend_title = "Anom.\n(°C)", uv_scalar = 1, viridis_colour = "D", BRAN = T)
  # BRAN_state_anom
  
  
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
                              bathy_dat = sa_bathy, site_dat = event2, legend_title = "Temp.\n(°C)", uv_scalar = 0.5, viridis_colour = "C", BRAN = F)
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
                                   bathy_dat = sa_bathy, site_dat = event2, legend_title = "Anom.\n(°C)", uv_scalar = 0.5, viridis_colour = "C", BRAN = F)
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
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text = element_text(size = 12),
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
  text_anchor <- data.frame(x = 1, y = 1)
  tt1 <- ttheme_default(core = list(fg_params = list(hjust = 0, x = 0.05)),
                        rowhead = list(fg_params = list(hjust = 0, x = 0)))
  # The table
  text_table <- ggplot(text_anchor, aes(x = x, y = y)) + geom_blank() + theme_void() +
    geom_point() +
    annotation_custom(tableGrob(all_text, rows = NULL, theme = tt1)) +
    scale_x_continuous(expand = F) + scale_y_continuous(expand = F)
  # text_table
  
  
  ## Combine figures and save ##
  # Generate file name
  # file_name <- "~/Desktop/test.pdf"
  file_name <- paste0("graph/synoptic/",event2$site[1],"_",event2$event_no[1],".pdf")
  # The figure
  pdf(file_name, width = 17, height = 10, pointsize = 10) # Set PDF dimensions
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3,3)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(BRAN_state, vp = vplayout(1,1:2))
  print(ERA_state, vp = vplayout(1,3))
  print(BRAN_state_anom, vp = vplayout(2,1:2))
  print(ERA_state_anom, vp = vplayout(2,3))
  print(event_flame, vp = vplayout(3,1:2))
  print(text_table, vp = vplayout(3,3))
  dev.off()
}

