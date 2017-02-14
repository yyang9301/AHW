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
source("~/RmarineHeatWaves/R/ggplotGeoms.R") # Load manually until they are in the package
## USED BY:
# 
## CREATES:
# Figures similar to Eric's figures in the MHW atlas
#############################################################################


# 1. Load SACTN event data ------------------------------------------------

load("data/events/SACTN_events.Rdata")
load("setupParams/SACTN_site_list.Rdata")

# Load SA map data
load("graph/southern_africa_coast.RData") # Lowres
names(southern_africa_coast)[1] <- "lon"
load("graph/sa_shore.Rdata") # Hires
names(sa_shore)[4:5] <- c("lon","lat")
load("graph/")

# Load SA bathymetry
load("~/SA_map/bathy.RData") # HiRes for 200m isobath
load("~/SA_map/sa_bathy.RData") # LowRes for deeper

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
event <- SACTN_events[SACTN_events$duration == max(SACTN_events$duration),]
event$lat <- SACTN_site_list$lat[SACTN_site_list$site == event$site]
event$lon <- SACTN_site_list$lon[SACTN_site_list$site == event$site]

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
system.time(BRAN_temp <- ddply(temp_idx, .(files), BRAN.Rdata, .progress = "text")) # 10 seconds
# This is not faster in parallel...
  # Ideally a data.table command could replace this use of ddply
# Create mean of temperatures
BRAN_temp <- filter(BRAN_temp, date %in% date_idx)
BRAN_temp <- data.table(BRAN_temp)
system.time(BRAN_temp <- BRAN_temp[, .(temp = mean(temp, na.rm = TRUE)), by = .(x,y)]) # 1 seconds
# BRAN_temp$stat <- "temp"
# Test visualisation
# sa + geom_raster(data = BRAN_temp, aes(x = x, y = y, fill = temp)) +
#   geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
#                fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) # It is necessary to reapply the coastal polygon on top

# U value
system.time(BRAN_u <- ddply(u_idx, .(files), BRAN.Rdata, .parallel = T)) # 13 seconds
# Create mean u values
BRAN_u <- filter(BRAN_u, date %in% date_idx)
BRAN_u <- data.table(BRAN_u)
system.time(BRAN_u <- BRAN_u[, .(u = mean(u, na.rm = TRUE)), by = .(x,y)]) # 1 seconds
# BRAN_u$stat <- "u"
# Test visualisation
# sa + geom_raster(data = BRAN_u, aes(x = x, y = y, fill = var)) + 
#   geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), 
#                fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE)

# V value
system.time(BRAN_v <- ddply(v_idx, .(files), BRAN.Rdata, .parallel = T)) # 13 seconds
# Create mean of v values
BRAN_v <- filter(BRAN_v, date %in% date_idx)
BRAN_v <- data.table(BRAN_v)
system.time(BRAN_v <- BRAN_v[, .(v = mean(v, na.rm = TRUE)), by = .(x,y)]) # 1 seconds
# BRAN_v$stat <- "v"
# Test visualisation
# sa + geom_raster(data = BRAN_v, aes(x = x, y = y, fill = var)) + 
#   geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), 
#                fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE)

# Create wind data frame
BRAN_wind <- merge(BRAN_u, BRAN_v, by = c("x", "y"))
# Visualise wind vectors
# scaler <- 1  # Use this to change unit. E.g., from meters per minute to meters per second.
# wind_fig <- ggplot(BRAN_wind, aes(x = x, y = y, xend = x + u * scaler, yend = y + v * scaler)) + 
#   geom_segment(arrow = arrow(angle = 15, length = unit(0.05, "inches"), type = "closed"))
# 
# first_wind <- sa + geom_raster(data = BRAN_temp, aes(x = x, y = y, fill = temp)) +
#   geom_segment(data = BRAN_wind, aes(x = x, y = y, xend = x + u * scaler, yend = y + v * scaler),
#                   arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
#   geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), 
#                                fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
#   scale_fill_viridis(expression(paste("Temp. (",degree,"C)")))
# first_wind
# ggsave("~/Desktop/first_wind.pdf", width = 10)


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
colnames(ERA_wind) <- c("x","y","u","v")

# scaler <- 1  # Use this to change unit. E.g., from meters per minute to meters per second.
# wind_fig <- ggplot(ERA_wind, aes(x = x, y = y, xend = x + u * scaler, yend = y + v * scaler)) +
#   geom_segment(arrow = arrow(angle = 15, length = unit(0.05, "inches"), type = "closed"))
# 
# first_wind <- sa + geom_raster(data = ERA_temp, aes(x = x, y = y, fill = temp)) +
#   geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
#                fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
#   geom_segment(data = ERA_wind, aes(x = x, y = y, xend = x + u * scaler, yend = y + v * scaler),
#                   arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
#   scale_fill_viridis(expression(paste("Temp. (",degree,"C)")))
# first_wind
# ggsave("~/Desktop/first_wind.pdf", width = 10)

# Combine all data for test plotting
BRAN_temp$type <- "BRAN"
BRAN_wind$type <- "BRAN"
ERA_temp$type <- "ERA"
ERA_temp$stat <- NULL
ERA_wind$type <- "ERA"


RE_temp <- rbind(BRAN_temp, ERA_temp)
RE_wind <- rbind(BRAN_wind, ERA_wind)


all_test <- sa + geom_raster(data = RE_temp, aes(x = x, y = y, fill = temp)) +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
               fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
    geom_segment(data = RE_wind, aes(x = x, y = y, xend = x + u * scaler, yend = y + v * scaler),
                    arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
    scale_fill_viridis(expression(paste("Temp. (",degree,"C)"))) +
  facet_grid(.~type)
all_test

# The event figure
load("data/SACTN/SACTN_cropped.Rdata")
muizenberg <- filter(SACTN_cropped, site == "Muizenberg")
colnames(muizenberg)[4] <- "t"
muizenberg <- make_whole(muizenberg)
muizenberg <- detect(muizenberg, climatology_start = 1974, climatology_end = 2014)
muizenberg_event <- data.frame(muizenberg$clim)
muizenberg_event <- muizenberg_event[muizenberg_event$date %in% ((date_idx[1]-31):(date_idx[length(date_idx)]+31)),]

event_flame <- ggplot(data = muizenberg_event, aes(x = date, y = temp, thresh = thresh_clim_year, seas = seas_clim_year, event = event_no)) +
  geom_flame() +
  scale_y_continuous(limits = c((min(muizenberg_event$seas_clim_year)-1), (max(muizenberg_event$temp, na.rm = T)+1))) +
  scale_x_date(expand = c(0,0))
event_flame

# Combine figures and save
pdf("~/Desktop/second_concept.pdf", width = 10, height = 8, pointsize = 10) # Set PDF dimensions
vp1 <- viewport(x = 0.5, y = 0.05, w = 1.00, h = 0.35, just = "bottom") # Flame
vp2 <- viewport(x = 0.5, y = 1.3, w = 1.00, h = 1.30, just = "top")  # Air/ Sea
print(all_test, vp = vp2)
print(event_flame, vp = vp1)
dev.off()

