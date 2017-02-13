#############################################################################
###"graph/figures3.R"
## This script does:
# 1. Load SACTN event data
# 2. Prepare graphing function for sea/ air state during events
# 3. Create figure for each event
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(grid)
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
library(RmarineHeatWaves)
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

# 2. Prepare graphing function for sea/ air state during events -----------

# Create the base map figure
sa <- ggplot() + bw_update +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), fill = "grey30", colour = "black", size = 0.1, show.legend = FALSE) +
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

### Extract BRAN data during this event
## The index of months to load
date_idx <- seq(event$date_start, event$date_stop, by = "day")
temp_idx <- data.frame(files = paste0("~/data/BRAN/ocean_temp_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"))
u_idx <- data.frame(files = paste0("~/data/BRAN/ocean_u_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"))
v_idx <- data.frame(files = paste0("~/data/BRAN/ocean_v_",format(seq(event$date_start, event$date_stop, by = "month"), "%Y_%m"),".Rdata"))

## Load the data
# Temperature
system.time(BRAN_temp <- ddply(temp_idx, .(files), BRAN.Rdata, .parallel = T)) # 10 seconds
# Create mean of temperatures
BRAN_temp <- filter(BRAN_temp, date %in% date_idx)
system.time(BRAN_temp <- ddply(BRAN_temp, .(x, y), summarise, var = mean(temp, na.rm = T), .parallel = T)) # 239 seconds
BRAN_temp$stat <- "temp"
# Test visualisation
# sa + geom_raster(data = BRAN_temp, aes(x = x, y = y, fill = var)) + 
#   geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), 
#                fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) # It is necessary to reapply the coastal polygon on top

# U value
system.time(BRAN_u <- ddply(u_idx, .(files), BRAN.Rdata, .parallel = T)) # 13 seconds
# Create mean u values
BRAN_u <- filter(BRAN_u, date %in% date_idx)
system.time(BRAN_u <- ddply(BRAN_u, .(x, y), summarise, var = mean(u, na.rm = T), .parallel = T)) # 267 seconds
BRAN_u$stat <- "u"
# Test visualisation
# sa + geom_raster(data = BRAN_u, aes(x = x, y = y, fill = var)) + 
#   geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), 
#                fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE)

# V value
system.time(BRAN_v <- ddply(v_idx, .(files), BRAN.Rdata, .parallel = T)) # 13 seconds
# Create mean of v values
BRAN_v <- filter(BRAN_v, date %in% date_idx)
system.time(BRAN_v <- ddply(BRAN_v, .(x, y), summarise, var = mean(v, na.rm = T), .parallel = T)) # 267 seconds
BRAN_v$stat <- "v"
# Test visualisation
# sa + geom_raster(data = BRAN_v, aes(x = x, y = y, fill = var)) + 
#   geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), 
#                fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE)

# Combine BRAN data
BRAN_all <- rbind(BRAN_temp, BRAN_u, BRAN_v)
BRAN_all$var[BRAN_all$stat == "temp"] <- BRAN_all$var[BRAN_all$stat == "temp"]/10 # Correction just for test plotting purposes
# Test visualisation
# sa + geom_raster(data = BRAN_all, aes(x = x, y = y, fill = var)) + 
#   geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group), 
#                fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
#   facet_grid(.~stat)

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
ERA_all$var[ERA_all$stat == "temp"] <- ERA_all$var[ERA_all$stat == "temp"]/10 # Correction just for test plotting purposes
sa + geom_raster(data = ERA_all, aes(x = x, y = y, fill = var)) +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
               fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
  facet_grid(.~stat)

# Combine all data for test plotting
BRAN_all$type <- "BRAN"
ERA_all$type <- "ERA"

RE_all <- rbind(BRAN_all, ERA_all)
all_test <- sa + geom_raster(data = RE_all, aes(x = x, y = y, fill = var)) +
  geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
               fill = "grey70", colour = "black", size = 0.1, show.legend = FALSE) +
  # geom_point(aes(x = event$lon[1], y = event$lat[1]), size = 2, colour = "purple") +
  facet_grid(type~stat)

# The event figure
load("data/SACTN/SACTN_cropped.Rdata")
muizenberg <- filter(SACTN_cropped, site == "Muizenberg")
colnames(muizenberg)[4] <- "t"
muizenberg <- make_whole(muizenberg)
muizenberg <- detect(muizenberg, climatology_start = 1974, climatology_end = 2014)
muizenberg_event <- data.frame(muizenberg$clim)
muizenberg_event <- muizenberg_event[muizenberg_event$date %in% ((date_idx[1]-31):(date_idx[length(date_idx)]+31)),]

event_flame <- ggplot(data = muizenberg_event, aes(x = date, y = temp, thresh = thresh_clim_year, seas = seas_clim_year, event = event_no)) +
  geom_flame()


# Combine figures and save
pdf("~/Desktop/first_concept.pdf", width = 10, height = 10, pointsize = 10) # Set PDF dimensions
vp1 <- viewport(x = 0.5, y = 0.05, w = 1.00, h = 0.35, just = "bottom") # Flame
vp2 <- viewport(x = 0.5, y = 1.3, w = 1.00, h = 1.30, just = "top")  # Air/ Sea
print(all_test, vp = vp2)
print(event_flame, vp = vp1)
dev.off()

