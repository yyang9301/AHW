#############################################################################
###"prep/air.sea.state.R"
## This script does:
# 1. Load, create, and save mean sea state files
# 2. Combine and save sea state files
# 3. Load, create, and save mean air state files
# 4. Combine and save air state files
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(raster)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(data.table)
library(zoo)
library(tidyr)
library(purrr)
library(broom)
library(RmarineHeatWaves)
source("func/load.reanalyses.R")
## USED BY:
# "graph/figures3.R"
## CREATES:
# 
#############################################################################


# 1. Load, create, and save mean sea state files --------------------------

# Indices for loading
temp_idx <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_temp", full.names = TRUE), 
                       x = 1:length(dir("~/data/BRAN", pattern = "ocean_temp")))
u_idx <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_u", full.names = TRUE), 
                       x = 1:length(dir("~/data/BRAN", pattern = "ocean_u")))
v_idx <- data.frame(files = dir("~/data/BRAN", pattern = "ocean_v", full.names = TRUE), 
                       x = 1:length(dir("~/data/BRAN", pattern = "ocean_v")))

# Function for loading and creating monthly means
# idx <- temp_idx[1,]
BRAN.monthly <- function(idx){
  # print(paste0("Now loading ",basename(as.character(df$files)))) # Doesn't appear to work with dplyr
  system.time(BRAN_var <- ddply(idx, .(files), BRAN.Rdata, .progress = "text")) # ~16 seconds for one file
  BRAN_var$files <- NULL
  colnames(BRAN_var) <- c("x","y","var","date")
  BRAN_var <- data.table(BRAN_var)
  system.time(BRAN_var <- BRAN_var[, .(var = mean(var, na.rm = TRUE)), by = .(x,y)]) # 1 seconds
  return(BRAN_var)
}

# Load all temp, create total mean, and save monthly and total
system.time(BRAN_temp <- ddply(temp_idx, .(files), BRAN.monthly, .parallel = T)) # 3606 seconds
BRAN_temp_monthly <- BRAN_temp
save(BRAN_temp_monthly, file = "data/BRAN/BRAN_temp_monthly.Rdata")
BRAN_temp2 <- data.table(BRAN_temp)
system.time(BRAN_temp2 <- BRAN_temp2[, .(var = mean(var, na.rm = TRUE)), by = .(x,y)]) # 6 seconds
BRAN_temp_all <- BRAN_temp2
save(BRAN_temp_all, file = "data/BRAN/BRAN_temp_all.Rdata")

### Create daily climatologies by month to save processing power
## The following code is intentionally repetitive in order to allow it to be run piecemeal
## Flushing the RAM between steps as required
# var1 <- "temp"; mon <- "01" # testing...
BRAN.daily <- function(var1, mon){
  idx_mon <- data.frame(files = dir("~/data/BRAN", pattern = paste0("ocean_",var1,"_*.*_", mon), full.names = TRUE), 
                        x = 1:length(dir("~/data/BRAN", pattern = paste0("ocean_",var1,"_*.*_", mon))))
  system.time(BRAN_var <- ddply(idx_mon, .(files), BRAN.Rdata, .progress = "text")) # ~16 seconds for one file
  BRAN_var$files <- NULL
  colnames(BRAN_var) <- c("x","y","var","date")
  system.time(BRAN_var$date <- format(BRAN_var$date, "%m-%d")) # 34 seconds
  BRAN_var <- data.table(BRAN_var)
  system.time(BRAN_var <- BRAN_var[, .(var = mean(var, na.rm = TRUE)), by = .(x,y,date)]) # 1 seconds
  return(BRAN_var)
}

## Temperature
# January
system.time(BRAN_temp_jan <- BRAN.daily("temp", "01")) # 524 seconds
save(BRAN_temp_jan, file = "data/BRAN/BRAN_temp_jan.Rdata")
# February
system.time(BRAN_temp_feb <- BRAN.daily("temp", "02")) # 407 seconds
save(BRAN_temp_feb, file = "data/BRAN/BRAN_temp_feb.Rdata")
# March
system.time(BRAN_temp_mar <- BRAN.daily("temp", "03")) # 416 seconds
save(BRAN_temp_mar, file = "data/BRAN/BRAN_temp_mar.Rdata")
# April
system.time(BRAN_temp_apr <- BRAN.daily("temp", "04")) # 375 seconds
save(BRAN_temp_apr, file = "data/BRAN/BRAN_temp_apr.Rdata")
# May
system.time(BRAN_temp_may <- BRAN.daily("temp", "05")) # 436 seconds
save(BRAN_temp_may, file = "data/BRAN/BRAN_temp_may.Rdata")
# June
system.time(BRAN_temp_jun <- BRAN.daily("temp", "06")) # 396 seconds
save(BRAN_temp_jun, file = "data/BRAN/BRAN_temp_jun.Rdata")
# July
system.time(BRAN_temp_jul <- BRAN.daily("temp", "07")) # 396 seconds
save(BRAN_temp_jul, file = "data/BRAN/BRAN_temp_jul.Rdata")
# August
system.time(BRAN_temp_aug <- BRAN.daily("temp", "08")) # 390 seconds
save(BRAN_temp_aug, file = "data/BRAN/BRAN_temp_aug.Rdata")
# September
system.time(BRAN_temp_sep <- BRAN.daily("temp", "09")) # 495 seconds
save(BRAN_temp_sep, file = "data/BRAN/BRAN_temp_sep.Rdata")
# October
system.time(BRAN_temp_oct <- BRAN.daily("temp", "10")) # 405 seconds
save(BRAN_temp_oct, file = "data/BRAN/BRAN_temp_oct.Rdata")
# November
system.time(BRAN_temp_nov <- BRAN.daily("temp", "11")) # 390 seconds
save(BRAN_temp_nov, file = "data/BRAN/BRAN_temp_nov.Rdata")
# December
system.time(BRAN_temp_dec <- BRAN.daily("temp", "12")) # 407 seconds
save(BRAN_temp_dec, file = "data/BRAN/BRAN_temp_dec.Rdata")

## U
# January
system.time(BRAN_u_jan <- BRAN.daily("u", "01")) # 445 seconds
save(BRAN_u_jan, file = "data/BRAN/BRAN_u_jan.Rdata")
# February
system.time(BRAN_u_feb <- BRAN.daily("u", "02")) # 372 seconds
save(BRAN_u_feb, file = "data/BRAN/BRAN_u_feb.Rdata")
# March
system.time(BRAN_u_mar <- BRAN.daily("u", "03")) # 424 seconds
save(BRAN_u_mar, file = "data/BRAN/BRAN_u_mar.Rdata")
# April
system.time(BRAN_u_apr <- BRAN.daily("u", "04")) # 407 seconds
save(BRAN_u_apr, file = "data/BRAN/BRAN_u_apr.Rdata")
# May
system.time(BRAN_u_may <- BRAN.daily("u", "05")) # 404 seconds
save(BRAN_u_may, file = "data/BRAN/BRAN_u_may.Rdata")
# June
system.time(BRAN_u_jun <- BRAN.daily("u", "06")) # 332 seconds
save(BRAN_u_jun, file = "data/BRAN/BRAN_u_jun.Rdata")
# July
system.time(BRAN_u_jul <- BRAN.daily("u", "07")) # 369 seconds
save(BRAN_u_jul, file = "data/BRAN/BRAN_u_jul.Rdata")
# August
system.time(BRAN_u_aug <- BRAN.daily("u", "08")) # 366 seconds
save(BRAN_u_aug, file = "data/BRAN/BRAN_u_aug.Rdata")
# September
system.time(BRAN_u_sep <- BRAN.daily("u", "09")) # 331 seconds
save(BRAN_u_sep, file = "data/BRAN/BRAN_u_sep.Rdata")
# October
system.time(BRAN_u_oct <- BRAN.daily("u", "10")) # 419 seconds
save(BRAN_u_oct, file = "data/BRAN/BRAN_u_oct.Rdata")
# November
system.time(BRAN_u_nov <- BRAN.daily("u", "11")) # 401 seconds
save(BRAN_u_nov, file = "data/BRAN/BRAN_u_nov.Rdata")
# December
system.time(BRAN_u_dec <- BRAN.daily("u", "12")) # 398 seconds
save(BRAN_u_dec, file = "data/BRAN/BRAN_u_dec.Rdata")

## V
# January
system.time(BRAN_v_jan <- BRAN.daily("v", "01")) # 435 seconds
save(BRAN_v_jan, file = "data/BRAN/BRAN_v_jan.Rdata")
# Febrvary
system.time(BRAN_v_feb <- BRAN.daily("v", "02")) # 408 seconds
save(BRAN_v_feb, file = "data/BRAN/BRAN_v_feb.Rdata")
# March
system.time(BRAN_v_mar <- BRAN.daily("v", "03")) # 394 seconds
save(BRAN_v_mar, file = "data/BRAN/BRAN_v_mar.Rdata")
# April
system.time(BRAN_v_apr <- BRAN.daily("v", "04")) # 339 seconds
save(BRAN_v_apr, file = "data/BRAN/BRAN_v_apr.Rdata")
# May
system.time(BRAN_v_may <- BRAN.daily("v", "05")) # 365 seconds
save(BRAN_v_may, file = "data/BRAN/BRAN_v_may.Rdata")
# Jvne
system.time(BRAN_v_jun <- BRAN.daily("v", "06")) # 445 seconds
save(BRAN_v_jun, file = "data/BRAN/BRAN_v_jun.Rdata")
# Jvly
system.time(BRAN_v_jul <- BRAN.daily("v", "07")) # 452 seconds
save(BRAN_v_jul, file = "data/BRAN/BRAN_v_jul.Rdata")
# Avgvst
system.time(BRAN_v_aug <- BRAN.daily("v", "08")) # 397 seconds
save(BRAN_v_aug, file = "data/BRAN/BRAN_v_aug.Rdata")
# September
system.time(BRAN_v_sep <- BRAN.daily("v", "09")) # 367 seconds
save(BRAN_v_sep, file = "data/BRAN/BRAN_v_sep.Rdata")
# October
system.time(BRAN_v_oct <- BRAN.daily("v", "10")) # 398 seconds
save(BRAN_v_oct, file = "data/BRAN/BRAN_v_oct.Rdata")
# November
system.time(BRAN_v_nov <- BRAN.daily("v", "11")) # 393 seconds
save(BRAN_v_nov, file = "data/BRAN/BRAN_v_nov.Rdata")
# December
system.time(BRAN_v_dec <- BRAN.daily("v", "12")) # 5400seconds
save(BRAN_v_dec, file = "data/BRAN/BRAN_v_dec.Rdata")

### Create daily running mean climatologies by year to save processing power
## The following code is intentionally repetitive in order to allow it to be run piecemeal
## Flushing the RAM between steps as required
source("~/RmarineHeatWaves/R/RmarineHeatWaves.R") # Manually load until changes reloaded to CRAN
# var1 <- "temp"; yr <- "1994" # testing...
BRAN.daily.run <- function(var1, yr){
  idx_yr <- data.frame(files = dir("~/data/BRAN", pattern = paste0("ocean_",var1,"_*.*_", yr), full.names = TRUE), 
                        x = 1:length(dir("~/data/BRAN", pattern = paste0("ocean_",var1,"_*.*_", yr))))
  system.time(BRAN_var <- ddply(idx_yr, .(files), BRAN.Rdata, .parallel = T)) # ~5 seconds for one file
  BRAN_var$files <- NULL
  colnames(BRAN_var) <- c("x","y","temp","t")
  BRAN_var <- make_whole(BRAN_var)
  BRAN_var <- detect(BRAN_var)
  system.time(BRAN_var$date <- format(BRAN_var$date, "%m-%d")) # 34 seconds
  BRAN_var <- data.table(BRAN_var)
  system.time(BRAN_var <- BRAN_var[, .(var = mean(var, na.rm = TRUE)), by = .(x,y,date)]) # 1 seconds
  return(BRAN_var)
}

BRAN.smooth <- function(df) {
  # BRAN_var <- BRAN.Rdata()
  start <- min(df$date, na.rm = T) 
  end <- max(df$date, na.rm = T)
  colnames(df) <- c("x","y","temp","t")
  whole <- make_whole(df)
  res <- detect(whole, climatology_start = start, climatology_end = end, clim_only = T)
  # events <- mhw$event
  return(res)
}

# Subsetting extent
sub_lon <- seq(10,11,0.1)
sub_lat <- seq(-26,-25,0.1)
# Load years of data
BRAN_temp1 <- ddply(temp_idx[1:12,], .(files), BRAN.Rdata, .parallel = T)
BRAN_temp1$files <- NULL
BRAN_temp1_sub <- BRAN_temp1[round(BRAN_temp1$x,1) %in% sub_lon & round(BRAN_temp1$y) %in% sub_lat,]
BRAN_temp2 <- ddply(temp_idx[13:24,], .(files), BRAN.Rdata, .parallel = T)
BRAN_temp2$files <- NULL
BRAN_temp2_sub <- BRAN_temp2[round(BRAN_temp2$x,1) %in% sub_lon & round(BRAN_temp2$y) %in% sub_lat,]
# BRAN_temp3 <- ddply(temp_idx[25:36,], .(files), BRAN.Rdata, .parallel = T)
BRAN_temp12 <- rbind(BRAN_temp1, BRAN_temp2)
BRAN_temp12$files <- NULL
BRAN_temp12_sub <- BRAN_temp12[round(BRAN_temp12$x,1) %in% sub_lon & round(BRAN_temp12$y) %in% sub_lat,]

# Smooth
system.time(BRAN_temp1_smooth <- ddply(BRAN_temp1_sub, .(x,y), BRAN.smooth, .parallel = T))
system.time(BRAN_temp2_smooth <- ddply(BRAN_temp2_sub, .(x,y), BRAN.smooth, .parallel = T))
system.time(BRAN_temp12_smooth <- ddply(BRAN_temp12_sub, .(x,y), BRAN.smooth, .parallel = T))
BRAN_temp12_smooth <- BRAN_temp12_smooth[1:(109800/2),]

# Compare different smoothing
BRAN_temp1_2 <- (BRAN_temp1_smooth$seas_clim_year + BRAN_temp2_smooth$seas_clim_year)/2
BRAN_res <- merge(BRAN_temp1_smooth[,c(1,2,3,6)], BRAN_temp2_smooth[,c(1,2,3,6)], by = c("x", "y", "doy"))
BRAN_res$res1 <- (BRAN_res$seas_clim_year.x + BRAN_res$seas_clim_year.y)/2

BRAN_res2 <- merge(BRAN_res, BRAN_temp12_smooth[,c(1,2,3,6)], by = c("x","y","doy"))
BRAN_res2$coord_idx <- paste(round(BRAN_res2$x,2), round(BRAN_res2$y,2), sep = "/ ")

res_all <- mean(BRAN_res2$res1 - BRAN_res2$seas_clim_year, na.rm = T)
# Mean difference = 9.898108e-18

# Create ggplot showing results
BRAN_res2_long <- melt(BRAN_res2, id.vars = c("coord_idx", "doy"), measure.vars = c("res1", "seas_clim_year"))
ggplot(data = BRAN_res2_long, aes(x = doy, y = value, colour = value)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~variable) +
  ylab("Smooth climatology")

ggplot(data = BRAN_res2, aes(x = doy, y = res2, group = coord_idx)) +
  geom_line() +
  scale_y_continuous(limits = c(-1,1)) +
  ylab("Difference")

BRAN_res3 <- ddply(BRAN_res2, .(x,y), summarise,
                   "1994" = mean(seas_clim_year.x, na.rm = T),
                   "1995" = mean(seas_clim_year.y, na.rm = T),
                   both = mean(seas_clim_year, na.rm = T))
BRAN_res3_long <- melt(BRAN_res3, id.vars = c("x","y"), variable.name = "seas", value.name = "clim")


BRAN_res3_long$coord_idx <- paste(round(BRAN_res3_long$x,2), round(BRAN_res3_long$y,2), sep = "/ ")

library(ggplot2)
ggplot(data = BRAN_res3_long, aes(x = x, y = y, fill = clim)) +
  geom_raster() +
  facet_wrap(~seas)

ggplot(data = BRAN_res3_long, aes(x = x, y = y, fill = clim)) +
  geom_raster()

BRAN_res2$res2 <- BRAN_res2$res1-BRAN_res2$seas_clim_year

ggplot(data = BRAN_res2, aes(x = x, y = y, fill = res2)) +
  geom_raster()

# Now test this on SACTN time series
load("~/SACTNraw/data/SACTNdaily_v4.1.Rdata")

# Subset Muizenberg
muiz <- filter(SACTNdaily_v4.1, site == "Muizenberg")
muiz$year <- year(muiz$date)

# Create smaller chunks for comparison
muiz1 <- muiz[muiz$year %in% seq(1975, 1984,1),4:5]
colnames(muiz1) <- c("t","temp")
muiz1 <- make_whole(muiz1)
start = year(min(muiz1$date))
end = year(max(muiz1$date))
muiz1 <- detect(muiz1, climatology_start = start, climatology_end = end, clim_only = T)

# 2. Combine and save sea state files -------------------------------------

# Temperature
load("data/BRAN/BRAN_temp_jan.Rdata")
load("data/BRAN/BRAN_temp_feb.Rdata")
load("data/BRAN/BRAN_temp_mar.Rdata")
load("data/BRAN/BRAN_temp_apr.Rdata")
load("data/BRAN/BRAN_temp_may.Rdata")
load("data/BRAN/BRAN_temp_jun.Rdata")
load("data/BRAN/BRAN_temp_jul.Rdata")
load("data/BRAN/BRAN_temp_aug.Rdata")
load("data/BRAN/BRAN_temp_sep.Rdata")
load("data/BRAN/BRAN_temp_oct.Rdata")
load("data/BRAN/BRAN_temp_nov.Rdata")
load("data/BRAN/BRAN_temp_dec.Rdata")
BRAN_temp_daily <- rbind(BRAN_temp_jan, BRAN_temp_feb, BRAN_temp_mar, BRAN_temp_apr,
                         BRAN_temp_may, BRAN_temp_jun, BRAN_temp_jul, BRAN_temp_aug,
                         BRAN_temp_sep, BRAN_temp_oct, BRAN_temp_nov, BRAN_temp_dec)

colnames(BRAN_temp_daily)[4] <- "temp"
save(BRAN_temp_daily, file = "data/BRAN/BRAN_temp_daily.Rdata")

# U
load("data/BRAN/BRAN_u_jan.Rdata")
load("data/BRAN/BRAN_u_feb.Rdata")
load("data/BRAN/BRAN_u_mar.Rdata")
load("data/BRAN/BRAN_u_apr.Rdata")
load("data/BRAN/BRAN_u_may.Rdata")
load("data/BRAN/BRAN_u_jun.Rdata")
load("data/BRAN/BRAN_u_jul.Rdata")
load("data/BRAN/BRAN_u_aug.Rdata")
load("data/BRAN/BRAN_u_sep.Rdata")
load("data/BRAN/BRAN_u_oct.Rdata")
load("data/BRAN/BRAN_u_nov.Rdata")
load("data/BRAN/BRAN_u_dec.Rdata")
BRAN_u_daily <- rbind(BRAN_u_jan, BRAN_u_feb, BRAN_u_mar, BRAN_u_apr,
                         BRAN_u_may, BRAN_u_jun, BRAN_u_jul, BRAN_u_aug,
                         BRAN_u_sep, BRAN_u_oct, BRAN_u_nov, BRAN_u_dec)
colnames(BRAN_u_daily)[4] <- "u"
save(BRAN_u_daily, file = "data/BRAN/BRAN_u_daily.Rdata")

# V
load("data/BRAN/BRAN_v_jan.Rdata")
load("data/BRAN/BRAN_v_feb.Rdata")
load("data/BRAN/BRAN_v_mar.Rdata")
load("data/BRAN/BRAN_v_apr.Rdata")
load("data/BRAN/BRAN_v_may.Rdata")
load("data/BRAN/BRAN_v_jun.Rdata")
load("data/BRAN/BRAN_v_jul.Rdata")
load("data/BRAN/BRAN_v_aug.Rdata")
load("data/BRAN/BRAN_v_sep.Rdata")
load("data/BRAN/BRAN_v_oct.Rdata")
load("data/BRAN/BRAN_v_nov.Rdata")
load("data/BRAN/BRAN_v_dec.Rdata")
BRAN_v_daily <- rbind(BRAN_v_jan, BRAN_v_feb, BRAN_v_mar, BRAN_v_apr,
                      BRAN_v_may, BRAN_v_jun, BRAN_v_jul, BRAN_v_aug,
                      BRAN_v_sep, BRAN_v_oct, BRAN_v_nov, BRAN_v_dec)
colnames(BRAN_v_daily)[4] <- "v"
save(BRAN_v_daily, file = "data/BRAN/BRAN_v_daily.Rdata")

# Merge u & v daily values and save
load("data/BRAN/BRAN_u_daily.Rdata")
load("data/BRAN/BRAN_v_daily.Rdata")
system.time(BRAN_uv_daily <- merge(BRAN_u_daily, BRAN_v_daily, by = c("x", "y", "date"))) # 32 seconds
save(BRAN_uv_daily, file = "data/BRAN/BRAN_uv_daily.Rdata")


# 3. Load, create, and save mean air state files --------------------------

# Function for loading entire ncdf file
# nc.file <- "~/data/ERA/ERA_1979_1989.nc" # testing...
# date_idx <- date_idx$date
# date_idx <- date_idx$date[100:162]
ERA.daily <- function(nc.file){
  nc.stor <- nc_open(nc.file)
  # print(nc.stor)
  date_idx <- ncvar_get(nc.stor, nc.stor$dim[[3]])
  nc_close(nc.stor)
  date_idx <- data.frame(date = unique(as.Date(convertDateNcdf2R(date_idx, units = "hours", 
                                        origin = as.POSIXct("1900-01-01 00:00:0.0", tz = "UTC"), time.format = c("%Y-%m-%d %H:%M:%S")))))
  system.time(ERA <- ERA.ncdf(nc.file, date_idx$date)) # 8 seconds
  system.time(ERA$date <- format(ERA$date, "%m-%d")) # 28 seconds
  ERA <- data.table(ERA)
  system.time(ERA <- ERA[, .(temp = mean(temp, na.rm = TRUE),
                             u = mean(u, na.rm = TRUE),
                             v = mean(v, na.rm = TRUE)), by = .(x,y,date)]) # 14 seconds
  return(ERA)
}

system.time(ERA1 <- ERA.daily("~/data/ERA/ERA_1979_1989.nc")) # 156 seconds
save(ERA1, file = "data/ERA/ERA1.Rdata")
system.time(ERA2 <- ERA.daily("~/data/ERA/ERA_1990_1998.nc")) # 133 seconds
save(ERA2, file = "data/ERA/ERA2.Rdata")
system.time(ERA3 <- ERA.daily("~/data/ERA/ERA_1999_2007.nc")) # 138 seconds
save(ERA3, file = "data/ERA/ERA3.Rdata")
system.time(ERA4 <- ERA.daily("~/data/ERA/ERA_2008_2016.nc")) # 139 seconds
save(ERA4, file = "data/ERA/ERA4.Rdata")


# 4. Combine and save air state files -------------------------------------

load("data/ERA/ERA1.Rdata")
load("data/ERA/ERA2.Rdata")
load("data/ERA/ERA3.Rdata")
load("data/ERA/ERA4.Rdata")

ERA_all_daily <- rbind(ERA1, ERA2, ERA3, ERA4)
ERA_all_daily <- data.table(ERA_all_daily)
system.time(ERA_all_daily <- ERA_all_daily[, .(temp = mean(temp, na.rm = TRUE),
                                               u = mean(u, na.rm = TRUE),
                                               v = mean(v, na.rm = TRUE)), by = .(x,y,date)]) # 1 seconds

save(ERA_all_daily, file = "data/ERA/ERA_all_daily.Rdata")

