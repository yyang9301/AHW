#############################################################################
###"func/load.remote.R"
## This script does:
# 1. Create a function for loading OISST data
# 2. Create a function for loading AVISO data
# 3. Create function for loading all AVISO days
# 4. Function for creating clim anomalies
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(tidyverse)
library(reshape2)
library(ncdf4)
library(ncdf.tools)
## USED BY:
# "1.Data_assembly.R"
## CREATES:
# OISST dataframes as requested
# AVISO dataframes as requested
#############################################################################

# The default lon/ lat ranges
wlon <- 10
elon <- 40
nlat <- -25
slat <- -40

sa_lons <- c(10, 40); sa_lats <- c(-40, -25)


# 1. Create a function for loading OISST data -----------------------------

# The ERA Interim data have been downloaded across four ncdf files
# It is therefore necessary to pass to this function which file should be loaded
# And therein, which dates should be subsetted
OISST.daily <- function(x){
  # Load and extract sst
  nc.file <- as.character(x$files)
  nc <- nc_open(nc.file)
  # nc <- nc_open("~/data/OISST/netCDF/avhrr-only-v2.20041023.nc")
  # nc <- nc_open(OISST_idx[1,1])
  sst <- ncvar_get(nc, varid = "sst")
  time <- convertDateNcdf2R(ncvar_get(nc, varid = "time"), units = "days", 
                            origin = as.POSIXct("1978-01-01 00:00:00", tz = "UTC"), time.format = c("%Y-%m-%d %H:%M:%S"))
  colnames(sst) <- nc$dim$lat$vals
  rownames(sst) <- nc$dim$lon$vals
  nc_close(nc)
  
  # Melt and trim to study area
  sst_long <- as.data.frame(melt(sst, value.name = "sst"), row.names = NULL) %>%
    rename(x = Var1, y = Var2) %>% 
    filter((x >= sa_lons[1]) & (x <= sa_lons[2])) %>%
    filter((y >= sa_lats[1]) & (y <= sa_lats[2])) %>%
    na.omit() %>% 
    mutate(date = time) %>% 
    mutate(x = plyr::round_any(x, 0.5)) %>% 
    mutate(y = plyr::round_any(y, 0.5))
  sst_long <- data.table::data.table(sst_long)
  sst_long <- sst_long[, .(sst = mean(sst, na.rm = TRUE)), 
                       by = .(x, y, date)]
  sst_long <- data.frame(sst_long)
  sst_long <- sst_long[,c(1,2,4,3)]
  sst_long <- sst_long[complete.cases(sst_long), ]
  
  # Finish
  return(sst_long)
}


# 2. Create a function for loading AVISO data -----------------------------

AVISO.ncdf <- function(nc.file, date_idx){
  # Load and extract U and V
  # nc.file <- "~/data/AVISO/dataset-duacs-rep-global-merged-allsat-phy-l4-v3_19930101-19991231.nc"
  nc <- nc_open(nc.file)
  time <- as.character(convertDateNcdf2R(ncvar_get(nc, varid = "time"), units = "days", 
                            origin = as.POSIXct("1950-01-01 00:00:00", tz = "UTC"), time.format = c("%Y-%m-%d %H:%M:%S")))
  index_t <- time[as.Date(time) %in% date_idx]
  lon <- nc$dim$lon$vals; lat <- nc$dim$lat$vals
  
  ssh_u <- (ncvar_get(nc, varid = "ugos")[,,which(time %in% index_t)])
  ssh_v <- (ncvar_get(nc, varid = "vgos")[,,which(time %in% index_t)])
  
  dimnames(ssh_u) <- list(lon = lon, lat = lat, time = index_t)
  dimnames(ssh_v) <- list(lon = lon, lat = lat, time = index_t)
  
  nc_close(nc)
  
  # Function for converting a variable from a list to a dataframe
  AVISO.to.df <- function(df, val){
    df1 <- as.data.frame(melt(df, value.name = "val"), row.names = NULL) %>%
      rename(x = lon, y = lat, date = time) %>% 
      filter((x >= sa_lons[1]) & (x <= sa_lons[2])) %>%
      filter((y >= sa_lats[1]) & (y <= sa_lats[2])) %>%
      na.omit() %>% 
      mutate(val = round(val, 4)) %>% 
      mutate(x = plyr::round_any(x, 0.5)) %>% 
      mutate(y = plyr::round_any(y, 0.5))
    df1 <- data.table::data.table(df1)
    df1 <- df1[, .(val = mean(val, na.rm = TRUE)), 
                 by = .(x, y, date)]
    df1 <- data.frame(df1)
    df1 <- df1[complete.cases(df1), ]
    colnames(df1)[4] <- val
    df1$date <- as.Date(df1$date)
    return(df1)
  }
  
  ## Melt and trim to study area
  # U
  ssh_u_long <- AVISO.to.df(ssh_u, "u")
  # V
  ssh_v_long <- AVISO.to.df(ssh_v, "v")
  
  # Combine and finish
  ssh_uv_long <- ssh_u_long
  ssh_uv_long$v <- ssh_v_long$v
  return(ssh_uv_long)
}

# uv_scalar <- 1
# ggplot(ssh_uv_long[ssh_uv_long$date == as.Date("1993-01-01"),], aes(x = x, y = y)) +
#   geom_segment(aes(xend = x + u * uv_scalar, yend = y + v * uv_scalar),
#                arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2)


# 3. Create function for loading all AVISO days ---------------------------

AVISO.daily <- function(nc.file){
  nc <- nc_open(nc.file)
  # print(nc.stor)
  # date_idx <- ncvar_get(nc, nc.stor$dim[[3]])
  date_idx <- data.frame(date = convertDateNcdf2R(ncvar_get(nc, varid = "time"), units = "days", 
                                                               origin = as.POSIXct("1950-01-01 00:00:00", tz = "UTC"), 
                                                               time.format = c("%Y-%m-%d %H:%M:%S")))
  nc_close(nc)
  system.time(AVISO <- AVISO.ncdf(nc.file, date_idx = as.Date(date_idx$date))) # 8 seconds
  return(AVISO)
}


# 4. Function for creating clim anomalies ---------------------------------

clim.anom <- function(df){
  # First create mean
  colnames(df)[3] <- val
  df1 <- data.table::data.table(df)
  df1 <- df1[, .(val = mean(val, na.rm = TRUE)),
                       by = .(x, y)]
  # Then order the two data frames the same
  df <- df[order(df$date, df$x),]
  df1 <- df1[order(df1$x),]
  # Lastly subtract the mean for anomalies
  df2 <- df %>%
    mutate(val = val-df1$val)
  # mean(df1$value)
}

