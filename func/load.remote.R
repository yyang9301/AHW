#############################################################################
###"func/load.remote.R"
## This script does:
# 1. Create a function for loading OISST data
# 2. Create a function for loading AVISO data

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

AVISO.daily <- function(x){
  # Load and extract U and V
  nc.file <- as.character(x$files)
  nc <- nc_open(nc.file)
  ssh_u <- ncvar_get(nc, varid = "u") %>%
    round(4)
  ssh_v <- ncvar_get(nc, varid = "v") %>%
    round(4)
  dimnames(slha_u) <- list(lon = nc$dim$lon$vals, lat = nc$dim$lat$vals)
  dimnames(slha_v) <- list(lon = nc$dim$lon$vals, lat = nc$dim$lat$vals)
  nc_close(nc)
  
  ## Melt and trim to study area
  # U
  ssh_u <- as.data.frame(melt(ssh_u, value.name = "u"), row.names = NULL) %>%
    rename(x = Var1, y = Var2) %>% 
    filter((x >= sa_lons[1]) & (x <= sa_lons[2])) %>%
    filter((y >= sa_lats[1]) & (y <= sa_lats[2])) %>%
    na.omit() %>% 
    mutate(date = time) %>% 
    mutate(x = plyr::round_any(x, 0.5)) %>% 
    mutate(y = plyr::round_any(y, 0.5))
  ssh_u <- data.table::data.table(ssh_u)
  ssh_u <- ssh_u[, .(u = mean(u, na.rm = TRUE)), 
                       by = .(x, y, date)]
  ssh_u <- data.frame(ssh_u)
  ssh_u <- ssh_u[,c(1,2,4,3)]
  ssh_u <- ssh_u[complete.cases(ssh_u), ]
  
  # V
  ssh_v <- as.data.frame(melt(ssh_v, value.name = "v"), row.names = NULL) %>%
    rename(x = Var1, y = Var2) %>% 
    filter((x >= sa_lons[1]) & (x <= sa_lons[2])) %>%
    filter((y >= sa_lats[1]) & (y <= sa_lats[2])) %>%
    na.omit() %>% 
    mutate(date = time) %>% 
    mutate(x = plyr::round_any(x, 0.5)) %>% 
    mutate(y = plyr::round_any(y, 0.5))
  ssh_v <- data.table::data.table(ssh_v)
  ssh_v <- ssh_v[, .(v = mean(v, na.rm = TRUE)), 
                       by = .(x, y, date)]
  ssh_v <- data.frame(ssh_v)
  ssh_v <- ssh_v[,c(1,2,4,3)]
  ssh_v <- ssh_v[complete.cases(ssh_v), ]
  
  # Combine and finish
  ssh_uv <- ssh_u
  ssh_uv$v <- ssh_v$v
  return(ssh_uv)
}

# uv_scalar <- 1
# 
# ggplot(slha_uv, aes(x = lon, y = lat)) +
#   geom_segment(aes(xend = lon + u * uv_scalar, yend = lat + v * uv_scalar),
#                arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2)
