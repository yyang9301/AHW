#############################################################################
###"func/load.reanalyses.R"
## This script does:
# 1. Create a function for loading BRAN data
# 2. Create a function for loading ERA Interim data
# 3. Create function for loading all ERA days
# 4. Create function for calculating daily clims from a grid
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
# library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(ncdf4)
library(ncdf.tools)
## USED BY:
# "1.Data_assembly.R"
## CREATES:
# BRAN dataframes as requested
# ERA Interim dataframes as requested
# Gridded daily clims as requested
#############################################################################

# The default lon/ lat ranges
wlon <- 10
elon <- 40
nlat <- -25
slat <- -40

sa_lons <- c(10, 40); sa_lats <- c(-40, -25)

# 1. Create a function for loading BRAN data ------------------------------

# The BRAN data have already been downloaded and saved as lists with .Rdata file extension
# It is therefore necessary to pass individual file names to the following function so that
# it may load and subset only the files required
# Note that this does not subset any dates etc.

# The .Rdata format
# x <- files_list[1,] # tester...
BRAN.Rdata <- function(x){
  BRAN_file <- as.character(x$files)
  load(BRAN_file)
  stor_length <- nrow(as.data.frame(stor.nc$var[1,1,]))
  stor_year <- sapply(strsplit(basename(BRAN_file), "_"), "[[", 3)
  stor_month <- sapply(strsplit(basename(BRAN_file), "_"), "[[", 4)
  stor_month <- sapply(strsplit(stor_month, ".Rdata"), "[[", 1)
  stor_date <- seq(as.Date(paste(stor_year, stor_month,1, sep = "-")), 
                   as.Date(paste(stor_year, stor_month,stor_length, sep = "-")), by = "day")
  
  # Function for converting a variable from a list to a dataframe
  BRAN.to.df <- function(){
    df1 <- data.frame(stor.nc[1])
    rownames(df1) <- stor.nc$x
    colnames(df1) <- rep(stor.nc$y, stor_length)
    df2 <- melt(as.matrix(df1), value.name = "var")
    df2$date <- rep(stor_date, each = (length(stor.nc$x)*length(stor.nc$y))) # At this step the hour values are removed
    colnames(df2)[1:2] <- c("x","y")
    df2 <- df2 %>% 
      mutate(x = plyr::round_any(x, 0.5)) %>% 
      mutate(y = plyr::round_any(y, 0.5))
    df2 <- data.table(df2)
    df2 <- df2[, .(var = mean(var, na.rm = TRUE)),
                       by = .(x, y, date)]
    df2 <- data.frame(df2)
    df2 <- df2[,c(1,2,4,3)]
    df2 <- df2[complete.cases(df2), ]
    return(df2)
  }
  
  # Run and exit
  BRAN_data <- BRAN.to.df()
  return(BRAN_data)
}


# 2. Create a function for loading ERA Interim data -----------------------

# The ERA Interim data have been downloaded across four ncdf files
# It is therefore necessary to pass to this function which file should be loaded
# And therein, which dates should be subsetted

ERA.ncdf <- function(nc.file, date_idx){
  nc.stor <- nc_open(nc.file)
  # print(nc.stor)
  
  # Create data frame of subsetting variables
  nc.data <- list()
  nc.data$x <- ncvar_get(nc.stor, nc.stor$dim[[1]]) # lon
  nc.data$y <- ncvar_get(nc.stor, nc.stor$dim[[2]]) # lat
  nc.data$t <- ncvar_get(nc.stor, nc.stor$dim[[3]]) # time
  # units: hours since 1900-01-01 00:00:0.0
  nc.data$t <-  convertDateNcdf2R(nc.data$t, units = "hours", 
                                  origin = as.POSIXct("1900-01-01 00:00:0.0", tz = "UTC"), time.format = c("%Y-%m-%d %H:%M:%S"))
  
  # Create subsetting indices
  index_x <- nc.data$x[nc.data$x<=elon & nc.data$x>=wlon] # lon
  index_y <- nc.data$y[nc.data$y<=nlat & nc.data$y>=slat] # lat
  index_t <- nc.data$t[as.Date(nc.data$t) %in% date_idx] # days
  
  # Group it all for prettier coding
  start <- c(which(nc.data$x == index_x[1]), which(nc.data$y == index_y[1]), 1)
  count <- c(length(index_x), length(index_y), -1)
  
  # Download data as desired from correct subsets
  stor.nc <- list()
  # var[x, y, z, t] # The subsetting format
  stor.nc$temp <- (ncvar_get(nc.stor, nc.stor$var[[3]], start = start, count = count)[,,which(nc.data$t %in% index_t)])-273.15 # K to C
  stor.nc$u <- (ncvar_get(nc.stor, nc.stor$var[[1]], start = start, count = count)[,,which(nc.data$t %in% index_t)])
  stor.nc$v <- (ncvar_get(nc.stor, nc.stor$var[[2]], start = start, count = count)[,,which(nc.data$t %in% index_t)])
  stor.nc$x <- ncvar_get(nc.stor, nc.stor$dim[[1]], start = start[1], count = count[1])
  stor.nc$y <- ncvar_get(nc.stor, nc.stor$dim[[2]], start = start[2], count = count[2])
  stor.nc$t <- (ncvar_get(nc.stor, nc.stor$dim[[3]], start = start[3])[which(nc.data$t %in% index_t)])
  stor.nc$t <-  convertDateNcdf2R(stor.nc$t, units = "hours", 
                                  origin = as.POSIXct("1900-01-01 00:00:0.0", tz = "UTC"), time.format = c("%Y-%m-%d %H:%M:%S"))
  nc_close(nc.stor)
  stor_length <- nrow(as.data.frame(stor.nc$temp[1,1,]))
  
  # Function for converting a variable from a list to a dataframe
  ERA.to.df <- function(col){
    df1 <- data.frame(stor.nc[col])
    rownames(df1) <- stor.nc$x
    colnames(df1) <- rep(stor.nc$y, stor_length)
    df2 <- melt(as.matrix(df1), value.name = "var")
    df2$date <- rep(as.Date(stor.nc$t), each = (length(stor.nc$x)*length(stor.nc$y))) # At this step the hour values are removed
    colnames(df2)[1:2] <- c("x","y")
    df2 <- data.table(df2)
    df2 <- df2[, .(var = mean(var, na.rm = TRUE)), by = .(x,y,date)]
    return(df2)
  }
  
  # Temperature
  ERA_temp <- ERA.to.df(1)
  colnames(ERA_temp)[4] <- "temp"
  
  # U
  ERA_u <- ERA.to.df(2)
  colnames(ERA_u)[4] <- "u"
  
  # V
  ERA_v <- ERA.to.df(3)
  colnames(ERA_v)[4] <- "v"
  
  # Combine
  ERA_uv <- merge(ERA_u, ERA_v, by = c("x", "y", "date"))
  ERA_all <- merge(ERA_temp, ERA_uv, by = c("x", "y", "date"))
  # ERA_all <- order(ERA_all, ERA_all$x)
  return(ERA_all)
}


# 3. Create function for loading all ERA days -----------------------------

ERA.daily <- function(nc.file){
  nc.stor <- nc_open(nc.file)
  # print(nc.stor)
  date_idx <- ncvar_get(nc.stor, nc.stor$dim[[3]])
  nc_close(nc.stor)
  date_idx <- data.frame(date = unique(as.Date(convertDateNcdf2R(date_idx, units = "hours", 
                                                                 origin = as.POSIXct("1900-01-01 00:00:0.0", tz = "UTC"), 
                                                                 time.format = c("%Y-%m-%d %H:%M:%S")))))
  system.time(ERA <- ERA.ncdf(nc.file, date_idx$date)) # 8 seconds
  return(ERA)
}


# 4. Create function for calculating daily clims from a grid --------------
# Use development version of detect()', not the one on CRAN
source("~/RmarineHeatWaves/R/RmarineHeatWaves.R")
grid.clim <- function(df) {
  start <- min(df$date, na.rm = T) 
  end <- max(df$date, na.rm = T)
  colnames(df) <- c("x", "y", "temp", "t")
  whole <- RmarineHeatWaves::make_whole(df)
  res <- detect(whole, climatology_start = start, climatology_end = end, clim_only = T)
  res <- unique(res[,c(1,4)])
  res <- res[order(res$doy), ]
  res$doy <- format(seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day"), "%m-%d")
  colnames(res) <- c("date", "clim")
  return(res)
}
