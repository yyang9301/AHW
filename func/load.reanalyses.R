#############################################################################
###"func/load.reanalyses.R"
## This script does:
# 1. Create a function for loading BRAN data
# 2. Create a function for loading ERA Interim data
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(plyr)
library(dplyr)
library(reshape2)
library(ncdf4)
library(ncdf.tools)
## USED BY:
# "graph/figures3.R"
## CREATES:
# BRAN dataframes as requested
# ERA Interim dataframes as requested
#############################################################################


# 1. Create a function for loading BRAN data ------------------------------

# The BRAN data have already been downloaded and saved as lists with .Rdata file extension
# It is therefore necessary to pass individual file names to the following function so that
# it may load and subset only the files required
# Note that this does not subset any dates etc. presently

# The .Rdata format
# x <- files_list[1,] # tester...
BRAN.Rdata <- function(x){
  BRAN_file <- as.character(x$files)
  load(BRAN_file)
  stor_length <- nrow(as.data.frame(stor.nc$var[1,1,]))
  # Unfortunately the date value is not saved directly in the list
  # It is necessary to use a for loop to construct the date while subsetting
  BRAN_data <- data.frame()
  for(i in 1:stor_length){
    # Subset and melt one day of data
    stor2 <- stor.nc$var[,,i]
    rownames(stor2) <- stor.nc$x
    colnames(stor2) <- stor.nc$y
    stor3 <- melt(stor2)
    stor_var <- sapply(strsplit(basename(BRAN_file), "_"), "[[", 2)
    colnames(stor3) <- c("x", "y", stor_var)
    stor_year <- sapply(strsplit(basename(BRAN_file), "_"), "[[", 3)
    stor_month <- sapply(strsplit(basename(BRAN_file), "_"), "[[", 4)
    stor_month <- sapply(strsplit(stor_month, ".Rdata"), "[[", 1)
    stor_day <- i
    stor3$date <- as.Date(paste0(stor_year,"-",stor_month,"-",stor_day))
    BRAN_data <- rbind(BRAN_data, stor3)
  }
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
  start <- c(which(nc.data$x == index_x[1]), which(nc.data$y == index_y[1]), which(nc.data$t == index_t[1]))
  count <- c(length(index_x), length(index_y), length(index_t))
  
  # Download data as desired from correct subsets
  stor.nc <- list()
  # var[x, y, z, t] # The subsetting format
  stor.nc$temp <- ncvar_get(nc.stor, nc.stor$var[[3]], start = start, count = count)-273.15 # K to C
  stor.nc$u <- ncvar_get(nc.stor, nc.stor$var[[1]], start = start, count = count)
  stor.nc$v <- ncvar_get(nc.stor, nc.stor$var[[2]], start = start, count = count)
  stor.nc$x <- ncvar_get(nc.stor, nc.stor$dim[[1]], start = start[1], count = count[1])
  stor.nc$y <- ncvar_get(nc.stor, nc.stor$dim[[2]], start = start[2], count = count[2])
  stor.nc$t <- ncvar_get(nc.stor, nc.stor$dim[[3]], start = start[3], count = count[3])
  stor.nc$t <-  convertDateNcdf2R(stor.nc$t, units = "hours", 
                                  origin = as.POSIXct("1900-01-01 00:00:0.0", tz = "UTC"), time.format = c("%Y-%m-%d %H:%M:%S"))
  nc_close(nc.stor)
  stor_length <- nrow(as.data.frame(stor.nc$temp[1,1,]))
  
  # Convert from a list to a dataframe
  # This will be converted to a proper function just now
  # Temp
  ERA_temp <- data.frame()
  for(i in 1:stor_length){
    # Subset and melt one day of data
    temp <- stor.nc$temp[,,i]
    rownames(temp) <- stor.nc$x
    colnames(temp) <- stor.nc$y
    stor3 <- melt(temp)
    colnames(stor3) <- c("x", "y", "var")
    # stor3$date <- stor.nc$t[i]
    # stor3$stat <- "temp"
    ERA_temp <- rbind(ERA_temp, stor3)
  }
  ERA_temp <- ddply(ERA_temp, .(x, y), summarise, var = mean(var, na.rm = T), .parallel = T)
  ERA_temp$stat <- "temp"
  
  # U
  ERA_u <- data.frame()
  for(i in 1:stor_length){
    # Subset and melt one day of data
    u <- stor.nc$u[,,i]
    rownames(u) <- stor.nc$x
    colnames(u) <- stor.nc$y
    stor3 <- melt(u)
    colnames(stor3) <- c("x", "y", "var")
    # stor3$date <- stor.nc$t[i]
    # stor3$stat <- "u"
    ERA_u <- rbind(ERA_u, stor3)
  }
  ERA_u <- ddply(ERA_u, .(x, y), summarise, var = mean(var, na.rm = T), .parallel = T)
  ERA_u$stat <- "u"
  
  # V
  ERA_v <- data.frame()
  for(i in 1:stor_length){
    # Subset and melt one day of data
    v <- stor.nc$v[,,i]
    rownames(v) <- stor.nc$x
    colnames(v) <- stor.nc$y
    stor3 <- melt(v)
    colnames(stor3) <- c("x", "y", "var")
    # stor3$date <- stor.nc$t[i]
    # stor3$stat <- "v"
    ERA_v <- rbind(ERA_v, stor3)
  }
  ERA_v <- ddply(ERA_v, .(x, y), summarise, var = mean(var, na.rm = T), .parallel = T)
  ERA_v$stat <- "v"
  
  # Combine
  ERA_all <- rbind(ERA_temp, ERA_u, ERA_v)
  return(ERA_all)
}
