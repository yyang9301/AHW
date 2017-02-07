#############################################################################
###"data/download.BRAN.R"
## This script does:
# 1. Test connection and finds BRAN specifics
# 2. Downloads all required BRAN data
## DEPENDS ON:
library(ncdf4)
## USED BY:
# Nothing
## CREATES:
#  downloads all BRAN data required for the AHW project
#############################################################################


# 1. Test connection and finds BRAN specifics -----------------------------

## Testing direct downloads
# Select: variable and year
var='air'
year=2015
# Download netCDF file
url=paste('ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/pressure/',var,'.',year,'.nc', sep='')
dfile=paste(var,'.',year,'.nc', sep='')
download.file(url, destfile=dfile) # This seems to work, but is a 303.9MB file so I did not let it run it's course

nc <- nc_open('AIRS.2003.02.05.L3.RetStd_H001.v6.0.12.0.G14112124328.hdf')

## Testing ncdf4 package against known sources
# Open the file
setwd("~/data/")
nc.file <- "test/cru10min30_tmp.nc" # Accessed from http://geog.uoregon.edu/GeogR/topics/netCDF-in-R.html
nc.file <- "http://geog.uoregon.edu/GeogR/data/raster/cru10min30_tmp.nc"
nc.stor <- nc_open(nc.file)
print(nc.stor)
# Extract lon lat values
nc.data <- list()
nc.data$lat <- ncvar_get(nc.stor,"lat")
nc.data$lon <- ncvar_get(nc.stor,"lon")
range(nc.data$lat)
range(nc.data$lon)
nlat <- -30
slat <- -35
wlon <- 20
elon <- 30
indexLat <- (nc.data$lat<nlat & nc.data$lat>slat)
indexLon <- (nc.data$lon<elon & nc.data$lon>wlon)
# Subset desired data
stor.nc <- list()
# the reference time is the same as the start_time
#stor.nc$reftime <- ncvar_get(nc.stor,"time") # time in seconds since 1981-01-01 00:00:00
#stor.nc$reftime <- as.POSIXct(test, origin="1981-01-01", tz = "UTC")
stor.nc$sstsub <- ncvar_get(nc.stor,"tmp")[indexLon, indexLat,]
stor.nc$lonsub <- ncvar_get(nc.stor,"lon")[indexLon]
stor.nc$latsub <- ncvar_get(nc.stor,"lat")[indexLat]
save(stor.nc, file = "test/test_out.nc")

#nc_close(nc.out)
nc_close(nc.stor)

## The first attempt

nlat <- -25
slat <- -40
wlon <- 10
elon <- 40

setwd("~/data/")
# Create a list of URLs from generated list (Cersat NAIAD)
# txt_files = list.files(path = "/home/robert/Downloads/", pattern = 'granulesList')
# listURL <- list()
# 
# for (i in 1:length(txt_files)){
#   a <- scan(txt_files[i], what="character", sep="\n")
#   listURL <- c(listURL, a)
# }
# rm(a)

listURL <- "http://dapds00.nci.org.au/thredds/dodsC/gb6/BRAN/BRAN_2016/OFAM/ocean_temp_1994_03.nc"

#listURL <- scan("./R/URL_AATSR", what="character", sep="\n")

# basePath <- "/home/robert/GHRSST_data/ghrsst"
# levelPath <- "/L2P"
# productPath <- "/AATSR"
# RDACPath <- "/UPA"
# yearPath <- "/2010"
# varPath <- "/sst/"

# myDir <- paste(basePath,levelPath,productPath,RDACPath,yearPath,varPath,sep="")
myDir <- "~/data/BRAN/ocean_temp/"

totfiles <- length(listURL)

for (i in 1:length(listURL)){
  
  cat("Now on file",i,"of", totfiles,"\n")
  nc.file <- as.character(listURL[i])
  ncfilename <- paste0(myDir,basename(nc.file))
  
  # check if file has not already been saved
  if (!exists(ncfilename)){
    # Open file via OPENDAP
    nc.file <- "http://dapds00.nci.org.au/thredds/dodsC/gb6/BRAN/BRAN_2016/OFAM/ocean_temp_1994_03.nc"
    nc.stor <- nc_open(nc.file)
    # print(nc.stor)
    
    # Create data frame of subsetting variables
    nc.data <- list()
    nc.data$yt_ocean <- ncvar_get(nc.stor,"yt_ocean") # lat
    # range(nc.data$yt_ocean)
    nc.data$xt_ocean <- ncvar_get(nc.stor,"xt_ocean") # lon
    # range(nc.data$xt_ocean)
    nc.data$st_ocean <- ncvar_get(nc.stor,"st_ocean") # depth
    # range(nc.data$st_ocean)
    nc.data$Time <- ncvar_get(nc.stor,"Time") # time
    # range(nc.data$Time)
    
    # Create subsetting indices
    
    # index_yt_ocean <- (nc.data$yt_ocean<nlat & nc.data$yt_ocean>slat) # lat
    index_yt_ocean <- nc.data$yt_ocean[nc.data$yt_ocean<nlat & nc.data$yt_ocean>slat]
    # start_yt_ocean <- which(nc.data$yt_ocean == index_yt_ocean[1])
    
    # index_xt_ocean <- (nc.data$xt_ocean<elon & nc.data$xt_ocean>wlon) $ lon
    index_xt_ocean <- nc.data$xt_ocean[nc.data$xt_ocean<elon & nc.data$xt_ocean>wlon]
    # start_xt_ocean <- which(nc.data$xt_ocean == index_xt_ocean[1])
    
    index_st_ocean <- nc.data$st_ocean[nc.data$st_ocean == 2.5] # Select only first layer of the ocean
    index_Time <- nc.data$Time# Select ALL days of data
    
    # Group it all for prettier coding
    start <- c(which(nc.data$xt_ocean == index_xt_ocean[1]), which(nc.data$yt_ocean == index_yt_ocean[1]), 1, 1) # -1 means use all values
    count <- c(length(index_xt_ocean), length(index_yt_ocean), length(index_st_ocean), length(index_Time))
    
    # Download data as desired from correct subsets
    stor.nc <- list()
    # temp[xt_ocean,yt_ocean,st_ocean,Time] 
    stor.nc$temp <- ncvar_get(nc.stor, "temp", start = start, count = count)
    stor.nc$xt_ocean <- ncvar_get(nc.stor, "xt_ocean", start = start[1], count = count[1])
    stor.nc$yt_ocean <- ncvar_get(nc.stor, "yt_ocean", start = start[2], count = count[2])
    save(stor.nc, file = ncfilename)
    #nc.out <- nc_create(ncfilename, stor.nc, force_v4=TRUE)
    
    #nc_close(nc.out)
    nc_close(nc.stor)
  }
}

# Melt it down for plotting
library(reshape2)
stor2 <- stor.nc$temp[,,1]
rownames(stor2) <- stor.nc$xt_ocean
colnames(stor2) <- stor.nc$yt_ocean
sst <- melt(stor2)
colnames(sst) <- c("x", "y", "temp")
sst$date <- NA # Blank for now

# Test visualisation
library(ggplot2)
ggplot(data = sst, aes(x = x, y = y, colour = temp)) +
  geom_point()
