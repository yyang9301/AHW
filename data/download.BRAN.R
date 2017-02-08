#############################################################################
###"data/download.BRAN.R"
## This script does:
# 1. Download all required BRAN data
## DEPENDS ON:
library(ncdf4)
## USED BY:
# Nothing
## CREATES:
#  downloads all BRAN data required for the AHW project
#############################################################################


# 1. Download all required BRAN data --------------------------------------

# The pathway where the files are saved
  # NB: The files are saved locally on Robert's computer and are not on GitHub or DropBox
BRANdir <- "~/data/BRAN/"

# The lon/ lat ranges
wlon <- 10
elon <- 40
nlat <- -25
slat <- -40

## Download the BRAN data
# select only specific variables to control the download process
vars <- c("ocean_temp", "ocean_u", "ocean_v", "ocean_w")
# vars <- c("ocean_temp") 
month.special <- c("01","02","03","04","05","06","07","08","09","10","11","12") # months must be this way for URLs to work
month.incomplete <- c("09","10","11","12") # These months are not yet available for 2016
for (i in vars){ # the variable level
  # var1 <- vars[i]
  for (j in 1994:2016){ # the year level
    for (k in month.special){ # the month level
      nc.file <- paste0("http://dapds00.nci.org.au/thredds/dodsC/gb6/BRAN/BRAN_2016/OFAM/",i,"_",j,"_",k,".nc")
      # ncfilename <- paste0(BRANdir,basename(nc.file)) # rather save the output in .Rdata format
      ncfilename <- paste0(BRANdir,i,"_",j,"_",k,".Rdata")
      if(j == 2016 & k %in% month.incomplete){ # There are no months available for 2016/09 onwards
        cat(basename(ncfilename),"not yet available for download","\n")
      } else {
        if(file.exists(ncfilename)){ # check if file has not already been saved
          cat(basename(ncfilename),"has already been downloaded","\n")
        } else if(!file.exists(ncfilename)){
          cat("Now creating", basename(ncfilename),"\n")
          # Open file via OPENDAP
          nc.stor <- nc_open(nc.file)
          # print(nc.stor)
          
          # Create data frame of subsetting variables
          nc.data <- list()
          nc.data$x <- ncvar_get(nc.stor, nc.stor$dim[[5]]) # lon
          # range(nc.data$xt_ocean)
          nc.data$y <- ncvar_get(nc.stor, nc.stor$dim[[6]]) # lat
          # range(nc.data$yt_ocean)
          nc.data$z <- ncvar_get(nc.stor, nc.stor$dim[[4]]) # depth
          # range(nc.data$st_ocean)
          nc.data$t <- ncvar_get(nc.stor, nc.stor$dim[[1]]) # time
          # range(nc.data$Time)
          
          # Create subsetting indices
          index_x <- nc.data$x[nc.data$x<=elon & nc.data$x>=wlon] # lon
          index_y<- nc.data$y[nc.data$y<=nlat & nc.data$y>=slat] # lat
          index_z <- nc.data$z[nc.data$z == 2.5] # Select only first depth layer of the ocean
          index_t <- nc.data$t # Select ALL days of data
          
          # Group it all for prettier coding
          start <- c(which(nc.data$x == index_x[1]), which(nc.data$y == index_y[1]), 1, 1)
          count <- c(length(index_x), length(index_y), length(index_z), length(index_t))
          
          # Download data as desired from correct subsets
          stor.nc <- list()
          # var[x, y, z, t] # The subsetting format
          
          test <- ncvar_get(nc.stor, nc.stor$dim[[6]], start = start[2], count = count[2])
          
          stor.nc$var <- ncvar_get(nc.stor, nc.stor$var[[5]], start = start, count = count)
          stor.nc$x <- ncvar_get(nc.stor, nc.stor$dim[[5]], start = start[1], count = count[1])
          stor.nc$y <- ncvar_get(nc.stor, nc.stor$dim[[6]], start = start[2], count = count[2])
          save(stor.nc, file = ncfilename)
          nc_close(nc.stor)
        }
      }
    }
  }
}


 
# Load an ncdf file to ensure it saved correctly
load("~/data/BRAN/ocean_v_1994_01.Rdata")

# Melt it down for plotting
library(reshape2)
stor2 <- stor.nc$var[,,1]
rownames(stor2) <- stor.nc$x
colnames(stor2) <- stor.nc$y
sst <- melt(stor2)
colnames(sst) <- c("x", "y", "temp")
sst$date <- NA # Blank for now

# Test visualisation
library(ggplot2)
ggplot(data = sst, aes(x = x, y = y, colour = temp)) +
  geom_point()

