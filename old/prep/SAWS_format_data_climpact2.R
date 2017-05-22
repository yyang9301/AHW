# Load packages etc. ------------------------------------------------------
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(FNN)
library(doMC); doMC::registerDoMC(cores = 4)
source("func/metaTemp.R") # Afunction that calculates meta-deta/ stats of time series
source("func/scaleBarFunc.R") # A custom ggplot function that creates a very snazy scale bar
source("setupParams/themes.R") # The ggplot theme used for all figures


# Load data ----------------------

load("data/SAWS/SAWS_data_clean.Rdata")


# Change format to match climpact2 requirements ---------------------------

## Separate date column into three columns
SAWS_data_clean$year <- year(SAWS_data_clean$date)
SAWS_data_clean$month <- month(SAWS_data_clean$date)
SAWS_data_clean$day <- day(SAWS_data_clean$date)

## Add dummy precipitation column
SAWS_data_clean$PR <- rep(-99.9, length(SAWS_data_clean$date))

## Change column order
SAWS_climpact2 <- SAWS_data_clean[,c(2,8:11,6:7)]

## Replace NA with -99.9
SAWS_climpact2$mx[is.na(SAWS_climpact2$mx)] <- -99.9
SAWS_climpact2$mn[is.na(SAWS_climpact2$mn)] <- -99.9


# Save each station individually for climpact2 ----------------------------

## For loop to split up stations and save
for(i in 1: length(levels(SAWS_climpact2$station))){
  station_name <- as.character(levels(SAWS_climpact2$station)[i])
  data1 <- droplevels(subset(SAWS_climpact2, station == station_name))
  station_name <- gsub(" ", "", station_name)
  data1 <- data1[2:7]
  write.table(data1, paste("~/climpact2-master/SAWS/", station_name, ".txt", sep = ""), 
            row.names = F, col.names = F, sep = "\t")
}


# Create meta-data file for climpact2 batch processing ----------------------------

## Get file names created above
files_list <- dir("~/climpact2-master/SAWS/", pattern = "*.*.txt", full.names = FALSE)
files_list <- files_list[!(files_list == "batch_metadata.txt")] # Remove meta-data file from list

## Create meta-data shell
station_meta_data <- SAWS_data_clean[2:4] %>% 
  mutate(station_file = paste(gsub(" ", "", station), ".txt", sep = "")) %>% 
  group_by(station_file) %>% 
  mutate(latitude = mean(lat)) %>% 
  mutate(longitude = mean(lon)) %>% 
  select(station_file, latitude, longitude) %>% 
  unique()

## Check that file names match
files_list %in% unique(station_meta_data$station_file) # Should all be TRUE

## Add the meta-data commands for climpact2

# wsdin - Number of consecutive warm >90th days
station_meta_data$wsdin <- 7

# csdin - Number of consecutive cold <10th days
station_meta_data$csdin <- 7

# Tb_HDD - for HDDheat, set the temperature to be used in the subtraction in these indices
station_meta_data$Tb_HDD <- 18

# Tb_CDD - for CDDcold, set the temperature to be used in the subtraction in these indices
station_meta_data$Tb_CDD <- 18

# Tb_GDD - for GDDgrow, set the temperature to be used in the subtraction in these indices
station_meta_data$Tb_GDD <- 10

# rxnday - the monthly maximum consecutive n-day precipitation to be recorded by the Rxnday index
station_meta_data$rxnday <- 3

# rnnmm - counts number of days where precipitation is > the given number
station_meta_data$rnnmm <- 30

# txtn - the number of consecutive days required for the nTXnTN and nTXbnTNb indices
station_meta_data$txtn <- 7

# SPEI - calculate SPE/ SPEI over custom months
station_meta_data$SPEI <- 24


## Save
write.table(station_meta_data, paste("~/climpact2-master/SAWS/batch_metadata.txt", sep = ""), 
            row.names = F, col.names = T, sep = "\t", quote = F)

## Check that correct number of rows and columns were created
count.fields("~/climpact2-master/SAWS/batch_metadata.txt")


# Run the climpact2 batch processor ---------------------------------------

## Open a terminal and run the following two lines without the "#"
    ## NB: You must already have downloaded and set up the climpact2 batch processor
      ## https://github.com/ARCCSS-extremes/climpact2

# cd climpact2-master/

# Rscript climpact2.batch.stations.r ./SAWS/ ./SAWS/batch_metadata.txt 1915 2016 4
