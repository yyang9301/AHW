#############################################################################
## This script does:
# 1. loads AHW results;
# 2. Prep AHW results for comparison
# 3. loads MHW results;
# 4. Prep MHW results for comparison;
# 5. Calculates co-occurrence of events for all sites vs stations;

#############################################################################

#############################################################################
## DEPENDS ON:
require(ggplot2)
require(stringr)
require(plyr)
require(zoo)
require(lubridate)
require(stringr)
library(reshape2)
library(doMC); doMC::registerDoMC(cores = 4)
source("setupParams/themes.R")
source("func/earthdist.R")
#############################################################################

#############################################################################
## USED BY:
# 
#############################################################################

#############################################################################
## CREATES:
# "data/mhwCO.csv"
# "data/mcsCO.csv"
#############################################################################


# 1. Load all AHW results ----------------------------------------------------

## Set the directory where the climpact2 output may be found
dir1 <- "~/climpact2-master/SAWS/indices"
folder_list <-  dir(dir1, full.names = TRUE)

allACW <- data.frame()
allAHW <- data.frame()
for(i in 1:length(folder_list)){
  files_list1 <- dir(folder_list[i], pattern = "heatwave", full.names = TRUE)
  files_list2 <- dir(folder_list[i], pattern = "heatwave", full.names = FALSE)
  if(length(files_list1) < 1){
    warning(paste("No events detected at: ", folder_list[i], sep = ""))
  } else {
    for(j in 1:length(files_list1)){
      x <- read.csv(files_list1[j], skip = 6)
      station_name <- sapply(strsplit(files_list2[j], "_"), "[[", 1)
      stat <- sapply(strsplit(files_list2[j], "_"), "[[", 2)
      stat <- gsub("_", " ", stat)
      stat <- gsub(".csv", "", stat)
      y <- data.frame(station_name, stat, x)
      y[y == -99.9] <- NA
      if (colnames(y[4]) == "HWM"){
        allAHW <- rbind(allAHW, y)
      } else if (colnames(y[4]) == "CWM"){
        allACW <- rbind(allACW, y)
      }
    }
  }
}

## Sites with no events 
  # Created manually from warning output
no_events <- c("EASTLONDONWOAWS", "GRABOUW", "PORTELIZABETH-CHATTYAWS", "PORTNOLLOTH-VRT")

## Drop Tx90and Tn90 for now...
allAHW <- allAHW[allAHW$stat == "EHF",]

## Sites that didn't calculate correctly for some reason
bad_station <- ("EASTLONDONAWS")

# Remove the site from the results
allAHW <- droplevels(allAHW[!(allAHW$station_name %in% bad_station),])
allACW <- droplevels(allACW[!(allACW$station_name %in% bad_station),])

# 2. Prep AHW results for comparison --------------------------------------

## Correct columns to better match MHW output
# Heatwaves
allAHW$year <- allAHW$time
allAHW$frequency <- allAHW$HWN
allAHW$duration <- allAHW$HWF/allAHW$HWN
allAHW$intensity <- allAHW$HWM
allAHW <- allAHW[,c(1:2,9,10:12)]
# Heatwaves
allACW$year <- allACW$time
allACW$frequency <- allACW$CWN
allACW$duration <- allACW$CWF/allACW$CWN
allACW$intensity <- allACW$CWM
allACW <- allACW[,c(1:2,9,10:12)]

## Load station meta-data
station_meta_data <- read.table("~/climpact2-master/SAWS/batch_metadata.txt", sep = "\t", header = T)
station_meta_data$station_file <- gsub(".txt", "", station_meta_data$station_file)

## Add lon/ lat to results data
# Heatwaves
allAHW <- allAHW %>%
  group_by(station_name) %>%
  mutate(lon = station_meta_data$longitude[station_meta_data$station_file == station_name][1]) %>% 
  mutate(lat = station_meta_data$latitude[station_meta_data$station_file == station_name][1])
allAHW <- data.frame(allAHW)
# Coldwaves
allACW <- allACW %>%
  group_by(station_name) %>%
  mutate(lon = station_meta_data$longitude[station_meta_data$station_file == station_name][1]) %>% 
  mutate(lat = station_meta_data$latitude[station_meta_data$station_file == station_name][1])
allACW <- data.frame(allACW)

# 3. Load all MHW results ---------------------------------------------------------

## First specify coastal sections
wc <- c("Hout Bay", "Kommetjie", "Port Nolloth", "Sea Point")
sc <- c("Fish Hoek", "Gordons Bay", "Hamburg", "Hermanus", "Humewood", "Knysna",
        "Mossel Bay", "Muizenberg", "Pollock Beach", "Tsitsikamma West",
        "Storms River Mouth", "Tsitsikamma East", "Ystervarkpunt")
ec <- c("Eastern Beach", "Nahoon Beach", "Orient Beach", "Sodwana")

# Then specify directories for loading
# Annual values
dir1 <- "~/MHW/data/MHW/annual/"
dir2 <- "~/MHW/data/MCS/annual/"

# Load annual event stats
annualLoad <- function(dir) {
  fname1 <-  dir(dir, full.names = TRUE)
  fname2 <-  dir(dir, full.names = FALSE)
  pf <- str_sub(fname2, -20, -1)
  siteNames1 <-  unlist(strsplit(dir(dir, full.names = FALSE), pf[1]))
  siteNames1 <-  str_replace_all(siteNames1, "_", " ") # parse names for site column
  dat <- data.frame()
  for(i in 1:length(fname1)) { # A shameful for loop... in order to label sites correctly
    x <- read.csv(fname1[i], header = TRUE, skip = 2)
    x$site <-  siteNames1[i]
    if(x$site[1] %in% wc) {
      x$coast <- "wc"
    } else if(x$site[1] %in% sc) {
      x$coast <- "sc"
    } else if(x$site[1] %in% ec) {
      x$coast <- "ec"
    }
    x <- x[,c(25:26,1,2,3,5)]
    colnames(x)[3:6] <- c("year","frequency","duration","intensity")
    x[is.na(x$frequency)] <- 0
    dat <- rbind(dat, x)
  }
  dat$coast <- factor(dat$coast, levels = c("wc", "sc", "ec"))
  return(dat)
}

mhwAnnual <- annualLoad(dir1)
mcsAnnual <- annualLoad(dir2)


# 4. Prep AHW results for comparison --------------------------------------

## Load metadata
load("~/MHW/data/metaData2.Rdata")

## Add lon/ lat to results data
# Heatwaves
mhwAnnual <- mhwAnnual %>%
  group_by(site) %>%
  mutate(lon = metaData2$lon[metaData2$site == site][1]) %>% 
  mutate(lat = metaData2$lat[metaData2$site == site][1])
mhwAnnual <- data.frame(mhwAnnual)
# Coldwaves
mcsAnnual <- mcsAnnual %>%
  group_by(site) %>%
  mutate(lon = metaData2$lon[metaData2$site == site][1]) %>% 
  mutate(lat = metaData2$lat[metaData2$site == site][1])
mcsAnnual <- data.frame(mcsAnnual)


# 5. Calculates co-occurrence of events for all sites vs stations ---------

MHW <- mhwAnnual
AHW <- allAHW

cooccurrence <- function(MHW, AHW){
  overall_cooccurrence <- data.frame()
  for(i in 1:length(levels(as.factor(MHW$site)))){
    mhw1 <- droplevels(subset(MHW, site == levels(as.factor(MHW$site))[i]))
    mhw1$lon <- deg2rad(mhw1$lon); mhw1$lat <- deg2rad(mhw1$lat)
    for(j in 1:length(levels(as.factor(AHW$station_name)))){
      ahw1 <- droplevels(subset(allAHW, station_name == levels(as.factor(AHW$station_name))[j]))
      ahw1$lon <- deg2rad(ahw1$lon); ahw1$lat <- deg2rad(ahw1$lat)
      mhw2 <- mhw1[mhw1$year >= min(ahw1$year), ]
      mhw2 <- mhw2[mhw2$year <= max(ahw1$year), ]
      ahw2 <- ahw1[ahw1$year >= min(mhw2$year), ]
      ahw2 <- ahw2[ahw2$year <= max(mhw2$year), ]
      if(length(ahw2$station_name) < 1){
        overlap <- 0
        overall_years <- 0
        overall_frequency <- 0
        overall_duration <- 0
        overall_intensity <- 0
      } else {
        overlap <- length(ahw2$station_name)
        overall_years <- 0
        for(k in 1:length(ahw2$station_name)){
          if(mhw2$frequency[k] > 0 & ahw2$frequency[k] > 0){
            overall_years <- overall_years + 1
          }
        }
        overall_frequency <- round(sum(mhw2$frequency, na.rm = T)/sum(ahw2$frequency, na.rm = T),2)
        overall_duration <- round(sum(mhw2$duration, na.rm = T)/sum(ahw2$duration, na.rm = T),2)
        overall_intensity <- round(sum(mhw2$intensity, na.rm = T)/sum(ahw2$intensity, na.rm = T),2)
      }
      distance <- round(gcd.hf(mhw1$lon[1], mhw1$lat[1], ahw1$lon[1], ahw1$lat[1]), 2)
      x <- data.frame(site = mhw1$site[1], station = ahw1$station_name[1],
                      overlap, overall_years, overall_frequency, overall_duration,
                      overall_intensity, distance)
      overall_cooccurrence <- rbind(overall_cooccurrence, x)
    }
  }
  overall_cooccurrence[is.na(overall_cooccurrence)] <- NA
  is.na(overall_cooccurrence) <- sapply(overall_cooccurrence, is.infinite)
  return(overall_cooccurrence)
}


# MHW
mhwCO <- cooccurrence(mhwAnnual, allAHW)
write.csv(mhwCO, "data/mhwCO.csv", row.names = F)
mhwCO <- read.csv("data/mhwCO.csv")
# MCS
mcsCO <- cooccurrence(mcsAnnual, allACW)
write.csv(mcsCO, "data/mcsCO.csv", row.names = F)
mcsCO <- read.csv("data/mcsCO.csv")


# 6. Visualise results ----------------------------------------------------

mhwCO$type <- "HW"
mcsCO$type <- "CS"

mhwCOlong <- melt(mhwCO, id.vars = c("site", "station", "type", "overlap", "distance"))
mcsCOlong <- melt(mcsCO, id.vars = c("site", "station", "type", "overlap", "distance"))

allCOlong <- rbind(mhwCOlong, mcsCOlong)

allCOlong[!(allCOlong$variable == "overall_years"),] %>% 
  ggplot(aes(x = distance, y = value)) + bw_update +
  geom_point(aes(colour = variable, size = overlap, shape = type), alpha = 0.6) +
  geom_smooth(method = "glm", aes(colour = variable))
ggsave("graph/change_in_variables_over_distance.pdf")

allCOlong[!(allCOlong$variable == "overall_years") & allCOlong$distance < 100,] %>%
  ggplot(aes(x = distance, y = value)) + bw_update +
  geom_point(aes(colour = variable, size = overlap, shape = type), alpha = 0.6) +
  geom_smooth(method = "glm", aes(colour = variable))
ggsave("graph/change_in_variables_over_distance_100km.pdf")

## Closest two
# The extracting function
closest.n <- function(dat, n, overlap){
  result <- data.frame()
  for(i in 1:length(levels(as.factor(dat$site)))){
    dat2 <- droplevels(subset(dat, site == levels(as.factor(dat$site))[i]))
    dat3 <- arrange(dat2, distance)
    dat4 <- dat3[!(dat3$overlap < overlap),]
    dat4 <- head(dat4, n)
    result <- rbind(result, dat4)
  }
  return(result)
}


allCOclose <- closest.n(rbind(mhwCO, mcsCO), 2, 5)

allCOcloseLong <- melt(allCOclose, id.vars = c("site", "station", "type", "overlap", "distance"))

allCOcloseLong[!(allCOcloseLong$variable == "overall_years"),] %>% 
  ggplot(aes(x = distance, y = value)) + bw_update +
  geom_point(aes(colour = variable, size = overlap, shape = type), alpha = 0.6) +
  geom_smooth(method = "glm", aes(colour = variable))
ggsave("graph/change_in_variables_over_distance_nearest_2.pdf")
