#############################################################################
###"proc/exploration.R"
## This script does:
# 1. Load co-occurrence results
# 2. Load OISST and in situ data from outside of this project
# 3. Create extreme event figures
# 4. Subset only events within a 7 day co-occurrence
# 5. Subset only events within a 2 day co-occurrence
# 6. Exploration
# 7. Line graphs for strong co-occurrence
# 8. Wind calculations
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(stringr)
library(zoo)
library(lubridate)
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(broom)
library(tibble)
library(purrr)
library(RmarineHeatWaves)
library(readr)
source("setupParams/theme.R")
## USED BY:
# "graph/figures.R"
## CREATES:
# All of the figures found in: "~/AHW/graph/single_event/*.pdf"
#############################################################################


# 1. Load co-occurrence results -------------------------------------------

# 1) SACTN - SACTN
# Heat waves
load("data/cooccurrence/SACTN_SACTN_hw_tmean_CO.Rdata")
# Cold-spells
load("data/cooccurrence/SACTN_SACTN_cs_tmean_CO.Rdata")


# 2) SAWS - SAWS
# Heat waves
load("data/cooccurrence/SAWS_SAWS_hw_tmean_CO.Rdata")
load("data/cooccurrence/SAWS_SAWS_hw_tmax_CO.Rdata")
load("data/cooccurrence/SAWS_SAWS_hw_tmin_CO.Rdata")
# Cold-spells
load("data/cooccurrence/SAWS_SAWS_cs_tmean_CO.Rdata")
load("data/cooccurrence/SAWS_SAWS_cs_tmax_CO.Rdata")
load("data/cooccurrence/SAWS_SAWS_cs_tmin_CO.Rdata")

# 3) SACTN - SAWS
# Heat waves
load("data/cooccurrence/SACTN_SAWS_hw_tmean_CO.Rdata")
load("data/cooccurrence/SACTN_SAWS_hw_tmax_CO.Rdata")
load("data/cooccurrence/SACTN_SAWS_hw_tmin_CO.Rdata")
# Cold-spells
load("data/cooccurrence/SACTN_SAWS_cs_tmean_CO.Rdata")
load("data/cooccurrence/SACTN_SAWS_cs_tmax_CO.Rdata")
load("data/cooccurrence/SACTN_SAWS_cs_tmin_CO.Rdata")


# 2. Load OISST and in situ data from outside of this project -------------

# OISST
load("~/R/forOthers/AJ/sst1.Rdata")

# in situ
load("~/SACTNraw/data/SACTNdaily_v4.1.Rdata")
load("~/AHW/data/SACTN/SACTN_cropped.Rdata")
SACTN_cropped <- SACTN_cropped[,c(1,4,5)]

# SAWS
load("~/AHW/data/SAWS/homogenised/SAWS_homogenised.Rdata")
SAWS_tmean <- SAWS_homogenised[,c(1,2,4)]
colnames(SAWS_tmean)[3] <- "temp"


# 3. Create extreme event figures -----------------------------------------

# Process and combine SAWS and SACTN
SAWS_tmean$type <- "a" # "a" for atmosphere
SACTN_cropped$type <- "m" # "m" for marine
all_data <- rbind(SAWS_tmean, SACTN_cropped)

# df <- all_data[all_data$site == "Betty's Bay",] # Tester...
largest.int.cum <- function(df){
  colnames(df) <- c("site", "t", "temp", "type")
  site <- droplevels(df$site[1])
  df$site <- NULL
  type <- df$type[1]
  df$type <- NULL
  df2 <- make_whole(df)
  if(type == "m"){
    start <- year(df2$date[1])+1
    end <- year(df2$date[nrow(df2)])-1
  } else if(type == "a"){
    start <- 1981
    end <- 2010
  }
  hw <- detect(df2, climatology_start = start, climatology_end = end,
                min_duration = 3, max_gap = 0, cold_spells = FALSE)
  event_line(hw, start_date = df2$date[1], end_date = df2$date[nrow(df2)])
  ggsave(paste("~/AHW/graph/single_event/", site, "_hw_int_cum.pdf", sep = ""), width = 12)
  cs <- detect(df2, climatology_start = start, climatology_end = end,
               min_duration = 3, max_gap = 0, cold_spells = TRUE)
  event_line(cs, start_date = df2$date[1], end_date = df2$date[nrow(df2)])
  ggsave(paste("~/AHW/graph/single_event/", site, "_cs_int_cum.pdf", sep = ""), width = 12)
}

# ddply(all_data, .(site), largest.int.cum) # Don't run this unless you want to re-create ALL of the figures


# 4. Subset only events within a 7 day co-occurrence ----------------------

# 1) SACTN - SACTN
# Heat waves
SACTN_SACTN_hw_tmean_CO_7_day <- SACTN_SACTN_hw_tmean_CO[abs(SACTN_SACTN_hw_tmean_CO$latest) <= 7,]
# Cold-spells
SACTN_SACTN_cs_tmean_CO_7_day <- SACTN_SACTN_cs_tmean_CO[abs(SACTN_SACTN_cs_tmean_CO$latest) <= 7,]

# 2) SAWS - SAWS
# Heat waves
SAWS_SAWS_hw_tmean_CO_7_day <- SAWS_SAWS_hw_tmean_CO[abs(SAWS_SAWS_hw_tmean_CO$latest) <= 7,]
SAWS_SAWS_hw_tmax_CO_7_day <- SAWS_SAWS_hw_tmax_CO[abs(SAWS_SAWS_hw_tmax_CO$latest) <= 7,]
SAWS_SAWS_hw_tmin_CO_7_day <- SAWS_SAWS_hw_tmin_CO[abs(SAWS_SAWS_hw_tmin_CO$latest) <= 7,]
# Cold-spells
SAWS_SAWS_cs_tmean_CO_7_day <- SAWS_SAWS_cs_tmean_CO[abs(SAWS_SAWS_cs_tmean_CO$latest) <= 7,]
SAWS_SAWS_cs_tmax_CO_7_day <- SAWS_SAWS_cs_tmax_CO[abs(SAWS_SAWS_cs_tmax_CO$latest) <= 7,]
SAWS_SAWS_cs_tmin_CO_7_day <- SAWS_SAWS_cs_tmin_CO[abs(SAWS_SAWS_cs_tmin_CO$latest) <= 7,]

# 3) SACTN - SAWS
# Heat waves
SACTN_SAWS_hw_tmean_CO_7_day <- SACTN_SAWS_hw_tmean_CO[abs(SACTN_SAWS_hw_tmean_CO$latest) <= 7,]
SACTN_SAWS_hw_tmax_CO_7_day <- SACTN_SAWS_hw_tmax_CO[abs(SACTN_SAWS_hw_tmax_CO$latest) <= 7,]
SACTN_SAWS_hw_tmin_CO_7_day <- SACTN_SAWS_hw_tmin_CO[abs(SACTN_SAWS_hw_tmin_CO$latest) <= 7,]
# Cold-spells
SACTN_SAWS_cs_tmean_CO_7_day <- SACTN_SAWS_cs_tmean_CO[abs(SACTN_SAWS_cs_tmean_CO$latest) <= 7,]
SACTN_SAWS_cs_tmax_CO_7_day <- SACTN_SAWS_cs_tmax_CO[abs(SACTN_SAWS_cs_tmax_CO$latest) <= 7,]
SACTN_SAWS_cs_tmin_CO_7_day <- SACTN_SAWS_cs_tmin_CO[abs(SACTN_SAWS_cs_tmin_CO$latest) <= 7,]


# 5. Subset only events within a 2 day co-occurrence ----------------------

# 1) SACTN - SACTN
# Heat waves
SACTN_SACTN_hw_tmean_CO_2_day <- SACTN_SACTN_hw_tmean_CO[abs(SACTN_SACTN_hw_tmean_CO$latest) <= 2,]
# Cold-spells
SACTN_SACTN_cs_tmean_CO_2_day <- SACTN_SACTN_cs_tmean_CO[abs(SACTN_SACTN_cs_tmean_CO$latest) <= 2,]

# 2) SAWS - SAWS
# Heat waves
SAWS_SAWS_hw_tmean_CO_2_day <- SAWS_SAWS_hw_tmean_CO[abs(SAWS_SAWS_hw_tmean_CO$latest) <= 2,]
SAWS_SAWS_hw_tmax_CO_2_day <- SAWS_SAWS_hw_tmax_CO[abs(SAWS_SAWS_hw_tmax_CO$latest) <= 2,]
SAWS_SAWS_hw_tmin_CO_2_day <- SAWS_SAWS_hw_tmin_CO[abs(SAWS_SAWS_hw_tmin_CO$latest) <= 2,]
# Cold-spells
SAWS_SAWS_cs_tmean_CO_2_day <- SAWS_SAWS_cs_tmean_CO[abs(SAWS_SAWS_cs_tmean_CO$latest) <= 2,]
SAWS_SAWS_cs_tmax_CO_2_day <- SAWS_SAWS_cs_tmax_CO[abs(SAWS_SAWS_cs_tmax_CO$latest) <= 2,]
SAWS_SAWS_cs_tmin_CO_2_day <- SAWS_SAWS_cs_tmin_CO[abs(SAWS_SAWS_cs_tmin_CO$latest) <= 2,]

# 3) SACTN - SAWS
# Heat waves
SACTN_SAWS_hw_tmean_CO_2_day <- SACTN_SAWS_hw_tmean_CO[abs(SACTN_SAWS_hw_tmean_CO$latest) <= 2,]
SACTN_SAWS_hw_tmax_CO_2_day <- SACTN_SAWS_hw_tmax_CO[abs(SACTN_SAWS_hw_tmax_CO$latest) <= 2,]
SACTN_SAWS_hw_tmin_CO_2_day <- SACTN_SAWS_hw_tmin_CO[abs(SACTN_SAWS_hw_tmin_CO$latest) <= 2,]
# Cold-spells
SACTN_SAWS_cs_tmean_CO_2_day <- SACTN_SAWS_cs_tmean_CO[abs(SACTN_SAWS_cs_tmean_CO$latest) <= 2,]
SACTN_SAWS_cs_tmax_CO_2_day <- SACTN_SAWS_cs_tmax_CO[abs(SACTN_SAWS_cs_tmax_CO$latest) <= 2,]
SACTN_SAWS_cs_tmin_CO_2_day <- SACTN_SAWS_cs_tmin_CO[abs(SACTN_SAWS_cs_tmin_CO$latest) <= 2,]


# 6. Exploration ----------------------------------------------------------

# The count of closely co-occurring events in relation to the percentile of their size
nrow(SACTN_SAWS_hw_tmean_CO_2_day) # 1329

nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile == 0,]) # 193
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.1 == 0,]) # 147
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.idx == 0,]) # 691

nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile == 25,]) # 292
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.1 == 25,]) # 278
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.idx == 25,]) # 382

nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile == 50,]) # 342
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.1 == 50,]) # 428
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.idx == 50,]) # 190

nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile == 75,]) # 433
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.1 == 75,]) # 423
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.idx == 75,]) # 63

nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile == 100,]) # 69
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.1 == 100,]) # 53
nrow(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.idx == 100,]) # 3
# It appears that there is definitely a relationship between these closely co-occurring events
# I reached this conclusion based on the fact that the largest number of closely occurring events are in the 75th percentile

top_1_2 <- droplevels(SACTN_SAWS_hw_tmean_CO_2_day[SACTN_SAWS_hw_tmean_CO_2_day$percentile.idx == 100,]) # 3
top_1_7 <- droplevels(SACTN_SAWS_hw_tmean_CO_7_day[SACTN_SAWS_hw_tmean_CO_7_day$percentile.idx == 100,]) # 8

nrow(SACTN_SAWS_hw_tmean_CO) # 40179
nrow(SACTN_SAWS_hw_tmean_CO[SACTN_SAWS_hw_tmean_CO$latest <= 7 & SACTN_SAWS_hw_tmean_CO$latest >= 0,]) #1986
nrow(SACTN_SAWS_hw_tmean_CO[SACTN_SAWS_hw_tmean_CO$latest >= -7 & SACTN_SAWS_hw_tmean_CO$latest <= 0,]) #1659

## Create figures to show these results more clearly ##

# 7. Line graphs for strong co-occurrence ---------------------------------

# The sites with the most extreme heat waves
SACTN_sites <- c("Bordjies", "Hermanus", "Knysna", "Storms River Mouth")
SAWS_sites <- c("Cape Agulhas", "Cape St Blaize")

# The datasets
SACTN_top <- droplevels(subset(SACTN_cropped, site %in% SACTN_sites))
SAWS_top <- droplevels(subset(SAWS_tmean, site %in% SAWS_sites))
all_top <- rbind(SACTN_top, SAWS_top)
all_top <- all_top[all_top$date >= as.Date("2004-10-01") & all_top$date <= as.Date("2005-01-01"),]

ggplot(data = all_top, aes(x = date, y = temp)) + bw_update +
  geom_line(aes(colour = site, linetype = type), alpha = 0.7) +
  geom_vline(data = top_1_7[1:6,], aes(xintercept = as.numeric(date_start), colour = site), linetype = "solid", alpha = 0.7) +
  geom_vline(data = top_1_7[1:6,], aes(xintercept = as.numeric(date_stop), colour = site), linetype = "solid", alpha = 0.7) +
  geom_vline(data = top_1_7[1:6,], aes(xintercept = as.numeric(date_start.1), colour = site.1), linetype = "dashed", alpha = 0.7) +
  geom_vline(data = top_1_7[1:6,], aes(xintercept = as.numeric(date_stop.1), colour = site.1), linetype = "dashed", alpha = 0.7)
ggsave("~/Desktop/Nov_2004.jpg", width = 10, height = 6)


# 8. Wind calculations ----------------------------------------------------

source("~/AHW/func/earthdist.R")

# Load data
wind <- read_csv("~/AHW/data/wind/port nolloth 1.csv", 
                 col_names = c("century", "year", "month", "day", "hour", "speed", "bearing"),
                 col_types = "iciiidi")
# levels(as.factor(wind$year))
# wind$year[wind$year == 0] <- 000
wind$date <- as.Date(paste(paste(wind$century, wind$year, sep = ""), wind$month, wind$day, sep = "-"))
wind <- wind[,c(8,6,7)]
wind <- wind[complete.cases(wind),]
# wind$speed[as.character(wind$speed) == "NaN"] <- NA

# The function used to create mean wind vectors
wind.vector <- function(df){
  # This calculation runs better when 360 = 0
  # df$bearing[df$bearing == 360] <- 0
  # Then convert decimal degrees to radians
  # df$bearing <- deg2rad(df$bearing)
  # Calculate east-west and north-south components
  # df <- filter(wind, date == "2000-01-04")
  # df$bearing[1] <- 160
  # df$bearing <- df$bearing-180
  ve <- -sum(df$speed*sin(df$bearing * pi/180))/nrow(df)
  vn <- -sum(df$speed*cos(df$bearing * pi/180))/nrow(df)
  # Mean wind speed
  u <- round_any((ve^2 + vn^2)^(1/2), 0.01)
  # Mean direction
  theta <- atan2(ve, vn) *180/pi
  theta2 <- round_any(theta + 180, 0.01)
  # theta <- atan2(vn, ve)
  # Convert back to decimal degrees
  # theta2 <- theta/pi*180
  # Finally adjust the result accordingly
  # if(!(is.na(theta))){
  #   if(theta2 == 0){
  #     theta3 <- 0
  #   } else if(theta2 < 180 & theta2 > -0.01) {
  #     theta3 <- round_any(theta2+180, 0.01)
  #   } else if(theta2 > 180) {
  #     theta3 <- round_any(theta2-180, 0.01)
  #   } else if(theta2 < -0.01) {
  #     theta3 <- round_any(theta2+360, 0.01)
  #   }
  # } else {
  #   theta3 <- NA
  # }
  # x <- ve/pi*180
  # y <- vn/pi*180
  # Combine results into a new dataframe
  df2 <- data.frame(date = df$date[1], speed = u, bearing = theta2)
  return(df2)
}

# Testing
test1 <- filter(wind, date == "1996-01-01") #180
test1 <- filter(wind, date == "1996-01-04") #~180
test1 <- filter(wind, date == "2000-01-04") #340
test1$bearing[1] <- 160
df <- test1
test2 <- wind.vector(test1)

wind2 <- ddply(wind, .(date), wind.vector, .parallel = T)

# Fill gaps in daily values for plotting purposes
wind3 <- wind2[order(wind2$date),]
grid.df <- data.frame(date = seq(min(wind3$date, na.rm = T), max(wind3$date, na.rm = T), by = "day"))
wind3 <- merge(wind3, grid.df, by = "date", all.y = T)

wind_PN <- data.frame(site = "Port Nolloth", wind3)
save(wind_PN, file = "data/wind/wind_PN.Rdata")
