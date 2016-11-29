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
# 9. Ideas from literature
# 10. Significant differences between events and CO-events
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
  # THis bit is blocked out as running it saves a moese lot of figures
  # hw <- detect(df2, climatology_start = start, climatology_end = end,
  #               min_duration = 3, max_gap = 0, cold_spells = FALSE)
  # event_line(hw, start_date = df2$date[1], end_date = df2$date[nrow(df2)])
  # ggsave(paste("~/AHW/graph/single_event/", site, "_hw_int_cum.pdf", sep = ""), width = 12)
  # cs <- detect(df2, climatology_start = start, climatology_end = end,
  #              min_duration = 3, max_gap = 0, cold_spells = TRUE)
  # event_line(cs, start_date = df2$date[1], end_date = df2$date[nrow(df2)])
  # ggsave(paste("~/AHW/graph/single_event/", site, "_cs_int_cum.pdf", sep = ""), width = 12)
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

nrow(SACTN_SAWS_hw_tmean_CO) # 40014
nrow(SACTN_SAWS_hw_tmean_CO[SACTN_SAWS_hw_tmean_CO$latest <= 7 & SACTN_SAWS_hw_tmean_CO$latest >= 0,]) #2014
nrow(SACTN_SAWS_hw_tmean_CO[SACTN_SAWS_hw_tmean_CO$latest >= -7 & SACTN_SAWS_hw_tmean_CO$latest <= 0,]) #1630

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
  # Calculate east-west and north-south components
  ve <- -sum(df$speed*sin(df$bearing * pi/180))/nrow(df)
  vn <- -sum(df$speed*cos(df$bearing * pi/180))/nrow(df)
  # Mean wind speed
  u <- round_any((ve^2 + vn^2)^(1/2), 0.01)
  # Mean direction
  theta <- atan2(ve, vn) * 180/pi
  theta2 <- round_any(theta + 180, 0.01)
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


# 9. Ideas from literature ------------------------------------------------

# http://people.earth.yale.edu/sites/default/files/files/Fedorov/49_Fedorov_Coupling_Oxford_2007.pdf
# SST anomalies force atmospheric temperatures in the tropics
# Which may then force feedback loops
# Coupling is stronger closer to the equator

# In mid-latitudes ocean-atmosphere coupling is debated
# Likely it is atmospheric anomalies that drive SST

# a) How does atmosphere respond to SST anomalies?
# b) What is the SST response to wind stress anomalies?
# c) Does the wind stress amplify the SST anomaly?

# Generally the heatflux from atmoshpere to ocean takes ~3 months

# Heat flux between the two is proportional to the difference

# Atmosphere forced SST anomalies can persist for months to years

# SST anomolies may drive wind stress

# Wind stress may play a larger role in temperature anomalies than air-sea interaction

# SST anomalies lead geospatial height anomalies by two months

# Tropical tmospheric anomalies generally lead to subsurface flows/ upwelling

# Mid-latitude anomalies generally lead to gyre circulation change

## Other ideas

# Look first for relationships between wind and atmosphere only

# Also wind and SST only


# 10. Significant differences between events and CO-events ----------------

# Load index of closest occurring sites
load("setupParams/SACTN_SAWS_nearest.Rdata")
SACTN_SAWS_nearest$index <- seq(1:nrow(SACTN_SAWS_nearest))

# Load event results
# Marine
load("data/events/SACTN_events.Rdata")
# Atmosphere
load("data/events/SAWS_SACTN_events_tmax.Rdata")
load("data/events/SAWS_SACTN_events_tmean.Rdata")
load("data/events/SAWS_SACTN_events_tmin.Rdata")

# Add more useful SACTN index column to the SAWS results
SAWS_SACTN_events_tmean$SACTN <- sapply(strsplit(as.character(SAWS_SACTN_events_tmean$index), " - "), "[[", 2)

# Create function that correctly extracts events and CO-events for given sites
## Only running for tmean currently
# df <- SACTN_SAWS_nearest[20,] ## For testing ##
event.sig <- function(df){
  
  # Currently not running analysis on cold-spells or 2 day lag window
  
  ## Subset events
  MHW_sub <- filter(SACTN_events, site == df$SACTN[1] & type == "MHW")
  MHW_sub$rate_decline[MHW_sub$rate_decline == Inf] <- NA
  # MCS_sub <- filter(SACTN_events, site == df$SACTN[1] & type == "MCS")
  AHW_tmean_sub <- filter(SAWS_SACTN_events_tmean, 
                          site == df$SAWS[1] & SACTN == df$SACTN[1] & type == "AHW")
  AHW_tmean_sub$rate_decline[AHW_tmean_sub$rate_decline == Inf] <- NA
  # ACS_tmean_sub <- filter(SAWS_SACTN_events_tmean, 
  #                         site == df$SAWS[1] & SACTN == df$SACTN[1] & type == "ACS")
  
  ## Subset CO-events
  # 7 day lag
  HW_tmean_CO_7_sub <- filter(SACTN_SAWS_hw_tmean_CO_7_day, site == df$SAWS[1] & site.1 == df$SACTN[1])
  # CS_tmean_CO_7_sub <- filter(SACTN_SAWS_cs_tmean_CO_7_day, site == df$SAWS[1] & site.1 == df$SACTN[1])
  # 2 day lag
  # HW_tmean_CO_2_sub <- filter(SACTN_SAWS_hw_tmean_CO_2_day, site == df$SAWS[1] & site.1 == df$SACTN[1])
  # CS_tmean_CO_2_sub <- filter(SACTN_SAWS_cs_tmean_CO_2_day, site == df$SAWS[1] & site.1 == df$SACTN[1])
  
  ## Remove duplicate CO-events from data frames
  # Use SAWS event numbers as they occasionally have 2 syncing with 1 SACTN event and so are more voluminous
  # The problem with this step is it removes the percentile.idx value
  # Though for now this specific analysis is not using that information
  # A future one perhaps would benefit from looking specifically at events within specific percentile.idx
  # The percentile values of the events still remain within the data frame so this can still be done as is
  # 7 day lag
  HW_tmean_CO_7_sub <- HW_tmean_CO_7_sub[1:length(unique(HW_tmean_CO_7_sub$event_no)),]
  HW_tmean_CO_7_sub$rate_decline[HW_tmean_CO_7_sub$rate_decline == Inf] <- NA
  HW_tmean_CO_7_sub$rate_decline.1[HW_tmean_CO_7_sub$rate_decline.1 == Inf] <- NA
  # CS_tmean_CO_7_sub <- CS_tmean_CO_7_sub[1:length(unique(CS_tmean_CO_7_sub$event_no)),]
  # 2 day lag
  # HW_tmean_CO_2_sub <- HW_tmean_CO_2_sub[1:length(unique(HW_tmean_CO_2_sub$event_no)),]
  # CS_tmean_CO_2_sub <- CS_tmean_CO_2_sub[1:length(unique(CS_tmean_CO_2_sub$event_no)),]
  
  ## Remove duplicate events and events with co-occurring partners from main data frames
  # Marine
  MHW_sub_sub <- MHW_sub[!(MHW_sub$event_no %in% HW_tmean_CO_7_sub$event_no.1),]
  # MCS_sub_sub <- MCS_sub[!(MCS_sub$event_no %in% CS_tmean_CO_7_sub$event_no.1),]
  # Atmosphere
  AHW_tmean_sub_sub <- AHW_tmean_sub[!(AHW_tmean_sub$event_no %in% HW_tmean_CO_7_sub$event_no),]
  # ACS_tmean_sub_sub <- ACS_tmean_sub[!(ACS_tmean_sub$event_no %in% CS_tmean_CO_7_sub$event_no),]
  
  ## 1) T-tests ##
  
  # 1.1) MHW vs. AHW
  MHW_vs_AHW_t.test <- data.frame(
    comparison = "MHW vs AHW",
    rate_decline = 
      t.test(MHW_sub$rate_decline, AHW_tmean_sub$rate_decline, 
             alternative = "two.sided", paired = F)$p.value,
    int_mean_abs =
      t.test(MHW_sub$int_mean_abs, AHW_tmean_sub$int_mean_abs, 
             alternative = "two.sided", paired = F)$p.value,
    int_max_abs =
      t.test(MHW_sub$int_max_abs, AHW_tmean_sub$int_max_abs, 
             alternative = "two.sided", paired = F)$p.value,
    int_cum_abs = 
      t.test(MHW_sub$int_cum_abs, AHW_tmean_sub$int_cum_abs, 
             alternative = "two.sided", paired = F)$p.value,
    duration =
      t.test(MHW_sub$duration, AHW_tmean_sub$duration, 
             alternative = "two.sided", paired = F)$p.value
  )
  MHW_vs_AHW_t.test[,2:length(MHW_vs_AHW_t.test)] <- apply(MHW_vs_AHW_t.test[,2:length(MHW_vs_AHW_t.test)], 
                                                           2, round_any, 0.0001)
  
  # 1.2) MHW vs. CO-MHW
  if(nrow(HW_tmean_CO_7_sub[complete.cases(HW_tmean_CO_7_sub$rate_decline.1),]) <= 2){
    MHW_vs_COMHW_t.test <- data.frame(
      comparison = "MHW vs CO-MHW",
      rate_decline = NA,
      int_mean_abs = NA,
      int_max_abs = NA,
      int_cum_abs = NA,
      duration = NA
    )
  } else {
    MHW_vs_COMHW_t.test <- data.frame(
      comparison = "MHW vs CO-MHW",
      rate_decline = 
        t.test(MHW_sub_sub$rate_decline, HW_tmean_CO_7_sub$rate_decline.1, 
               alternative = "two.sided", paired = F)$p.value,
      int_mean_abs =
        t.test(MHW_sub_sub$int_mean_abs, HW_tmean_CO_7_sub$int_mean_abs.1, 
               alternative = "two.sided", paired = F)$p.value,
      int_max_abs =
        t.test(MHW_sub_sub$int_max_abs, HW_tmean_CO_7_sub$int_max_abs.1, 
               alternative = "two.sided", paired = F)$p.value,
      int_cum_abs = 
        t.test(MHW_sub_sub$int_cum_abs, HW_tmean_CO_7_sub$int_cum_abs.1, 
               alternative = "two.sided", paired = F)$p.value,
      duration =
        t.test(MHW_sub_sub$duration, HW_tmean_CO_7_sub$duration.1, 
               alternative = "two.sided", paired = F)$p.value
    )
    MHW_vs_COMHW_t.test[,2:length(MHW_vs_COMHW_t.test)] <- apply(MHW_vs_COMHW_t.test[,2:length(MHW_vs_COMHW_t.test)], 
                                                                 2, round_any, 0.0001)
  }
  
  # 1.3) AHW vs. CO-AHW
  if(nrow(HW_tmean_CO_7_sub[complete.cases(HW_tmean_CO_7_sub$rate_decline),]) <= 2){
    AHW_vs_COAHW_t.test <- data.frame(
      comparison = "AHW vs CO-AHW",
      rate_decline = NA,
      int_mean_abs = NA,
      int_max_abs = NA,
      int_cum_abs = NA,
      duration = NA
    )
  } else {
    AHW_vs_COAHW_t.test <- data.frame(
      comparison = "AHW vs CO-AHW",
      rate_decline = 
        t.test(AHW_tmean_sub_sub$rate_decline, HW_tmean_CO_7_sub$rate_decline, 
               alternative = "two.sided", paired = F)$p.value,
      int_mean_abs =
        t.test(AHW_tmean_sub_sub$int_mean_abs, HW_tmean_CO_7_sub$int_mean_abs, 
               alternative = "two.sided", paired = F)$p.value,
      int_max_abs =
        t.test(AHW_tmean_sub_sub$int_max_abs, HW_tmean_CO_7_sub$int_max_abs, 
               alternative = "two.sided", paired = F)$p.value,
      int_cum_abs = 
        t.test(AHW_tmean_sub_sub$int_cum_abs, HW_tmean_CO_7_sub$int_cum_abs, 
               alternative = "two.sided", paired = F)$p.value,
      duration =
        t.test(AHW_tmean_sub_sub$duration, HW_tmean_CO_7_sub$duration, 
               alternative = "two.sided", paired = F)$p.value
    )
    AHW_vs_COAHW_t.test[,2:length(AHW_vs_COAHW_t.test)] <- apply(AHW_vs_COAHW_t.test[,2:length(AHW_vs_COAHW_t.test)], 
                                                                 2, round_any, 0.0001)
  }
  
  # Stitch it up
  ALL_t.test <- rbind(MHW_vs_AHW_t.test, MHW_vs_COMHW_t.test, AHW_vs_COAHW_t.test)
  
  # Add meta-data
  ALL_t.test$SACTN <- df$SACTN[1]
  ALL_t.test$SAWS <- df$SAWS[1]
  ALL_t.test$distance <- df$distance[1]
  
  # Melt it
  ALL_t.test <- melt(ALL_t.test,
                     id.vars = c("SACTN", "SAWS", "distance","comparison"),
                     variable.name = "stat",
                     value.name = "p")
  
  
  ## 2) ANOVA ##
  # Not running ANOVA at the moment as, while pairwise t.tests aren't the best in terms of sound statistics
  # I believe they are a better tool for answering the question at hand:
  # "Are co-occurring events significantly different from all of the events at a given time series?"
  # An ANOVA looking at significance BETWEEN groups does not answer this as specifically as paired t.tests
  
  return(ALL_t.test)
  
}

# Run it
event_sig <- ddply(SACTN_SAWS_nearest, .(index), event.sig, .parallel = T)

# Prep for plotting
event_sig$index <- paste(event_sig$SACTN, event_sig$SAWS, sep = " - ")
event_sig$index <- as.factor(event_sig$index)
index_guide <- data.frame(index = paste(SACTN_SAWS_nearest$SACTN, SACTN_SAWS_nearest$SAWS, sep = " - "),
                          order = seq(1:nrow(SACTN_SAWS_nearest)))
index_guide$index <- reorder(index_guide$index, index_guide$order)
event_sig <- event_sig %>%
  group_by(index) %>%
  mutate(order = index_guide$order[index_guide$index == index][1])
event_sig <- data.frame(event_sig)
event_sig <- event_sig[order(event_sig$order),]
event_sig$index <- reorder(event_sig$index, event_sig$order)
event_sig$order <- NULL
rownames(event_sig) <- NULL

## Create beefy figures
# Boxplot ## Not run as it looks like garbage
# bp <- ggplot(data = event_sig, aes(x = stat, y = p, fill = comparison)) + bw_update +
#   geom_boxplot() +
#   facet_wrap(~index, ncol = 7) +
#   theme(axis.text.x = element_text(angle = 90))
# ggsave("graph/ALL_t.test_boxplot.pdf", width = 12, height = 6)
# Points
p <- ggplot(data = event_sig, aes(x = stat, y = p)) + bw_update +
  geom_point(aes(colour = comparison)) +
  geom_point(data = event_sig[!(is.na(event_sig$p)) & event_sig$p <= 0.05,], shape = 4, size = 1.5) +
  facet_wrap(~index, ncol = 7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3))
ggsave("graph/ALL_t.test_point.pdf", width = 10, height = 6)
