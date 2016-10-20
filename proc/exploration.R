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

ddply(all_data, .(site), largest.int.cum)


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
