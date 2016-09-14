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

load("data/SAWS/SAWS_data.Rdata")


# Combine specific time series --------------------------------------------
## NB: The word document containing the reasoning for these changes may be found at "setupParams/combine_time_series.docx"

# Graph of all clean data
ggplot(data = SAWS_data_clean, aes(x = date)) + bw_update +
  geom_line(aes(y = mx), colour = "Red") +
  geom_line(aes(y = mn), colour = "Blue") +
  facet_wrap(~station)
ggsave("graph/SAWS_data_old.pdf", width = 12, height = 12)

# Combine time series by changing site names
levels(SAWS_data_clean$station)
SAWS_data_clean$station <- as.character(SAWS_data_clean$station)

SAWS_data_clean$station[SAWS_data_clean$station == "CAPE TOWN D.F.MALAN - WK"] <- "CAPE TOWN WO"
SAWS_data_clean$station[SAWS_data_clean$station == "EAST LONDON WO AWS"] <- "EAST LONDON AWS"
SAWS_data_clean$station[SAWS_data_clean$station == "EAST LONDON - WK"] <- "EAST LONDON WO"
SAWS_data_clean$station[SAWS_data_clean$station == "GEORGE CLIMATE AWS"] <- "GEORGE AWS"
SAWS_data_clean$station[SAWS_data_clean$station == "JOUBERTINA SCHOOL AWS"] <- "JOUBERTINA AWS"
SAWS_data_clean$station[SAWS_data_clean$station == "PORT ELIZABETH - WK"] <- "PORT ELIZABETH AWOS"
SAWS_data_clean$station[SAWS_data_clean$station == "PORT NOLLOTH - VRT"] <- "PORT NOLLOTH"
SAWS_data_clean$station[SAWS_data_clean$station == "SPRINGBOK - WK"] <- "SPRINGBOK WO"
SAWS_data_clean$station[SAWS_data_clean$station == "VIOOLSDRIF - AWS"] <- "VIOOLSDRIF"

SAWS_data_clean$station <- as.factor(SAWS_data_clean$station)
levels(SAWS_data_clean$station)

# Smooth any duplicates and lat/ lon
# SAWS_data_clean <- ddply(SAWS_data_clean, .(ID, station, date, lat, lon), summarise,
#                          mx = round_any(mean(mx, na.rm = T),0.1),
#                          mn = round_any(mean(mn, na.rm = T),0.1),
#                          .progress = "text")
# 443759 rows to 443759
# No overlap

SAWS_data_clean <- SAWS_data_clean %>% 
  group_by(station) %>% 
  mutate(lat = round_any(mean(lat), 0.0001)) %>% 
  mutate(lon = round_any(mean(lon), 0.0001))
SAWS_data_clean <- data.frame(SAWS_data_clean)

length(levels(SAWS_data_clean$station))
length(levels(as.factor(SAWS_data_clean$lat)))
length(levels(as.factor(SAWS_data_clean$lon)))

# Graph of new data
ggplot(data = SAWS_data_clean, aes(x = date)) + bw_update +
  geom_line(aes(y = mx), colour = "Red") +
  geom_line(aes(y = mn), colour = "Blue") +
  facet_wrap(~station)
ggsave("graph/SAWS_data.pdf", width = 12, height = 12)


# Save clean data ---------------------------------------------------------

save(SAWS_data_clean, file = "data/SAWS/SAWS_data_clean.Rdata")

SAWS_site_list <- unique(SAWS_data_clean[2:4])
write.csv(SAWS_site_list, "setupParams/SAWS_site_list_clean.csv", row.names = F)


# Crop data based on length and NA ----------------------------------------

load("data/SAWS/SAWS_data_clean.Rdata")
SAWS_data_clean$temp <- (SAWS_data_clean$mx+SAWS_data_clean$mn)/2

SAWS_meta <- read.csv("data/SAWS_meta_data.csv")

## Remove <10 year and >10%NA time series
short <- SAWS_meta[SAWS_meta$length <= 3650,]
mnNA <- SAWS_meta[SAWS_meta$na_perc_mn >= 10,]
mxNA <- SAWS_meta[SAWS_meta$na_perc_mx >= 10,]

SAWS_data_cropped <- droplevels(SAWS_data_clean[!(SAWS_data_clean$station %in% short$station),])
SAWS_data_cropped <- droplevels(SAWS_data_cropped[!(SAWS_data_cropped$station %in% mnNA$station),])
SAWS_data_cropped <- droplevels(SAWS_data_cropped[!(SAWS_data_cropped$station %in% mxNA$station),])

save(SAWS_data_cropped, file = "data/SAWS_data_cropped.Rdata")

## Crop meta-data
SAWS_meta_cropped <- SAWS_meta[SAWS_meta$station %in% SAWS_data_cropped$station,]
write.csv(SAWS_meta_cropped, file = "data/SAWS_meta_data_cropped.csv", row.names = F)
