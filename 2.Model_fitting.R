############################################################################
### "2.Model_fitting.R"
## This script calculates MHWs, creates data packets, and clusters events
# 1. Load all libraries and functions used in this script 
# 2. Calculate MHWs from cropped SACTN data
# 3. Create data packets from BRAN and ERA data based on MHWs
# 4. Cluster the events using SOMs
# 5. Create mean synoptic states for each node
# 6. Perform HCA and MDS on clim vs. event days
# 7. ANOSIM on differences between SOM nodes
############################################################################


# 1. Load all libraries and functions used in this script  ----------------
library(doMC); registerDoMC(cores = 4)
library(tidyverse)
library(RmarineHeatWaves)
source("func/detect.full.R")
source("func/synoptic.func.R") # This loads several files
source("func/som.func.R")


# 2. Calculate MHWs from cropped SACTN data -------------------------------

# Prep data for analysis
load("~/data/SACTN/AHW/SACTN_cropped.Rdata")
SACTN_cropped <- SACTN_cropped[,c(1,4:5)]
colnames(SACTN_cropped)[2] <- "t"
load("setupParams/SACTN_site_list.Rdata")
load("setupParams/SACTN_analysis_period.Rdata") # This file was created manually
SACTN_cropped <- SACTN_cropped %>% 
  group_by(site) %>% 
  mutate(start = SACTN_analysis_period$start[SACTN_analysis_period$site == site[1]]) %>% 
  mutate(end = SACTN_analysis_period$end[SACTN_analysis_period$site == site[1]])
SACTN_cropped <- data.frame(SACTN_cropped)

SACTN_cropped$idx <- as.numeric(SACTN_cropped$site) # Use numeric values as site names introduce problems

# Calculate events
system.time(SACTN_all <- plyr::dlply(SACTN_cropped, c("idx"), detect.SACTN, .parallel = TRUE)) ## 9 seconds

# Calculate climatologies
SACTN_clims <- data.frame()
for(i in 1:max(SACTN_cropped$idx)){
  clims <- SACTN_all[[i]]$clim
  SACTN_clims <- rbind(SACTN_clims, clims)
}
rm(clims)
save(SACTN_clims, file = "~/data/SACTN/AHW/SACTN_clims.Rdata")

# Calculate Events
SACTN_events <- data.frame()
for(i in 1:max(SACTN_cropped$idx)){
  events <- SACTN_all[[i]]$event
  SACTN_events <- rbind(SACTN_events, events)
}
rm(SACTN_all, SACTN_analysis_period, SACTN_cropped, events)

# Screen out cold-spells
SACTN_events <- filter(SACTN_events, type == "MHW") # 976 events

# Screen out those under 15 days in length
SACTN_events <- filter(SACTN_events, duration >= 15) # 129

# Screen out those occurring before or after reanalysis period
SACTN_events <- filter(SACTN_events, date_start > as.Date("1994-01-01")) # 98
SACTN_events <- filter(SACTN_events, date_stop < as.Date("2016-08-31")) # 98

# Season index
summer <- format(seq(as.Date("2015-12-01"), as.Date("2016-02-29"), by = "day"),"%m-%d")
autumn <- format(seq(as.Date("2016-03-01"), as.Date("2016-05-31"), by = "day"),"%m-%d")
winter <- format(seq(as.Date("2016-06-01"), as.Date("2016-08-31"), by = "day"),"%m-%d")
spring <- format(seq(as.Date("2016-09-01"), as.Date("2016-11-30"), by = "day"),"%m-%d")

# Add coastal clusters
SACTN_events$coast <- NA
SACTN_events$coast[SACTN_events$site %in% SACTN_site_list$site[SACTN_site_list$coast == "wc"]] <- "wc"
SACTN_events$coast[SACTN_events$site %in% SACTN_site_list$site[SACTN_site_list$coast == "sc"]] <- "sc"
SACTN_events$coast[SACTN_events$site %in% SACTN_site_list$site[SACTN_site_list$coast == "ec"]] <- "ec"

# Add season
SACTN_events$season <- NA
SACTN_events$season[format(SACTN_events$date_start,"%m-%d") %in% summer] <- "summer"
SACTN_events$season[format(SACTN_events$date_start,"%m-%d") %in% autumn] <- "autumn"
SACTN_events$season[format(SACTN_events$date_start,"%m-%d") %in% winter] <- "winter"
SACTN_events$season[format(SACTN_events$date_start,"%m-%d") %in% spring] <- "spring"

# Add event indexing column for later
SACTN_events$event <- paste0(SACTN_events$site,"_",SACTN_events$event_no)

# Save
save(SACTN_events, file = "data/SACTN/SACTN_events.Rdata")


# 3. Create data packets from BRAN and ERA data based on MHWs -------------

# Load SACTN data
load("~/data/SACTN/AHW/SACTN_clims.Rdata")
load("data/SACTN/SACTN_events.Rdata")
load("setupParams/SACTN_site_list.Rdata")

# Load reanalysis data
load("data/BRAN/BRAN_temp_clim.Rdata")
load("data/BRAN/BRAN_u_clim.Rdata")
load("data/BRAN/BRAN_v_clim.Rdata")
load("data/ERA/ERA_temp_clim.Rdata")
load("data/ERA/ERA_u_clim.Rdata")
load("data/ERA/ERA_v_clim.Rdata")

# Merge U and V data frames for data.packet()
system.time(BRAN_uv_clim <- merge(BRAN_u_clim, BRAN_v_clim, by = c("x", "y", "date"))) # 15 seconds
BRAN_uv_clim <- BRAN_uv_clim[order(BRAN_uv_clim$x, BRAN_uv_clim$y, BRAN_uv_clim$date),]
system.time(ERA_uv_clim <- merge(ERA_u_clim, ERA_v_clim, by = c("x", "y", "date"))) # 21 seconds
ERA_uv_clim <- ERA_uv_clim[order(ERA_uv_clim$x, ERA_uv_clim$y, ERA_uv_clim$date),]

# ERA Interim file indices for data.packet()
file_1_dates <- seq(as.Date("1979-01-01"), as.Date("1989-12-31"), by = "day")
file_2_dates <- seq(as.Date("1990-01-01"), as.Date("1998-12-31"), by = "day")
file_3_dates <- seq(as.Date("1999-01-01"), as.Date("2007-12-31"), by = "day")
file_4_dates <- seq(as.Date("2008-01-01"), as.Date("2016-12-31"), by = "day")

# Create a data packet for each MHW
system.time(plyr::ddply(SACTN_events, c("event"), data.packet, .parallel = TRUE)) # 539 seconds


# 4. Cluster the events using SOMs ----------------------------------------

# The files for loading
event_idx <- data.frame(event = dir("data/SOM", full.names = TRUE),
                        x = length(dir("data/SOM")))

# Load SACTN info if the previous sections were not run
load("data/SACTN/SACTN_events.Rdata")
load("setupParams/SACTN_site_list.Rdata")

# All BRAN anomaly data
# system.time(BRAN_anom <- plyr::ddply(event_idx, c("event"), load.data.packet,
#                                      var = c("BRAN/temp-anom", "BRAN/u-anom", "BRAN/v-anom"), .parallel = T)) # 4 seconds
# save(BRAN_anom, file = "data/BRAN/BRAN_anom.Rdata")
load("data/BRAN_anom.Rdata")

# All ERA anomaly data
# system.time(ERA_anom <- plyr::ddply(event_idx, c("event"), load.data.packet,
#                                     var = c("ERA/temp-anom", "ERA/u-anom", "ERA/v-anom"), .parallel = T)) # 5 seconds
# save(ERA_anom, file = "data/ERA_anom.Rdata")
load("data/ERA_anom.Rdata")

# Combine data frames for modeling
all_anom <- cbind(BRAN_anom, ERA_anom[,-1])

# Run SOM
system.time(som_mdel_pci <- som.model.PCI(all_anom)) # 2 seconds
save(som_mdel_pci, file = "data/som_model_pci.Rdata")

# Extract node information
node_all_anom <- event.node(all_anom, som_mdel_pci)
save(node_all_anom, file = "data/node_all_anom.Rdata")


# 5. Create mean synoptic states for each node ----------------------------

# Load required data if the above sections were not run
load("data/som_model_pci.Rdata")
# load("data/node_all_anom.Rdata")
load("data/BRAN_anom.Rdata")
load("data/ERA_anom.Rdata")
all_anom <- cbind(BRAN_anom, ERA_anom[,-1]); rm(BRAN_anom, ERA_anom)

# Calculate node means
node_means <- som.unpack.mean(all_anom, som_mdel_pci)
save(node_means, file = "data/node_means.Rdata")


# 6. Perform HCA and MDS on clim vs. event days ---------------------------


## Calculate MDS
# all_anom_0.5$node <- NULL
# all_anom_0.5_MDS <- metaMDS(vegdist(decostand(all_anom_0.5[,-c(1)],
#                                               method = "standardize"), 
#                                     method = "euclidean"), try = 100)
# save(all_anom_0.5_MDS, file = "results/all_anom_0.5_MDS.Rdata")
load("results/all_anom_0.5_MDS.Rdata")

# Fit environmental variables
ord_fit <- envfit(all_anom_0.5_MDS ~ coast + season, data = event_list[,26:27])
# ord_fit
ord_fit_df <- as.data.frame(ord_fit$factors$centroids)
ord_fit_df$factors <- rownames(ord_fit_df)

# Create MDS dataframe
mds_df <- data.frame(all_anom_0.5_MDS$points)

# Plot the fits
ggplot(data = mds_df, aes(x = MDS1, y = MDS2)) +
  geom_point(colour = "salmon") +
  geom_text(data = ord_fit_df, aes(label = factors, x = NMDS1, y = NMDS2))

# 7. ANOSIM on differences between SOM nodes ------------------------------

som_anosim <- anosim(as.matrix(scale(all_anom_0.5[,-1])), node_all_anom_pci_1r$node)