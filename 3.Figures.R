###########################################################################
### "3.Figures.R"
## This script creates the figures for the paper and supplemental
# 1. Load all libraries and functions used in this script
# 2. Create synoptic figure for each event
# 3. Create synoptic figure showing SOM nodes
# 4. Create lolliplots for the SOM nodes
# 5. Create dendrogram for HCA results
# 6. Create ordiplot for MDS results
#############################################################################


# 1. Load all libraries and functions used in this script -----------------
# library(ggplot2)
# library(plyr)
# library(dplyr)
# library(reshape2)
# library(tidyr)
# library(tibble)
# library(doMC); doMC::registerDoMC(cores = 4)
source("func/synoptic.func.R")
source("func/som.func.R")


# 2. Create synoptic figure for each event  -------------------------------

# Load SACTN data
load("~/data/SACTN/AHW/SACTN_clims.Rdata")
load("data/SACTN/SACTN_events.Rdata")
load("setupParams/SACTN_site_list.Rdata")

# The files for loading
event_idx <- data.frame(event = dir("data/SOM", full.names = TRUE),
                        x = length(dir("data/SOM")))

# Create a synoptic atlas figure for each MHW
system.time(plyr::ddply(event_idx, c("event"), synoptic.fig, .progress = "text")) # 539 seconds


# 3. Create synoptic figure showing SOM nodes -----------------------------

load("data/node_means.Rdata")
load("data/node_all_anom.Rdata")
all.panels(node_means, node_all_anom)


# 4. Create lolliplots for the SOM nodes ----------------------------------

load("data/SACTN/SACTN_events.Rdata")
load("data/node_all_anom.Rdata")

node_all <- merge(node_all_anom, SACTN_events, by = c("event", "site", "season", "event_no"))
ggplot(data = node_all, aes(x = date_start, y = int_cum)) +
  geom_lolli() +
  geom_point(aes(colour = season)) +
  facet_wrap(~node) +
  labs(x = "", y = "cummulative intensity (°Cxdays)")
ggsave("graph/SOM_lolli.pdf", height = 9, width = 9)


# 5. Create dendrogram for HCA results ------------------------------------


# 6. Create ordiplot for MDS results --------------------------------------

