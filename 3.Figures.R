###########################################################################
### "3.Figures.R"
## This script creates the figures for the paper and supplemental
# 1. Load all libraries and functions used in this script
# 2. Create synoptic figure for each event
# 3. Create figure showing SOM nodes

# 1. Load all packages required for the analyses
# 2. Load all functions etc. found in other scripts
# 3. Load the site list and spatial data used for the map
# 4. Create the map of Africa to be windowed
# 5. Create the map of southern Africa
# 6. Add site list information
# 7. Create figure01
# 8. Load analysis results
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


# 3. Create figure showing SOM nodes --------------------------------------

