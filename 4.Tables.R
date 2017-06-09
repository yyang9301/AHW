###########################################################################
### "4.Tables.R"
## This script creates the tables seen in the manuscript
# 1. Load all libraries and functions used in this script
# 2. Create table of statistics for each SOM node
# 3. Supplementary table
#############################################################################


# 1. Load all libraries and functions used in this script -----------------
library(xtable)
# library(ggplot2)
# library(plyr)
# library(dplyr)
# library(reshape2)
# library(tidyr)
# library(tibble)
# library(doMC); doMC::registerDoMC(cores = 4)
source("func/synoptic.func.R")
source("func/som.func.R")


# 2. Create table of statistics for each SOM node -------------------------

# Load MHW and SOM results from anomaly data prepared by "2.Data_assembly.R"
load("data/SACTN/SACTN_events.Rdata")
load("data/node_all_anom.Rdata")

# Run the metric summary function on these data
node_table <- node.summary.metrics(node_all_anom, SACTN_events)
save(node_table, file = "data/node_table.Rdata")
write.csv(node_table, file = "data/node_table.csv")

# Trim off unused columns
load("data/node_table.Rdata")
node_table <- node_table[,c(1:9, 11, 14, 17)]
node_table[10,1] <- "ALL"

# Generate table for LaTeX
print(xtable(node_table), include.rownames=FALSE)


# 3. Supplementary table --------------------------------------------------

load("~/SACTN/metadata/site_list_v4.1.Rdata")

# Exclude time series under 10 years or over 10% NA
site_list <- droplevels(site_list[site_list$NA.perc <= 10, ]) # 50 sites
site_list <- droplevels(site_list[site_list$length >= 3650, ]) # 26 sites
xtable(site_list)

