#############################################################################
###"func/site.list.order.meta.R"
## This script does:
# 1. Finds the ID of the nearest lon/lat pair in fortified shoreline;
# 2. Selects the lon/lat/order of row in the fortified shoreline with the minimum distance between it and the point coordinate (lon/lat) in the input data frame;
# 3. Arranges the sites sequentially from west to east
## DEPENDS ON:
library(FNN)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
## USED BY:
#"load/ALLdaily_v4.0.R"
## CREATES:
#"metadata/site_list_v4.0.csv"
#############################################################################


source("func/meta.data.R")

site.list <- function(){
  
  ## Load the site list and daily data
  load("metadata/site_list_v4.1.Rdata")
  site_list <- site_list[,1:9]
  load("data/SACTNdaily_v4.1.Rdata")
  SACTNdaily_v4.1$index <- as.factor(paste(SACTNdaily_v4.1$site, SACTNdaily_v4.1$src, sep = "/ "))
  
  ## Calculate the meta-data for each site and append it to the site list
  meta <- SACTNdaily_v4.1 %>% 
    group_by(index) %>%
    nest() %>% 
    mutate(mod = data %>% map(meta.data))
  meta <- unnest(meta, mod)
  meta$data <- NULL
  # Merge 
  site_list <- merge(site_list, meta, by = "index")
  
  ## Order the sites correctly along coast
  # Load the shoreline
  load("metadata/shore.Rdata")
  # Converts to data frame and keep the shoreline, not islands etc. The returned indices should be in the same order they appeared in in the original shoreline (i.e. coordinates arranged sequentially from east to west along the shoreline)
  shore <- droplevels(subset(shore, (PID == 1)))
  # Create index of site position along coast
  sites_idx <- as.data.frame(knnx.index(shore[,4:5], as.matrix(site_list[,5:6]), k = 1))
  # Add the index to the front of the sites data.frame
  site_list <- cbind(sites_idx, site_list)
  # Order sites by index, west to east
  site_list <- site_list[order(site_list$V1, decreasing = FALSE),]
  # Remove index column
  site_list$V1 <- NULL
  # Relabel order of sites to match new sequential order
  site_list$order <- as.integer(1:length(site_list$order))
  # plot(site_list$lon, site_list$lat, pch = 1, cex = 0.3, col = "blue") # just checking --> okay
  # Order index factor for use with other scripts
  site_list$index <- reorder(site_list$index, site_list$order)
  site_list <- site_list[,c(2:4,1,5:length(site_list))]
  # Save site list as .Rdata and .csv files
  save(site_list, file = "metadata/site_list_v4.1.Rdata")
  write.csv(site_list, "metadata/site_list_v4.1.csv", row.names = FALSE)
}
