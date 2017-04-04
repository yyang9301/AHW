#############################################################################
###"proc/results.som.R"
## This script does:
# 1. Load required data
# 2. Run SOMs
# 3. Have a peak at the models
# 4. Unscale SOM results for plotting
# 5. Create figures
# 6. Additional analyses
## DEPENDS ON:
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(viridis)
library(doMC); registerDoMC(cores = 4)
source("func/som.func.R")
## USED BY:
# 
## CREATES:
# 
#############################################################################


# 1. Load required data ---------------------------------------------------

## Load the synoptic data for all events
  # NB:These following data frames are not pushed to GitHub due to their size
  # So one must run the commented out load scripts that create these data frames if they are not already saved locally

# All BRAN normal data
# system.time(BRAN_norm <- ddply(event_idx, .(event), load.data.packet, var = c("BRAN/temp", "BRAN/u", "BRAN/v"), .parallel = T)) # 91 seconds
# save(BRAN_norm, file = "data/BRAN_norm.Rdata")
system.time(load("data/BRAN_norm.Rdata")) # 2 seconds

# All ERA normal data
# system.time(ERA_norm <- ddply(event_idx, .(event), load.data.packet, var = c("ERA/temp", "ERA/u", "ERA/v"), .parallel = T)) # 7 seconds
# save(ERA_norm, file = "data/ERA_norm.Rdata")
system.time(load("data/ERA_norm.Rdata")) # 1 seconds

# All BRAN anomaly data
# system.time(BRAN_anom <- ddply(event_idx, .(event), load.data.packet, var = c("BRAN/temp-anom", "BRAN/u-anom", "BRAN/v-anom"), .parallel = T)) # 86 seconds
# save(BRAN_anom, file = "data/BRAN_anom.Rdata")
system.time(load("data/BRAN_anom.Rdata")) # 2 seconds

# All ERA anomaly data
# system.time(ERA_anom <- ddply(event_idx, .(event), load.data.packet, var = c("ERA/temp-anom", "ERA/u-anom", "ERA/v-anom"), .parallel = T)) # 7 seconds
# save(ERA_anom, file = "data/ERA_anom.Rdata")
system.time(load("data/ERA_anom.Rdata")) # 1 seconds


# 2. Run SOMs -------------------------------------------------------------

## NB: Only run one of the sets of SOMs below

# Combine data frames for modeling
all_norm <- cbind(BRAN_norm, ERA_norm[,-1])
all_anom <- cbind(BRAN_anom, ERA_anom[,-1])

## 3x3
# Normal data
system.time(som_all_norm <- som.model(all_norm)) # 121 seconds
# Anomalies
system.time(som_all_anom <- som.model(all_anom)) # 118 seconds

## 2x1
# Normal data
system.time(som_all_norm <- som.model(all_norm, xdim = 2, ydim = 1)) # 23 seconds
# Anomalies
system.time(som_all_anom <- som.model(all_anom, xdim = 2, ydim = 1)) # 23 seconds

## 3x1
# Normal data
system.time(som_all_norm <- som.model(all_norm, xdim = 3, ydim = 1)) # 32 seconds
# Anomalies
system.time(som_all_anom <- som.model(all_anom, xdim = 3, ydim = 1)) # 32 seconds

## 2x2
# Normal data
system.time(som_all_norm <- som.model(all_norm, xdim = 2, ydim = 2)) # 117 seconds
# Anomalies
system.time(som_all_anom <- som.model(all_anom, xdim = 2, ydim = 2)) # 106 seconds

## 3x2
# Normal data
system.time(som_all_norm <- som.model(all_norm, xdim = 3, ydim = 2)) # 170 seconds
# Anomalies
system.time(som_all_anom <- som.model(all_anom, xdim = 3, ydim = 2)) # 169 seconds


### This section is no longer used as it has been decided not to run the events by coast
  ### But rather to allow the clustering of all the data to separate out the coasts, or not
# ### Run the SOMs with only one coastal section of events
# event_idx$site <- sapply(strsplit(basename(as.character(event_idx$event)), ".Rdata"),  "[[", 1)
# event_idx$site <- sapply(strsplit(basename(as.character(event_idx$site)), "_"),  "[[", 1)
# event_idx_wc <- event_idx[event_idx$site %in% event_list$site[event_list$coast == "wc"],] # 22
# event_idx_sc <- event_idx[event_idx$site %in% event_list$site[event_list$coast == "sc"],] # 69
# event_idx_ec <- event_idx[event_idx$site %in% event_list$site[event_list$coast == "ec"],] # 4
# 
# ## WC - 2x2
# all_norm <- cbind(BRAN_norm, ERA_norm[,-1])
# all_anom <- cbind(BRAN_anom, ERA_anom[,-1])
# all_norm <- all_norm[all_norm$event %in% event_idx_wc$event,]
# all_anom <- all_anom[all_anom$event %in% event_idx_wc$event,]
# # Normal data
# system.time(som_all_norm <- som.model(all_norm, xdim = 2, ydim = 2)) # 12 seconds
# # Anomalies
# system.time(som_all_anom <- som.model(all_anom, xdim = 2, ydim = 2)) # 12 seconds
# 
# ## SC - 3x2
# all_norm <- cbind(BRAN_norm, ERA_norm[,-1])
# all_anom <- cbind(BRAN_anom, ERA_anom[,-1])
# all_norm <- all_norm[all_norm$event %in% event_idx_sc$event,]
# all_anom <- all_anom[all_anom$event %in% event_idx_sc$event,]
# # Normal data
# system.time(som_all_norm <- som.model(all_norm, xdim = 3, ydim = 2)) # 51 seconds
# # Anomalies
# system.time(som_all_anom <- som.model(all_anom, xdim = 3, ydim = 2)) # 54 seconds
# 
# ## EC - 2x1
# all_norm <- cbind(BRAN_norm, ERA_norm[,-1])
# all_anom <- cbind(BRAN_anom, ERA_anom[,-1])
# all_norm <- all_norm[all_norm$event %in% event_idx_ec$event,]
# all_anom <- all_anom[all_anom$event %in% event_idx_ec$event,]
# # Normal data
# system.time(som_all_norm <- som.model(all_norm, xdim = 2, ydim = 1)) # 1 seconds
# # Anomalies
# system.time(som_all_anom <- som.model(all_anom, xdim = 2, ydim = 1)) # 1 seconds


# 3. Have a peak at the models --------------------------------------------

# Pick which data frame to visualise
# som_model <- som_BRAN_temp
som_model <- som_all_norm
som_model <- som_all_anom

# The different visualisations for quality control
# Training progress. How many iterations the model needs to git gud
plot(som_model, type = "changes")
# Counts within nodes. How many synoptic states fall within each node
plot(som_model, type = "counts", main = "Node Counts")
# Map quality
plot(som_model, type = "quality", main = "Node Quality/Distance")
# Neighbour distances
plot(som_model, type = "dist.neighbours", main = "SOM neighbour distances", palette.name = grey.colors)
# Code spread. This shows the values for each pixel as a line graph, so it doesn't look like much
  # NB: With massive vectors this may take a minute or so to render
plot(som_model, type = "codes")

# The WCSS metric for different kmeans clustering
codes <- som_model$codes
wss <- (nrow(codes)-1)*sum(apply(codes,2,var))
for (i in 2:nrow(codes)-1) wss[i] <- sum(kmeans(codes,
                                     centers=i)$withinss)
par(mar = c(5.1,4.1,4.1,2.1))
plot(1:(nrow(codes)-1), wss, type = "b", xlab = "Kmeans Clusters",
     ylab = "Within groups sum of squares", main = "Within cluster sum of squares (WCSS)")
rm(codes) # Save on RAM...
# It appears that as few as three clusters are needed
# That is surprising...

# Form clusters on grid
# use hierarchical clustering to cluster the synoptic vectors
som_cluster <- cutree(hclust(dist(som_model$codes)), 3)

# Show the map with different colours for every cluster						  
plot(som_model, type = "mapping", bgcol = som_cluster, main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

# Show the same plot with the synoptic vectors as well
  # NB: With massive vectors this may take a minute or so to render
plot(som_model, type = "codes", bgcol = som_cluster, main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)


# 4. Unscale SOM results for plotting -------------------------------------

## Unpack the results for plotting
# mean
system.time(res_all_norm_mean <- som.unpack.mean(all_norm, som_all_norm)) # 14 seconds
system.time(res_all_anom_mean <- som.unpack.mean(all_anom, som_all_anom)) # 14 seconds
# rescale
system.time(res_all_norm_rescale <- som.unpack.rescale(all_norm, som_all_norm)) # 71 seconds
system.time(res_all_anom_rescale <- som.unpack.rescale(all_anom, som_all_anom)) # 74 seconds

## Create node index
node_all_norm <- event.node(all_norm, som_all_norm)
node_all_anom <- event.node(all_anom, som_all_anom)


# 5. Create figures -------------------------------------------------------

## Create figures
# Mean
system.time(all.panels(res_all_norm_mean, node_all_norm)) # 14 seconds
system.time(all.panels(res_all_anom_mean, node_all_anom)) # 15 seconds
# Rescale
system.time(all.panels(res_all_norm_rescale, node_all_norm)) # 14 seconds
system.time(all.panels(res_all_anom_rescale, node_all_anom)) # 15 seconds


# 6. Additional analyses --------------------------------------------------

# Seasonality of events

