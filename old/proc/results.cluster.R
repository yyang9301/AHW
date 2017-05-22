#############################################################################
###"proc/results.cluster.R"
## This script does:
# 1. Load required data
# 2. Run cluster analysis
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
library(vegan)
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


# 2. Run cluster analysis -------------------------------------------------

# Combine data frames for modeling
all_norm <- cbind(BRAN_norm, ERA_norm[,-1])
all_anom <- cbind(BRAN_anom, ERA_anom[,-1])

# Ordination
# coast.deco <- decostand(all_norm[,-1], method = "standardize") # Standardize all values for similarity
all.vegdist <- function(df){
  coast.deco <- decostand(df[,-1], method = "standardize") # Standardize all values for similarity
  rownames(coast.deco) <- BRAN_norm$event
  coast.dis <- vegdist(coast.deco, method = "euclidean") # Distance matrix for ordinations
}

norm_MDS <- metaMDS(all.vegdist(all_norm), k = 4, trace = F)
anom_MDS <- metaMDS(all.vegdist(all_norm), k = 4, trace = F)

ordiplot(norm_MDS)
ordiplot(anom_MDS)
# ordiplot(coast.mds, type = "t") # Show site names... This looks terrible

# Cluster
norm_clust <- hclust(all.vegdist(all_norm), method = "ward.D2") # method = "average" pushes a few sites out
anom_clust <- hclust(all.vegdist(all_anom), method = "ward.D2")
norm_groups <- cutree(norm_clust, k = 6)
anom_groups <- cutree(anom_clust, k = 6)

# Plot cluster with red boxes for coast
pdf("~/Desktop/cluster.pdf", pointsize = 10)
plot(clust, hang = -0.05, cex = 0.4, main = "Anomaly Data")
rect.hclust(clust, k = 6, border = "red") # Draw dendrogram with red borders around the 3 clusters
dev.off()

# Overlay clusters onto the ordination and save
## NB: all of the commented out code below may be 'turned on' to see how this affects the output
pdf("~/Desktop/ordClustOverlay.pdf", pointsize = 10)
# Ellipses showing clusters. Spread is to large, ellipses lok crappy...
# plot(coast.mds, display = "sites")
# ordiellipse(coast.mds, groups, lty = 2, col = "red")
# Polygons Showing clusters. This looks the best.
plot(coast.mds, display = "sites")
ordihull(coast.mds, groups, lty = 2, col = "red")
# Ordi spiders, just for fun...
#plot(coast.mds, display = "sites")
#ordispider(coast.mds, groups, lty = 2, col = "red")
# Overlay classification tree on ordination... not working
#plot(coast.mds, display="sites")
#ordicluster(coast.mds, hclust(vegdist(coast.stats)), prune = 3)
dev.off()


# 3. Create figures -------------------------------------------------------

# Rather force these results into the same format as SOM so that the same functions may be used
norm_cluster <- data.frame(unit.classif = norm_groups)
anom_cluster <- data.frame(unit.classif = anom_groups)
# Create means by cluster
system.time(res_all_norm_mean <- som.unpack.mean(all_norm, norm_cluster)) # 8 seconds
system.time(res_all_anom_mean <- som.unpack.mean(all_anom, anom_cluster)) # 8 seconds
# Calculate cluster data for plotting
event_node_norm <- event.node(all_norm, norm_cluster)
event_node_norm$duration <- event_list$duration
event_node_anom <- event.node(all_anom, anom_cluster)
event_node_anom$duration <- event_list$duration
# Figures
system.time(all.panels(res_all_norm_mean, event_node_norm))
system.time(all.panels(res_all_anom_mean, event_node_anom))


# 4. Investigate clusters -------------------------------------------------

# Subset out each node
event_list$norm_cluster <- as.factor(norm_groups)
event_list$anom_cluster <- as.factor(anom_groups)

ggplot(data = event_list, aes(x = date_start)) +
  # geom_point(aes(colour = norm_cluster, y = int_cum), size = 6) #+
  # geom_point(aes(colour = anom_cluster, y = int_cum), size = 6) #+
  # geom_density(aes(colour = anom_cluster)) #+
  # geom_col(aes(y = int_cum)) #+
  geom_count(aes(y = date_start, colour = anom_cluster, shape = season, size = duration), alpha = 0.5)
