#############################################################################
###"proc/SOM.R"
## This script does:

# 1. Load required data
# 2. Run SOM and have a peak
# 3. Unscale SOM results for plotting

## DEPENDS ON:

library(kohonen)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(doMC); registerDoMC(cores = 4)



## USED BY:
# 
## CREATES:
# 
#############################################################################


# 1. Load required data ---------------------------------------------------

# The files
file_idx <- data.frame(file = dir("data/SOM", full.names = TRUE),
                       x = length(dir("data/SOM")))

# A helper function for the following function
col.shimmy <- function(df, var = "blank"){
  colnames(df)[3] <- "value"
  df$var <- rep(var, nrow(df))
  return(df)
}

# Function for loading and formatting data for use with SOMs
# df <- file_idx[1,] # tester...
load.data.packet <- function(df){
  df2 <- as.character(df$file)
  load(df2)
  # Combine all BRAN data
  res <- col.shimmy(SOM_packet$BRAN_temp, "BRAN/temp")
  res <- rbind(res, col.shimmy(SOM_packet$BRAN_uv[,c(1:3)], "BRAN/u"))
  res <- rbind(res, col.shimmy(SOM_packet$BRAN_uv[,c(1:2,4)], "BRAN/v"))
  res <- rbind(res, col.shimmy(SOM_packet$BRAN_temp_anom, "BRAN/temp-anom"))
  res <- rbind(res, col.shimmy(SOM_packet$BRAN_uv_anom[,c(1:3)], "BRAN/u-anom"))
  res <- rbind(res, col.shimmy(SOM_packet$BRAN_uv_anom[,c(1:2,4)], "BRAN/v-anom"))
  # Then the ERA data all in one data frame
  res <- rbind(res, col.shimmy(SOM_packet$ERA_temp, "ERA/temp"))
  res <- rbind(res, col.shimmy(SOM_packet$ERA_uv[,c(1:3)], "ERA/u"))
  res <- rbind(res, col.shimmy(SOM_packet$ERA_uv[,c(1:2,4)], "ERA/v"))
  res <- rbind(res, col.shimmy(SOM_packet$ERA_temp_anom, "ERA/temp-anom"))
  res <- rbind(res, col.shimmy(SOM_packet$ERA_uv_anom[,c(1:3)], "ERA/u-anom"))
  res <- rbind(res, col.shimmy(SOM_packet$ERA_uv_anom[,c(1:2,4)], "ERA/v-anom"))
  # Round x/y for better colum names
  res$x <- round(res$x,2) # Have to use second decimal place as first decimal rounding creates possible duplicate entries
  res$y <- round(res$y,2)
  # Remove any possible duplicates. SHould be none.
  res <- res[, .(value = mean(value, na.rm = TRUE)),
               by = .(x,y, var)]
  res$coords <- paste0(res$x, "_" , res$y, "_", res$var)
  res_wide <- t(res$value)
  colnames(res_wide) <- res$coords
  # res_wide_matrix <- scale(res_wide) # Can't run on only a single row... obviously
  return(res_wide)
}

# Load the synoptic data for only the first ten events for now
system.time(data_packet <- ddply(file_idx[1:10,], .(file), load.data.packet, .parallel = T)) # 25 seconds
row.names(data_packet) <- sapply(strsplit(basename(as.character(data_packet$file)), ".Rdata"),  "[[", 1)
data_packet$file <- NULL


# 2. Run SOM and have a peak ----------------------------------------------

# Create a scaled matrix for the SOM
system.time(data_packet_matrix <- as.matrix(scale(data_packet))) # 6 seconds

# Create the grid that the SOM will use to determine the number of nodes
som_grid <- somgrid(xdim = 2, ydim = 2, topo="hexagonal") # This grid looks for the best 4 nodes

# Run the SOM
system.time(som_model <- som(data_packet_matrix, # 17 seconds
                             grid = som_grid, 
                             rlen = 100, 
                             alpha = c(0.05,0.01), 
                             n.hood = "circular",
                             keep.data = TRUE ))

# Visualise
plot(som_model, type = "changes")
# Counts within nodes
plot(som_model, type = "counts", main = "Node Counts")
# Map quality
plot(som_model, type = "quality", main = "Node Quality/Distance")
# Neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name = grey.colors)
# Code spread
# plot(som_model, type = "codes") # Don't run this as it is too beefy due to all of the pixels

# The WCSS metric for different kmeans clustering
codes <- som_model$codes
wss <- (nrow(codes)-1)*sum(apply(codes,2,var))
for (i in 2:nrow(codes)-1) wss[i] <- sum(kmeans(codes,
                                     centers=i)$withinss)
par(mar = c(5.1,4.1,4.1,2.1))
plot(1:(nrow(codes)-1), wss, type = "b", xlab = "Kmeans Clusters",
     ylab = "Within groups sum of squares", main = "Within cluster sum of squares (WCSS)")

# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes)), 2)

# Show the map with different colours for every cluster						  
plot(som_model, type="mapping", bgcol = som_cluster, main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

# Show the same plot with the codes instead of just colours
# plot(som_model, type="codes", bgcol = som_cluster, main = "Clusters") # This is too beefy...
# add.cluster.boundaries(som_model, som_cluster)


# 3. Unscale SOM results for plotting -------------------------------------

# Melt data_packet
data_packet$event <- rownames(data_packet)
data_packet_long <- melt(data_packet, id = "event")

# Determine which event goes in which node
event_node <- data.frame(event = data_packet$event, node = som_model$unit.classif)

data_packet_long <- data_packet_long %>%
  group_by(event) %>%
  mutate(node = event_node$node[event_node$event == event][1])
data_packet_long <- data.table(data_packet_long)

# Create the mean values that serve as the unscaled results from the SOM
var_unscaled <- data_packet_long[, .(value = mean(value, na.rm = TRUE)),
                             by = .(node, variable)]
var_unscaled$x <- as.numeric(sapply(strsplit(as.character(var_unscaled$variable), "_"), "[[", 1))
var_unscaled$y <- as.numeric(sapply(strsplit(as.character(var_unscaled$variable), "_"), "[[", 2))
var_unscaled$var <- sapply(strsplit(as.character(var_unscaled$variable), "_"), "[[", 3)
var_unscaled$variable <- NULL

# Test plot
ggplot(data = var_unscaled, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  facet_grid(var~node)


