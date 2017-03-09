#############################################################################
###"proc/SOM.R"
## This script does:
# 1. Load required data
# 2. Run SOM and have a peak
# 3. Unscale SOM results for plotting
## DEPENDS ON:
library(kohonen)
library(ggplot2)
library(viridis)
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

# The site list info
load("setupParams/SACTN_site_list.Rdata")

# The files
event_idx <- data.frame(event = dir("data/SOM", full.names = TRUE),
                       x = length(dir("data/SOM")))

# A helper function for the following function
col.shimmy <- function(df, var = "blank"){
  colnames(df)[3] <- "value"
  df$var <- rep(var, nrow(df))
  return(df)
}

# Function for loading and formatting data for use with SOMs
# df <- file_idx[1,] # tester...
load.data.packet <- function(df, var){
  df2 <- as.character(df$event)
  load(df2)
  res <- data.frame()
  # Combine all BRAN data
  if("BRAN/temp" %in% var) res <- rbind(res, col.shimmy(SOM_packet$BRAN_temp, "BRAN/temp"))
  if("BRAN/u" %in% var) res <- rbind(res, col.shimmy(SOM_packet$BRAN_uv[,c(1:3)], "BRAN/u"))
  if("BRAN/v" %in% var) res <- rbind(res, col.shimmy(SOM_packet$BRAN_uv[,c(1:2,4)], "BRAN/v"))
  # Combine all BRAN anomaly data
  if("BRAN/temp-anom" %in% var) res <- rbind(res, col.shimmy(SOM_packet$BRAN_temp_anom, "BRAN/temp-anom"))
  if("BRAN/u-anom" %in% var) res <- rbind(res, col.shimmy(SOM_packet$BRAN_uv_anom[,c(1:3)], "BRAN/u-anom"))
  if("BRAN/v-anom" %in% var) res <- rbind(res, col.shimmy(SOM_packet$BRAN_uv_anom[,c(1:2,4)], "BRAN/v-anom"))
  # Combine all ERA data
  if("ERA/temp" %in% var) res <- rbind(res, col.shimmy(SOM_packet$ERA_temp, "ERA/temp"))
  if("ERA/u" %in% var) res <- rbind(res, col.shimmy(SOM_packet$ERA_uv[,c(1:3)], "ERA/u"))
  if("ERA/v" %in% var) res <- rbind(res, col.shimmy(SOM_packet$ERA_uv[,c(1:2,4)], "ERA/v"))
  # Combine all ERA anomaly data
  if("ERA/temp-anom" %in% var) res <- rbind(res, col.shimmy(SOM_packet$ERA_temp_anom, "ERA/temp-anom"))
  if("ERA/u-anom" %in% var) res <- rbind(res, col.shimmy(SOM_packet$ERA_uv_anom[,c(1:3)], "ERA/u-anom"))
  if("ERA/v-anom" %in% var) res <- rbind(res, col.shimmy(SOM_packet$ERA_uv_anom[,c(1:2,4)], "ERA/v-anom"))
  # Round x/y for better colum names
  res$x <- round(res$x,2) # Have to use second decimal place as first decimal rounding creates possible duplicate entries
  res$y <- round(res$y,2)
  # Remove any possible duplicates. Should be none.
  # res <- res[, .(value = mean(value, na.rm = TRUE)),
               # by = .(x,y, var)]
  res$coords <- paste0(res$x, "_" , res$y, "_", res$var)
  res_wide <- t(res$value)
  colnames(res_wide) <- res$coords
  return(res_wide)
}

# Load the synoptic data for all events
system.time(BRAN_temp <- ddply(event_idx, .(event), load.data.packet, var = "BRAN/temp", .parallel = T)) # 27 seconds




# 2. Run SOM and have a peak ----------------------------------------------

# Function for running SOMs
  # Currently choosing to run SOM on only one variable at once
# data_packet <- BRAN_temp # tester...
som.var <- function(data_packet){
  # Create a scaled matrix for the SOM
    # Cancel out first column as this is the file name of the data packet
  data_packet_matrix <- as.matrix(scale(data_packet[,-1]))
  # Create the grid that the SOM will use to determine the number of nodes
  som_grid <- somgrid(xdim = 3, ydim = 3, topo = "hexagonal") # This grid looks for the best 9 nodes
  # Run the SOM
  som_model <- som(data_packet_matrix,
                               grid = som_grid, 
                               rlen = 100, 
                               alpha = c(0.05,0.01), 
                               n.hood = "circular",
                               keep.data = TRUE )
  return(som_model)
}

# Run the SOMs
system.time(som_BRAN_temp <- som.var(BRAN_temp)) # 62 seconds







# Pick which data frame to visualise
som_model <- som_BRAN_temp
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
# plot(som_model, type = "codes")

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
# plot(som_model, type = "codes", bgcol = som_cluster, main = "Clusters")
# add.cluster.boundaries(som_model, som_cluster)


# 3. Unscale SOM results for plotting -------------------------------------

# The unpacking function
# data_packet <- BRAN_temp; som_output <- som_BRAN_temp # tester...
som.unpack <- function(data_packet, som_output){
  # Melt data_packet
  data_packet$event <- sapply(strsplit(basename(as.character(data_packet$event)), ".Rdata"),  "[[", 1)
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
  return(var_unscaled)
}

# Unpack the results for plotting
system.time(res_BRAN_temp <- som.unpack(BRAN_temp, som_BRAN_temp)) ## 13 seconds

# Recalculate site clustering ... for now
# data_packet <- BRAN_temp; som_output <- som_BRAN_temp # tester...
event.node <- function(data_packet, som_output){
  event_node <- data.frame(event = sapply(strsplit(basename(as.character(data_packet$event)), ".Rdata"),  "[[", 1),
                           node = som_BRAN_temp$unit.classif)
  node_count <- as.data.frame(table(event_node$node))
  node_count
  # event_node$site <- sapply(strsplit(as.character(event_node$event), "_"), "[[", 1)
  event_node <- event_node %>%
    group_by(node) %>% 
    mutate(count = node_count$Freq[as.integer(node_count$Var1) == node][1]) %>% 
    mutate(site = sapply(strsplit(as.character(event), "_"), "[[", 1)) %>% 
    group_by(site) %>% 
    mutate(lon = SACTN_site_list$lon[as.character(SACTN_site_list$site) == site][1]) %>% 
    mutate(lat = SACTN_site_list$lat[as.character(SACTN_site_list$site) == site][1])
  return(event_node)
}

# Plot of site clustering
event_node <- event.node(BRAN_temp, som_BRAN_temp)

# Test plot
ggplot(data = var_unscaled, aes(x = x, y = y)) +
  geom_raster(aes(fill = value)) +
  geom_point(data = event_node, aes(x = lon, y = lat), shape = 21,  size = 3, alpha = 0.7, colour = "red", fill = "white") +
  scale_fill_viridis("BRAN\nSST", option = "D") +
  geom_label(data = event_node, aes(x = 25, y = -28, label = paste0("n = ", count,"/",length(node))), size = 3, label.padding = unit(0.5, "lines")) +
  # scale_colour_discrete("Node") +
  # facet_grid(var~node)
  facet_wrap(~node, ncol = 3)



# Seasons -----------------------------------------------------------------


