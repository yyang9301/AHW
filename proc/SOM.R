#############################################################################
###"proc/SOM.R"
## This script does:
# 1. Load required data
# 2. Run SOMs
# 3. Have a peak at the models
# 4. Unscale SOM results for plotting
# 5. Create figures
# 6. Additional analyses
## DEPENDS ON:
library(kohonen)
library(grid)
library(ggplot2)
library(gridExtra)
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
# BRAN SST only
# system.time(BRAN_temp <- ddply(event_idx, .(event), load.data.packet, var = "BRAN/temp", .parallel = T)) # 27 seconds
# All BRAN normal data
system.time(BRAN_norm <- ddply(event_idx, .(event), load.data.packet, var = c("BRAN/temp", "BRAN/u", "BRAN/v"), .parallel = T)) # 91 seconds
# All ERA normal data
system.time(ERA_norm <- ddply(event_idx, .(event), load.data.packet, var = c("ERA/temp", "ERA/u", "ERA/v"), .parallel = T)) # 7 seconds
# All BRAN anomaly data
system.time(BRAN_anom <- ddply(event_idx, .(event), load.data.packet, var = c("BRAN/temp-anom", "BRAN/u-anom", "BRAN/v-anom"), .parallel = T)) # 86 seconds
# All ERA anomaly data
system.time(ERA_anom <- ddply(event_idx, .(event), load.data.packet, var = c("ERA/temp-anom", "ERA/u-anom", "ERA/v-anom"), .parallel = T)) # 7 seconds

# Combine data frames for modeling
all_norm <- cbind(BRAN_norm, ERA_norm[,-1])
all_anom <- cbind(BRAN_anom, ERA_anom[,-1])


# 2. Run SOMs -------------------------------------------------------------

# Function for running SOMs
  # Currently choosing to run SOM on only one variable at once
# data_packet <- BRAN_temp # tester...
som.var <- function(data_packet, xdim = 3, ydim = 3){
  # Create a scaled matrix for the SOM
    # Cancel out first column as this is the file name of the data packet
  data_packet_matrix <- as.matrix(scale(data_packet[,-1]))
  # Create the grid that the SOM will use to determine the number of nodes
  som_grid <- somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal") # This grid looks for the best 9 nodes
  # Run the SOM
  som_model <- som(data_packet_matrix,
                               grid = som_grid, 
                               rlen = 100, # It appears that rlen = 40 may be sufficient
                               alpha = c(0.05,0.01), 
                               n.hood = "circular",
                               keep.data = TRUE )
  return(som_model)
}

### Run the SOMs
## 3x3
# Normal data
system.time(som_all_norm <- som.var(all_norm)) # 274 seconds
# Anomalies
system.time(som_all_anom <- som.var(all_anom)) # 270 seconds

## 2x1
# Normal data
system.time(som_all_norm <- som.var(all_norm, xdim = 2, ydim = 1)) # 55 seconds
# Anomalies
system.time(som_all_anom <- som.var(all_anom, xdim = 2, ydim = 1)) # 65 seconds

## 3x1
# Normal data
system.time(som_all_norm <- som.var(all_norm, xdim = 3, ydim = 1)) # 85 seconds
# Anomalies
system.time(som_all_anom <- som.var(all_anom, xdim = 3, ydim = 1)) #  seconds

## 2x2
# Normal data
system.time(som_all_norm <- som.var(all_norm, xdim = 2, ydim = 2)) # 117 seconds
# Anomalies
system.time(som_all_anom <- som.var(all_anom, xdim = 2, ydim = 2)) # 106 seconds

## 3x2
# Normal data
system.time(som_all_norm <- som.var(all_norm, xdim = 3, ydim = 2)) # 170 seconds
# Anomalies
system.time(som_all_anom <- som.var(all_anom, xdim = 3, ydim = 2)) # 169 seconds

# 3. Have a peak at the models --------------------------------------------

# Pick which data frame to visualise
# som_model <- som_BRAN_temp
# som_model <- som_all_norm
# som_model <- som_all_anom

# The different visualisations for quality control
# Training progress. How many iterations the model needs to git gud
# plot(som_model, type = "changes")
# Counts within nodes. How many synoptic states fall within each node
# plot(som_model, type = "counts", main = "Node Counts")
# Map quality
# plot(som_model, type = "quality", main = "Node Quality/Distance")
# Neighbour distances
# plot(som_model, type = "dist.neighbours", main = "SOM neighbour distances", palette.name = grey.colors)
# Code spread. This shows the values for each pixel as a line graph, so it doesn't look like much
  # NB: With massive vectors this may take a minute or so to render
# plot(som_model, type = "codes")

# The WCSS metric for different kmeans clustering
# codes <- som_model$codes
# wss <- (nrow(codes)-1)*sum(apply(codes,2,var))
# for (i in 2:nrow(codes)-1) wss[i] <- sum(kmeans(codes,
                                     # centers=i)$withinss)
# par(mar = c(5.1,4.1,4.1,2.1))
# plot(1:(nrow(codes)-1), wss, type = "b", xlab = "Kmeans Clusters",
     # ylab = "Within groups sum of squares", main = "Within cluster sum of squares (WCSS)")
# rm(codes) # Save on RAM...
# It appears that as few as three clusters are needed
# That is surprising...

# Form clusters on grid
# use hierarchical clustering to cluster the synoptic vectors
# som_cluster <- cutree(hclust(dist(som_model$codes)), 3)

# Show the map with different colours for every cluster						  
# plot(som_model, type = "mapping", bgcol = som_cluster, main = "Clusters")
# add.cluster.boundaries(som_model, som_cluster)

# Show the same plot with the synoptic vectors as well
  # NB: With massive vectors this may take a minute or so to render
# plot(som_model, type = "codes", bgcol = som_cluster, main = "Clusters")
# add.cluster.boundaries(som_model, som_cluster)


# 4. Unscale SOM results for plotting -------------------------------------

# The unpacking function
# data_packet <- BRAN_temp; som_output <- som_BRAN_temp # tester...
som.unpack <- function(data_packet, som_output){
  # Melt data_packet
  data_packet$event <- sapply(strsplit(basename(as.character(data_packet$event)), ".Rdata"),  "[[", 1)
  data_packet_long <- melt(data_packet, id = "event")
  # Determine which event goes in which node
  event_node <- data.frame(event = data_packet$event, node = som_output$unit.classif)
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
# system.time(res_BRAN_temp <- som.unpack(BRAN_temp, som_BRAN_temp)) # 13 seconds
system.time(res_all_norm <- som.unpack(all_norm, som_all_norm)) # 43 seconds
system.time(res_all_anom <- som.unpack(all_anom, som_all_anom)) # 43 seconds

# Season index
# summer <- seq()

# Recalculate site clustering ... for now
# data_packet <- BRAN_temp; som_output <- som_BRAN_temp # tester...
event.node <- function(data_packet, som_output){
  event_node <- data.frame(event = sapply(strsplit(basename(as.character(data_packet$event)), ".Rdata"),  "[[", 1),
                           node = som_output$unit.classif)
  node_count <- as.data.frame(table(event_node$node))
  
  ## Insert season finding bit here ##
  
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
# node_BRAN_temp <- event.node(BRAN_temp, som_BRAN_temp)
node_all_norm <- event.node(all_norm, som_all_norm)
node_all_anom <- event.node(all_anom, som_all_anom)


# 5. Create figures -------------------------------------------------------

# ## Create wind vector labels
# # Normal
# label_BRAN_norm <- data_frame(txt = "1.0 m/s\n",
#                               x = 36, y = -37, type = "SST + Current")
# label_ERA_norm <- data_frame(txt = "4.0 m/s\n",
#                             x = 36, y = -37)
# 
# # Anomaly
# label_BRAN_anom <- data_frame(txt = "1.0 m/s\n",
#                               x = 36, y = -37, type = "SST Anomaly + Current Anomaly")
# label_ERA_anom <- data_frame(txt = "4.0 m/s\n",
#                                  x = 36, y = -37)

## Create plots

# Load SA map data
load("graph/southern_africa_coast.RData") # Lowres
names(southern_africa_coast)[1] <- "lon"

# The lon/ lat ranges
wlon <- 10
elon <- 40
nlat <- -25
slat <- -40

sa_lons <- c(10, 40); sa_lats <- c(-40, -25)

# The plotting function
  # NB: This function requires input generated from 'all.panels()'
# data_temp <- res_BRAN_temp; data_uv <- res_BRAN_uv; data_node <- node_all_norm; vector_label <-  "1.0 m/s\n"; plot_title <- "SST + Current"; legend_title <- "SST"; viridis_col <- "D" # tester...
node.panels <- function(data_temp, data_uv, data_node, plot_title, legend_title, vector_label, viridis_col){
  np <- ggplot(data = data_temp, aes(x = x, y = y)) +
    geom_raster(aes(fill = value)) +
    geom_segment(data = data_uv, aes(x = x, y = y, xend = x + u, yend = y + v),
                 arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
    geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    
    ## Need to fix the UV vector label...
    # geom_label(data = label_dat, aes(x = x, y = y, label = txt), size = 5, label.padding = unit(0.5, "lines")) +
    # geom_segment(data = segment_dat, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_label(data = data_node, aes(x = 25, y = -28, label = paste0("n = ", count,"/",length(node))), size = 3, label.padding = unit(0.5, "lines")) +
    
    geom_point(data = data_node, aes(x = lon, y = lat), shape = 21,  size = 3, alpha = 0.7, colour = "red", fill = "white") +
    scale_x_continuous(limits = sa_lons, expand = c(0, 0), breaks = seq(15, 35, 5),
                       labels = scales::unit_format("°E", sep = "")) +
    scale_y_continuous(limits = sa_lats, expand = c(0, 0), breaks = seq(-35, -30, 5),
                       labels = c("35°S", "30°S")) +
    coord_fixed(xlim = c(10.5, 39.5), ylim = c(-39.5, -25.5), expand = F) +
    xlab("") + ylab("") + ggtitle(plot_title) +
    scale_fill_viridis(legend_title, option = viridis_col) +
    facet_wrap(~node, nrow = ceiling((length(unique(data_temp$node))/3))) +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          panel.background = element_rect(fill = "grey70"),
          panel.border = element_rect(fill = NA, colour = "black", size = 1),
          panel.grid.major = element_line(colour = "grey70"),
          panel.grid.minor = element_line(colour = "grey70"),
          legend.position = "right",
          legend.direction = "vertical",
          strip.text = element_text(size = 12),
          strip.background = element_rect(fill = NA),
          legend.key.height = unit(1.1, "cm"),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12))
  # np
  return(np)
}

# The function that puts it all together
# data_res <- res_all_norm # tester...
all.panels <- function(data_res){
  data_res$var <- as.factor(data_res$var)
  ## Separate BRAN from ERA and temp from uv
  # BRAN
  res_BRAN_temp <- data_res[data_res$var == levels(data_res$var)[1],]
  res_BRAN_uv <- data_res[data_res$var %in% levels(data_res$var)[2:3],]
  res_BRAN_uv <- dcast(res_BRAN_uv, x+y+node~var)
  colnames(res_BRAN_uv)[4:5] <- c("u", "v")
  lon_sub <- seq(10, 40, by = 0.5)
  lat_sub <- seq(-40, -15, by = 0.5)
  res_BRAN_uv <- res_BRAN_uv[(res_BRAN_uv$x %in% lon_sub & res_BRAN_uv$y %in% lat_sub),]
  
  # ERA
  res_ERA_temp <- data_res[data_res$var == levels(data_res$var)[4],]
  res_ERA_uv <- data_res[data_res$var %in% levels(data_res$var)[5:6],]
  res_ERA_uv <- dcast(res_ERA_uv, x+y+node~var)
  colnames(res_ERA_uv)[4:5] <- c("u", "v")
  res_ERA_uv$u <- res_ERA_uv$u/2
  res_ERA_uv$v <- res_ERA_uv$v/2
  lon_sub <- seq(10, 40, by = 1)
  lat_sub <- seq(-40, -15, by = 1)
  res_ERA_uv <- res_ERA_uv[(res_ERA_uv$x %in% lon_sub & res_ERA_uv$y %in% lat_sub),]
  
  # Determine node to use
  if(sum(res_BRAN_temp$value) >= 500000){
    node_data <- node_all_norm
    plot_title_BRAN = "SST + Current"
    plot_title_ERA = "Air Temp + Wind"
    legend_title = "Temp.\n(°C)"
    data_type = "norm"
  } 
  if(sum(res_BRAN_temp$value) < 500000){
    node_data <- node_all_anom
    plot_title_BRAN = "SST Anomaly + Current Anomaly"
    plot_title_ERA = "Air Temp Anomaly + Wind Anomaly"
    legend_title = "Anom.\n(°C)"
    data_type = "anom"
  } 
  
  ## The  panel figures
  # BRAN
  panels_BRAN <- node.panels(data_temp = res_BRAN_temp, data_uv = res_BRAN_uv, data_node = node_data,
                             plot_title = plot_title_BRAN, legend_title = legend_title, vector_label = "1.0 m/s\n", viridis_col = "D")
  # panels_BRAN
  # ERA
  panels_ERA <- node.panels(data_temp = res_ERA_temp, data_uv = res_ERA_uv, data_node = node_data,
                             plot_title = plot_title_ERA, legend_title = legend_title, vector_label = "4.0 m/s\n", viridis_col = "A")
  # panels_ERA
  
  ## Combine figures and save
  # Generate file name
  file_name <- paste0("graph/som/",data_type,"_",length(unique(res_BRAN_temp$node)), ".pdf")
  # The figure
  pdf(file_name, width = 10, height = 12, pointsize = 10) # Set PDF dimensions
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2,1)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(panels_BRAN, vp = vplayout(1,1))
  print(panels_ERA, vp = vplayout(2,1))
  dev.off()

}

# Run it
system.time(all.panels(res_all_norm)) # 14 seconds
system.time(all.panels(res_all_anom)) # 15 seconds


# 6. Additional analyses --------------------------------------------------

# Seasonality of events




