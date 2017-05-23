#############################################################################
###"func/som.func.R"
## This script does:
# 1. A load function for reanalysis data by variable
# 2. Function for calculating SOMs using PCI
# 3. Function for determining node indexes
# 4. Functions for unpacking som results
# 5. Functions for creating figures
# 6. Function for creating event metrics table
# 7. Function for melting rounding and re-casting data
# 8. Function for melting trimming and re-casting data 
# 9. Function for melting subsetting and re-casting data
# 10. Function for extracting only count of events per node
## DEPENDS ON:
# library(kohonen)
# library(SOMbrero)
library(yasomi)
# library(plyr)
# library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
# library(reshape2)
# library(lubridate)
# library(zoo)
library(doMC); registerDoMC(cores = 4)
## USED BY:
# "2.Model_fitting.R"
## CREATES:
# SOM results and analyses
#############################################################################


# 1. A load function for reanalysis data by variable ----------------------

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
  if("BRAN/temp" %in% var) res <- rbind(res, col.shimmy(data_packet$BRAN_temp, "BRAN/temp"))
  if("BRAN/u" %in% var) res <- rbind(res, col.shimmy(data_packet$BRAN_uv[,c(1:3)], "BRAN/u"))
  if("BRAN/v" %in% var) res <- rbind(res, col.shimmy(data_packet$BRAN_uv[,c(1:2,4)], "BRAN/v"))
  # Combine all BRAN anomaly data
  if("BRAN/temp-anom" %in% var) res <- rbind(res, col.shimmy(data_packet$BRAN_temp_anom, "BRAN/temp-anom"))
  if("BRAN/u-anom" %in% var) res <- rbind(res, col.shimmy(data_packet$BRAN_uv_anom[,c(1:3)], "BRAN/u-anom"))
  if("BRAN/v-anom" %in% var) res <- rbind(res, col.shimmy(data_packet$BRAN_uv_anom[,c(1:2,4)], "BRAN/v-anom"))
  # Combine all ERA data
  if("ERA/temp" %in% var) res <- rbind(res, col.shimmy(data_packet$ERA_temp, "ERA/temp"))
  if("ERA/u" %in% var) res <- rbind(res, col.shimmy(data_packet$ERA_uv[,c(1:3)], "ERA/u"))
  if("ERA/v" %in% var) res <- rbind(res, col.shimmy(data_packet$ERA_uv[,c(1:2,4)], "ERA/v"))
  # Combine all ERA anomaly data
  if("ERA/temp-anom" %in% var) res <- rbind(res, col.shimmy(data_packet$ERA_temp_anom, "ERA/temp-anom"))
  if("ERA/u-anom" %in% var) res <- rbind(res, col.shimmy(data_packet$ERA_uv_anom[,c(1:3)], "ERA/u-anom"))
  if("ERA/v-anom" %in% var) res <- rbind(res, col.shimmy(data_packet$ERA_uv_anom[,c(1:2,4)], "ERA/v-anom"))
  # Create coords column
  res$coords <- paste0(res$x, "_" , res$y, "_", res$var)
  # Transpose
  res_wide <- t(res$value)
  colnames(res_wide) <- res$coords
  return(res_wide)
}


# 2. Function for calculating SOMs using PCI ------------------------------

som.model.PCI <- function(data_packet, xdim = 3, ydim = 3){
  # Create a scaled matrix for the SOM
  # Cancel out first column as this is the file name of the data packet
  data_packet_matrix <- as.matrix(scale(data_packet[,-1]))
  
  # Create the grid that the SOM will use to determine the number of nodes
  som_grid <- somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal")
  
  # Run the SOM with PCI
  som_model <- batchsom(data_packet_matrix, 
                        somgrid = som_grid, 
                        init = "pca",
                        max.iter = 100)
  return(som_model)
}

# 3. Function for determining node indexes --------------------------------

event.node <- function(data_packet, som_output){
  event_node <- data.frame(event = sapply(strsplit(basename(as.character(data_packet$event)), ".Rdata"),  "[[", 1),
                            node = som_output$classif)
  node_count <- as.data.frame(table(event_node$node))
  event_node <- event_node %>%
    group_by(node) %>% 
    mutate(count = node_count$Freq[as.integer(node_count$Var1) == node[1]]) %>% 
    mutate(site = sapply(strsplit(as.character(event), "_"), "[[", 1)) %>% 
    mutate(event_no = as.integer(sapply(strsplit(as.character(event), "_"), "[[", 2))) %>% 
    group_by(site) %>% 
    mutate(lon = SACTN_site_list$lon[as.character(SACTN_site_list$site) == site[1]]) %>% 
    mutate(lat = SACTN_site_list$lat[as.character(SACTN_site_list$site) == site[1]]) %>%
    group_by(event) %>% 
    mutate(season = as.factor(SACTN_events$season[SACTN_events$event == event[1]]))
  event_node <- as.data.frame(event_node)
  return(event_node)
}


# 4. Functions for unpacking som results ----------------------------------

# Create mean results from initial data frame based on node clustering
som.unpack.mean <- function(data_packet, som_output){
  # Melt data_packet
  data_packet$event <- sapply(strsplit(basename(as.character(data_packet$event)), ".Rdata"),  "[[", 1)
  data_packet$node <- som_output$classif
  data_packet_long <- melt(data_packet, id = c("event", "node"))
  # Determine which event goes in which node
  # event_node <- data.frame(event = data_packet$event, node = som_output$classif)
  # data_packet_long <- data_packet_long %>%
  #   group_by(event) %>%
  #   mutate(node = event_node$node[event_node$event == event][1])
  data_packet_long <- data.table(data_packet_long)
  # Create the mean values that serve as the unscaled results from the SOM
  var_unscaled <- data_packet_long[, .(value = mean(value, na.rm = TRUE)),
                                   by = .(node, variable)]
  var_unscaled$x <- as.numeric(sapply(strsplit(as.character(var_unscaled$variable), "_"), "[[", 1))
  var_unscaled$y <- as.numeric(sapply(strsplit(as.character(var_unscaled$variable), "_"), "[[", 2))
  var_unscaled$var <- sapply(strsplit(as.character(var_unscaled$variable), "_"), "[[", 3)
  var_unscaled$variable <- NULL
  var_unscaled <- var_unscaled[order(var_unscaled$node, var_unscaled$x, var_unscaled$y),]
  return(var_unscaled)
}


# Rescale the actual som results
# This is not used as it is prone to creating less even results
som.unpack.rescale <- function(data_packet, som_output){
  # Create matrices for rescaling som results
  matrix1 <- data_packet[,-1]
  matrix2 <- as.data.frame(som_output$prototypes)
  # Calculate range for rescale
  min_max <- t(data.frame(min_col = apply(matrix1, 2, min), max_col = apply(matrix1, 2, max)))
  # center <- attributes(som_output$data)
  # center <- t(data.frame(center = center$`scaled:center`))
  matrix3 <- rbind(min_max, center, matrix2)
  # A little wrapper function
  # x <- matrix3[,1]
  rescale.som <- function(x){
    rescale(x[3:length(x)], to = c(x[1], x[2]))
  }
  # Run it
  df1 <- as.data.frame(apply(matrix3, 2, rescale.som))
  df1$node <- rownames(df1)
  # Melt and separate out necessary columns
  var_unscaled <- melt(df1, id = "node")
  var_unscaled$x <- as.numeric(sapply(strsplit(as.character(var_unscaled$variable), "_"), "[[", 1))
  var_unscaled$y <- as.numeric(sapply(strsplit(as.character(var_unscaled$variable), "_"), "[[", 2))
  var_unscaled$var <- sapply(strsplit(as.character(var_unscaled$variable), "_"), "[[", 3)
  var_unscaled$variable <- NULL
  return(var_unscaled)
}


# 5. Functions for creating figures ---------------------------------------

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
load("~/AHW/graph/southern_africa_coast.Rdata") # Lowres
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
node.panels <- function(data_temp, data_uv, data_node, plot_title, legend_title, vector_label, viridis_col, BRAN = TRUE){
  np <- ggplot(data = data_temp, aes(x = x, y = y)) +
    geom_raster(aes(fill = value)) +
    geom_segment(data = data_uv, aes(x = x, y = y, xend = x + u, yend = y + v),
                 arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.2) +
    geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
                 fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
    
    ### Need to fix the UV vector label...
    # geom_label(data = label_dat, aes(x = x, y = y, label = txt), size = 5, label.padding = unit(0.5, "lines")) +
    # geom_segment(data = segment_dat, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_label(data = data_node, aes(x = 25, y = -28, label = paste0("n = ", count,"/",length(node))), size = 3, label.padding = unit(0.5, "lines")) +
    ###
    
    ### Different point options
    ## White points
    # geom_point(data = data_node, aes(x = lon, y = lat), shape = 21,  size = 3, alpha = 0.7, colour = "red", fill = "white") +
    ## Seasonal coloured points
    geom_point(data = data_node, aes(x = lon, y = lat, colour = season), shape = 19,  size = 3, alpha = 0.6) +
    scale_color_discrete("Season") +
    ###
    
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
  if(BRAN){
    np <- np + geom_polygon(data = southern_africa_coast, aes(x = lon, y = lat, group = group),
                   fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
      geom_label(data = data_node, aes(x = 25, y = -28, label = paste0("n = ", count,"/",length(node))), size = 3, label.padding = unit(0.5, "lines")) +
      geom_point(data = data_node, aes(x = lon, y = lat, colour = season), shape = 19,  size = 3, alpha = 0.6)
  }
  # np
  return(np)
}

# The function that puts it all together
# data_res <- node_means # tester...
# data_node <- node_all_anom
all.panels <- function(data_res, data_node){
  
  # Prep the data
  data_res$var <- as.factor(data_res$var)
  
  ## Separate BRAN from ERA and temp from uv
  # BRAN
  res_BRAN_temp <- data_res[data_res$var == levels(data_res$var)[1],]
  res_BRAN_uv <- data_res[data_res$var %in% levels(data_res$var)[2:3],]
  res_BRAN_uv <- dcast(res_BRAN_uv, x+y+node~var)
  colnames(res_BRAN_uv)[4:5] <- c("u", "v")
  # lon_sub <- seq(10, 40, by = 0.5)
  # lat_sub <- seq(-40, -15, by = 0.5)
  # res_BRAN_uv <- res_BRAN_uv[(res_BRAN_uv$x %in% lon_sub & res_BRAN_uv$y %in% lat_sub),]
  
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
  
  # node_data <- node_all_anom
  plot_title_BRAN = "SST Anomaly + Current Anomaly"
  plot_title_ERA = "Air Temp Anomaly + Wind Anomaly"
  legend_title = "Anom.\n(°C)"
  data_type = "anom"
  
  ## The  panel figures
  # BRAN
  panels_BRAN <- node.panels(data_temp = res_BRAN_temp, data_uv = res_BRAN_uv, data_node = data_node, BRAN = TRUE,
                             plot_title = plot_title_BRAN, legend_title = legend_title, vector_label = "1.0 m/s\n", viridis_col = "D")
  # panels_BRAN
  # ERA
  panels_ERA <- node.panels(data_temp = res_ERA_temp, data_uv = res_ERA_uv, data_node = data_node, BRAN = FALSE,
                            plot_title = plot_title_ERA, legend_title = legend_title, vector_label = "4.0 m/s\n", viridis_col = "A")
  # panels_ERA
  
  # Create grid
  # print(grid.arrange(panels_BRAN, panels_ERA, layout_matrix = cbind(c(1,2), c(1,2))))
  # ggsave("graph/SOM_nodes.pdf", height = 18, width = 10)
  
  ## Combine figures and save
  # Generate file name
  file_name <- paste0("graph/SOM_nodes.pdf")
  # The figure
  pdf(file_name, width = 10, height = 12, pointsize = 10) # Set PDF dimensions
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2,1)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  print(panels_BRAN, vp = vplayout(1,1))
  print(panels_ERA, vp = vplayout(2,1))
  dev.off()
}


# 6. Function for creating event metrics table ----------------------------

# df <- node_all_anom
node.summary.metrics <- function(data_node, data_event){
  df_1 <- merge(data_node, data_event, by = c("event", "site", "season", "event_no"))
  df_2 <- df_1 %>% 
    group_by(node) %>% 
    summarise(count = count[1],
              summer = length(season[as.character(season) == "summer"]),
              autumn = length(season[as.character(season) == "autumn"]),
              winter = length(season[as.character(season) == "winter"]),
              spring = length(season[as.character(season) == "spring"]),
              west = length(season[as.character(coast) == "wc"]),
              south = length(season[as.character(coast) == "sc"]),
              east = length(season[as.character(coast) == "ec"]),
              duration_min = min(duration, na.rm = T),
              duration_mean = round(mean(duration, na.rm = T),1),
              duration_max = max(duration, na.rm = T),
              int_cum_min = round(min(int_cum, na.rm = T),2),
              int_cum_mean = round(mean(int_cum, na.rm = T),3),
              int_cum_max = round(max(int_cum, na.rm = T),2),
              int_max_min = round(min(int_max, na.rm = T),2),
              int_max_mean = round(mean(int_max, na.rm = T),3),
              int_max_max = round(max(int_max, na.rm = T),2))
  df_3 <- df_1 %>% 
    summarise(count = length(count),
              summer = length(season[as.character(season) == "summer"]),
              autumn = length(season[as.character(season) == "autumn"]),
              winter = length(season[as.character(season) == "winter"]),
              spring = length(season[as.character(season) == "spring"]),
              west = length(season[as.character(coast) == "wc"]),
              south = length(season[as.character(coast) == "sc"]),
              east = length(season[as.character(coast) == "ec"]),
              duration_min = min(duration, na.rm = T),
              duration_mean = round(mean(duration, na.rm = T),1),
              duration_max = max(duration, na.rm = T),
              int_cum_min = round(min(int_cum, na.rm = T),2),
              int_cum_mean = round(mean(int_cum, na.rm = T),3),
              int_cum_max = round(max(int_cum, na.rm = T),2),
              int_max_min = round(min(int_max, na.rm = T),2),
              int_max_mean = round(mean(int_max, na.rm = T),3),
              int_max_max = round(max(int_max, na.rm = T),2))
  df_4 <- data.frame(node = NA)
  df_3 <- cbind(df_4, df_3)
  df_5 <- rbind(df_2, df_3)
  return(df_5)
}


# 7. Function for melting rounding and re-casting data --------------------

# df <- all_anom
# resolution <- 0.5
synoptic.round <- function(df, resolution = 0.5){
  # Melt ans separate out columns
  df_1 <- melt(df, id.vars = "event")
  df_1$x <- as.numeric(sapply(strsplit(as.character(df_1$variable), "_"), "[[", 1))
  df_1$y <- as.numeric(sapply(strsplit(as.character(df_1$variable), "_"), "[[", 2))
  df_1$variable <- sapply(strsplit(as.character(df_1$variable), "_"), "[[", 3)
  # Reduce resolution
  df_resolution <- df_1 %>% 
    mutate(x = round_any(x, resolution)) %>% 
    mutate(y = round_any(y, resolution))
  df_resolution <- data.table(df_resolution)
  df_resolution <- df_resolution[, .(value = mean(value, na.rm = TRUE)),
                     by = .(x, y, variable, event)]
  df_resolution$index <- paste0(df_resolution$x,"_",df_resolution$y,"_",df_resolution$variable)
  # Recast to wide format for clustering
  df_resolution_wide <- dcast(df_resolution, event~index, value.var = "value")
  return(df_resolution_wide)
}


# 8. Function for melting trimming and re-casting data --------------------
# df <- all_anom_0.5
# trim <- 1
synoptic.trim <- function(df, trim = 1){
  # Melt ans separate out columns
  df_1 <- melt(df, id.vars = "event")
  df_1$x <- as.numeric(sapply(strsplit(as.character(df_1$variable), "_"), "[[", 1))
  df_1$y <- as.numeric(sapply(strsplit(as.character(df_1$variable), "_"), "[[", 2))
  df_1$variable <- sapply(strsplit(as.character(df_1$variable), "_"), "[[", 3)
  # Reduce area extent
  lon <- range(df_1$x)
  lat <- range(df_1$y)
  df_trim <- filter(df_1, x >= lon[1]+trim, x <= lon[2]-trim, y >= lat[1]+trim)
  df_trim$index <- paste0(df_trim$x,"_",df_trim$y,"_",df_trim$variable)
  # Recast to wide format for clustering
  df_trim_wide <- dcast(df_trim, event~index, value.var = "value")
  return(df_trim_wide)
}


# 9. Function for melting subsetting and re-casting data ------------------
# df <- all_anom_0.5
# subvar <- "BRAN"
synoptic.sub <- function(df, subvar){
  # Melt ans separate out columns
  df_1 <- melt(df, id.vars = "event")
  df_1$x <- as.numeric(sapply(strsplit(as.character(df_1$variable), "_"), "[[", 1))
  df_1$y <- as.numeric(sapply(strsplit(as.character(df_1$variable), "_"), "[[", 2))
  df_1$variable <- sapply(strsplit(as.character(df_1$variable), "_"), "[[", 3)
  # Subset by the chosen variable
  df_sub <- df_1[grepl(subvar, df_1$variable),]
  df_sub$index <- paste0(df_sub$x,"_",df_sub$y,"_",df_sub$variable)
  # Recast to wide format for clustering
  df_sub_wide <- dcast(df_sub, event~index, value.var = "value")
  return(df_sub_wide)
}


# 10. Function for extracting only count of events per node ---------------

# This function runs a SOM with PCI and then counts the number of events in each node
# It returns only one column of data
# It is intended for use comparing variables within the dataset
node.count <- function(data_packet, column_name, xdim = 3, ydim = 3, kohonen = FALSE){
  som_model <- som.model.PCI(data_packet, xdim = xdim, ydim = ydim)
  res <- as.data.frame(as.matrix(table(event.node(data_packet, som_model, kohonen)[2])))
  colnames(res) <- column_name
  return(res)
}

