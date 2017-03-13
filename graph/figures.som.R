#############################################################################
###"graph/figures.som.R"
## This script does:
# 1. Create list of events to be calculated
# 2. Mass produce the synoptic figures
## DEPENDS ON:
library(grid)
library(ggplot2)
library(gridExtra)
library(viridis)
source("func/som.func.R")
## USED BY:
# 
## CREATES:
# Figures similar to Eric's figures in the MHW atlas
#############################################################################


# 1. Create figures --------------------------------------------------------

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
  file_name <- paste0("graph/som/",data_type,"_",length(unique(res_BRAN_temp$node)),".pdf")
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
