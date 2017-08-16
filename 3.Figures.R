###########################################################################
### "3.Figures.R"
## This script creates the figures for the paper and supplemental
# 1. Load all libraries and functions used in this script
# 2. Create synoptic figure for each event
# 3. Create synoptic figure showing SOM nodes
# 4. Create lolliplots for the SOM nodes
# 5. Create dendrogram for HCA results
# 6. Create ordiplot for MDS results
# 7. Create map of study area
#############################################################################


# 1. Load all libraries and functions used in this script -----------------
source("func/synoptic.func.R")
source("func/som.func.R")
source("func/scale.bar.func.R")
library(vegan)
library(ggdendro)
library(broom)


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


# 3. Create synoptic figure showing SOM nodes -----------------------------

load("data/node_means.Rdata")
load("data/node_all_anom.Rdata")
all.panels(node_means, node_all_anom)


# 4. Create lolliplots for the SOM nodes ----------------------------------

# Load data for figure
load("data/SACTN/SACTN_events.Rdata")
load("data/node_all_anom.Rdata")

# Merge into one dataframe
node_all <- merge(node_all_anom, SACTN_events, by = c("event", "site", "season", "event_no"))

# Calculate mean and median per node for plotting
node_h_lines <- node_all %>% 
  group_by(node) %>% 
  summarise(mean_int_cum = mean(int_cum, na.rm = T),
            median_int_cum = median(int_cum, na.rm = T))

# Create the figure
ggplot(data = node_all, aes(x = date_start, y = int_cum)) +
  geom_lolli() +
  geom_point(aes(colour = season)) +
  geom_label(aes(x = as.Date("2005-01-01"), y = 580, label = paste0("n = ", count,"/",length(node))), 
             size = 3, label.padding = unit(0.5, "lines")) +
  geom_hline(data = node_h_lines, aes(yintercept = mean_int_cum), linetype = "dashed") +
  geom_hline(data = node_h_lines, aes(yintercept = median_int_cum), linetype = "dotted") +
  facet_wrap(~node) +
  labs(x = "", y = "Cummulative intensity (°C·days)", colour = "Season") +
  theme_grey() +
  # scale_y_continuous(expand = c(0, 100)) +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"))
ggsave("graph/SOM_lolli.pdf", height = 9, width = 10)


# 5. Create dendrogram for HCA results ------------------------------------

# Load data
load("data/all_anom_hclust.Rdata")
load("data/all_anom_env.Rdata")

# Prep dendrogram
dhc <- as.dendrogram(all_anom_hclust)
ddata <- dendro_data(dhc, type = "rectangle")
label_data <- label(ddata)
label_data$season <- all_anom_env$season[as.numeric(as.character(label_data$label))]
label_data$type <- all_anom_env$type[as.numeric(as.character(label_data$label))]

# Plot the dendrogram
ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_point(data = label_data, aes(x = x, y = y, shape = type, colour = season)) +
  scale_shape_manual(values = c(1, 16)) +
  theme_grey() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"))
ggsave("graph/HCA.pdf", height = 9, width = 9)


# 6. Create ordiplot for MDS results --------------------------------------

# Load data
load("data/all_anom_MDS.Rdata")
load("data/all_anom_env.Rdata")
load("data/SACTN/SACTN_events.Rdata")
SACTN_events$type <- NULL
# load("data/node_means.Rdata")
# load("data/node_all_anom.Rdata")

## Fit environmental variables
ord_fit <- envfit(all_anom_MDS ~ season + type, data = all_anom_env)
# ord_fit
ord_fit_df <- as.data.frame(ord_fit$factors$centroids)
ord_fit_df$factors <- c("autumn", "spring", "summer", "winter", "clim", "MHW")

# Merge event values
# SACTN_events_sub <- SACTN_events[,c(1,2)]
all_anom_env <- left_join(all_anom_env, SACTN_events, by = c("event", "season"))

# Create MDS dataframe
# mds_df <- data.frame(all_anom_MDS$points, type = all_anom_env$type, 
#                      event = all_anom_env$event, season = all_anom_env$season)
mds_df <- data.frame(all_anom_MDS$points, all_anom_env)
mds_df$duration[is.na(mds_df$duration)] <- mean(mds_df$duration, na.rm = T)

# Plot the fits
ggplot(data = mds_df, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(colour = season, shape = type, size = duration)) +
  geom_segment(data = ord_fit_df, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(angle = 40, length = unit(0.2, "cm"), type = "open"), 
               alpha = 1, colour = "black", size = 0.5)  +
  geom_text(data = ord_fit_df, aes(label = factors, x = NMDS1, y = NMDS2), size = 8) +
  scale_shape_manual(name = "State", values = c(19, 15), labels = c("clim", "MHW")) +
  scale_colour_discrete(name = "Season") +
  scale_size_continuous(name = "Duration\n(days)", breaks = c(20, 70, 120, 170, 220)) +
  guides(colour = guide_legend(order = 1),
         shape = guide_legend(order = 2),
         size = guide_legend(override.aes = list(shape = 15), order = 3)) +
  # labs(size = "Duration") +
  theme_grey() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"))
ggsave("graph/MDS.pdf", height = 9, width = 12)

# Linear model: duration vs. ordination from centre point
lm_all_results <- mds_df %>%
  na.omit() %>%
  mutate(MDS2 = abs(MDS2)) %>% 
  select(-MDS1, -event, -season, -type, -coast, -site) %>%
  gather(key = group, 
         value = measurement,
         -MDS2) %>%
  group_by(group) %>% 
  nest() %>%
  mutate(model = purrr::map(data, ~lm(measurement ~ MDS2, data = .))) %>% 
  unnest(model %>% purrr::map(glance)) %>% 
  select(-data, -model)
lm_all_results[lm_all_results$adj.r.squared == max(lm_all_results$adj.r.squared, na.rm = T),]

# Visualization of the best linear model
ggplot(data = mds_df) +
  geom_smooth(aes(x = duration, y = abs(MDS2)), method = "lm", se = F) +
  theme_grey()


# 7. Create map of study area ---------------------------------------------

## Load data
# International borders
load("graph/africa_borders.Rdata")

# Hi-res bathy
load("graph/bathy.Rdata")

# Reanalysis data
load("data/ERA/ERA_temp_clim.Rdata")
load("data/ERA/ERA_u_clim.Rdata")
colnames(ERA_u_clim)[4] <- "val"
ERA_u_clim$variable <- "u"
load("data/ERA/ERA_v_clim.Rdata")
colnames(ERA_v_clim)[4] <- "val"
ERA_v_clim$variable <- "v"

# Remote data
load("data/OISST/OISST_temp_clim.Rdata")
load("data/AVISO/AVISO_u_clim.Rdata")
colnames(AVISO_u_clim)[4] <- "val"
AVISO_u_clim$variable <- "u"
load("data/AVISO/AVISO_v_clim.Rdata")
colnames(AVISO_v_clim)[4] <- "val"
AVISO_v_clim$variable <- "v"

# In situ time series locations
load("setupParams/SACTN_site_list.Rdata")
SACTN_site_list$order <- 1:nrow(SACTN_site_list)

# Create annual mean air-sea state
# sea_temp <- filter(OISST_temp_clim, date == "01-15") %>% 
sea_temp <- data.table::data.table(OISST_temp_clim)
sea_temp <- sea_temp[, .(temp = mean(temp, na.rm = TRUE)),
           by = .(x, y)] %>% 
  rename(lon = x, lat = y)
currents <- data.table::data.table(rbind(AVISO_u_clim, AVISO_v_clim))
currents <- currents[, .(val = mean(val, na.rm = TRUE)),
                     by = .(x, y, variable)] %>% 
  spread(key = variable, value = val) %>% 
  rename(lon = x, lat = y) %>% 
  mutate(arrow_size = ((abs(u*v)/ max(abs(u*v)))+0.2)/6)
air_temp <- data.table::data.table(ERA_temp_clim)
air_temp <- air_temp[, .(temp = mean(temp, na.rm = TRUE)),
                     by = .(x, y)] %>% 
  rename(lon = x, lat = y)
winds <- data.table::data.table(rbind(ERA_u_clim, ERA_v_clim))
winds <- winds[, .(val = mean(val, na.rm = TRUE)),
                     by = .(x, y, variable)] %>% 
  spread(key = variable, value = val) %>% 
  rename(lon = x, lat = y) %>% 
  mutate(arrow_size = ((abs(u*v)/ max(abs(u*v)))+0.3)/6)

# Reduce wind/ current vectors
lon_sub <- seq(10, 40, by = 1)
lat_sub <- seq(-40, -15, by = 1)
# currents <- currents[(currents$lon %in% lon_sub & currents$lat %in% lat_sub),]
winds <- winds[(winds$lon %in% lon_sub & winds$lat %in% lat_sub),]

# Establish the vector scalar for the currents
current_uv_scalar <- 2

# Establish the vector scalar for the wind
wind_uv_scalar <- 0.5

# Wind feature vector coordinates
cyc_atlantic <- data.frame(x = c(14.0, 16.1, 16.0), y = c(-36.0, -34.4, -32.1), xend = c(16.0, 16.1, 14.0), yend = c(-34.5, -32.2, -30.6))
cyc_indian <- data.frame(x = c(36.0, 33.9, 34.0), y = c(-31.5, -33.1, -35.4), xend = c(34.0, 33.9, 36.0), yend = c(-33.0, -35.3, -36.9))
westerlies <- data.frame(x = c(18.0, 21.1, 24.2), y = c(-38.0, -37.8, -37.8), xend = c(21.0, 24.1, 27.2), yend = c(-37.8, -37.8, -38.0))

# The top figure (sea)
fig_1_top <- ggplot(data = southern_africa_coast, aes(x = lon, y = lat)) +
  # The ocean temperature
  geom_raster(data = sea_temp, aes(fill = temp)) +
  # The bathymetry
  stat_contour(data = bathy[bathy$depth < -100 & bathy$depth > -300,], 
               aes(x = lon, y = lat, z = depth), alpha = 0.5,
               colour = "ivory", size = 0.5, binwidth = 200, na.rm = TRUE, show.legend = FALSE) +
  # The current vectors
  geom_segment(data = currents, aes(xend = lon + u * current_uv_scalar, yend = lat + v * current_uv_scalar),
               arrow = arrow(angle = 40, length = unit(currents$arrow_size, "cm"), type = "open"),
                             linejoin = "mitre", size = 0.4) +
  # The land mass
  geom_polygon(aes(group = group), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE) +
  geom_path(data = africa_borders, aes(group = group)) +
  # The legend for the vector length
  geom_label(aes(x = 37.0, y = -38.0, label = "1.0 m/s\n"), size = 3, label.padding = unit(0.5, "lines")) +
  geom_segment(aes(x = 36.0, y = -38.3, xend = 38.0, yend = -38.3), linejoin = "mitre",
               arrow = arrow(angle = 40, length = unit(0.2, "cm"), type = "open")) +
  # The in situ sites
  geom_point(data = SACTN_site_list, shape = 19,  size = 2.8, colour = "ivory") +
  geom_text(data = SACTN_site_list[-c(3,4,7:9,18,21,23:24),], aes(label = order), size = 1.9, colour = "red") +
  # Oceans
  annotate("text", label = "INDIAN\nOCEAN", x = 37.00, y = -34.0, size = 4.0, angle = 0, colour = "ivory") +
  
  annotate("text", label = "ATLANTIC\nOCEAN", x = 13.10, y = -34.0, size = 4.0, angle = 0, colour = "ivory") +
  # Benguela
  geom_segment(aes(x = 17.2, y = -32.6, xend = 15.2, yend = -29.5),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.5, colour = "ivory") +
  annotate("text", label = "Benguela", x = 16.0, y = -31.8, size = 3.5, angle = 298, colour = "ivory") +
  # Agulhas
  geom_segment(aes(x = 33, y = -29.5, xend = 29.8, yend = -33.0),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.5, colour = "ivory") +
  annotate("text", label = "Agulhas", x = 31.7, y = -31.7, size = 3.5, angle = 53, colour = "ivory") +
  # Agulhas Bank
  annotate("text", label = "Agulhas\nBank", x = 22.5, y = -35.5, size = 3.0, angle = 0, colour = "ivory") +
  # Cape Peninsula
  annotate("text", label = "Cape\nPeninsula", x = 17.2, y = -35, size = 3.0, angle = 0, colour = "ivory") +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(15, 35, 5),
                     labels = scales::unit_format("°E", sep = ""),
                     position = "top") +
  scale_y_continuous(breaks = seq(-35, -30, 5),
                     labels = c("35°S", "30°S")) +
  labs(x = NULL, y = NULL) +
  # Slightly shrink the plotting area
  coord_cartesian(xlim = c(10.5, 39.5), ylim = c(-39.5, -25.5), expand = F) +
  # Use viridis colour scheme
  scale_fill_viridis(name = "Temp.\n(°C)", option = "D", breaks = c(24, 20, 16)) +
  # Adjust the theme
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"))
# fig_1_top

# False Bay inset
fb_inset <- ggplot(data = sa_shore, aes(x = lon, y = lat)) +
  # The land mass
  geom_polygon(aes(group = PID),
               fill = "grey70", colour = NA, size = 0.5, show.legend = FALSE) +
  # The in situ sites
  geom_point(data = SACTN_site_list, shape = 1,  size = 3, colour = "black") +
  geom_text(data = SACTN_site_list[-6,], aes(label = order), size = 2.0, colour = "red") +
  # Text label
  geom_text(aes(x = 18.65, y = -34.25, label = "False\nBay"), size = 2.7) +
  # Control the x and y axes
  coord_cartesian(xlim = c(18.2, 19), ylim = c(-34.5, -33.8), expand = F) +
  scale_x_continuous(breaks = c(18.5), label = "18.5°E") +
  scale_y_continuous(breaks = c(-34.1), label = "34.1°S") +
  labs(x = NULL, y = NULL) +
  # Change the theme for cleaner over-plotting
  theme_bw() +
  theme(plot.background = element_blank(),
        axis.text = element_text(colour = "ivory"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.ticks = element_line(colour = "ivory"),
        panel.border = element_rect(colour = "ivory"),
        panel.grid = element_blank())
# fb_inset

# The bottom figure (air)
fig_1_bottom <- ggplot(data = southern_africa_coast, aes(x = lon, y = lat)) +
  # The ocean temperature
  geom_raster(data = air_temp, aes(fill = temp)) +
  # The land mass
  geom_polygon(aes(group = group), fill = NA, colour = "black", size = 0.5, show.legend = FALSE) +
  geom_path(data = africa_borders, aes(group = group)) +
  # The current vectors
  geom_segment(data = winds, aes(xend = lon + u * wind_uv_scalar, yend = lat + v * wind_uv_scalar),
               arrow = arrow(angle = 40, length = unit(winds$arrow_size, "cm"), type = "open"),
               linejoin = "mitre", size = 0.4) +
  # The legend for the vector length
  geom_label(aes(x = 37.0, y = -38.0, label = "4.0 m/s\n"), size = 3, label.padding = unit(0.5, "lines")) +
  geom_segment(aes(x = 36.0, y = -38.3, xend = 38.0, yend = -38.3), linejoin = "mitre",
               arrow = arrow(angle = 40, length = unit(0.2, "cm"), type = "open")) +
  # The coastal sections
  geom_spoke(aes(x = 18.46520, y = -34.31050, angle = 180, radius = -2), linetype = "dotted", colour = "ivory") +
  geom_spoke(aes(x = 18.46520, y = -34.31050, angle = 180, radius = 2), linetype = "dotted", colour = "ivory") +
  geom_spoke(aes(x = 27.48889, y = -33.28611, angle = 40, radius = -2), linetype = "dotted", colour = "ivory") +
  geom_spoke(aes(x = 27.48889, y = -33.28611, angle = 40, radius = 2), linetype = "dotted", colour = "ivory") +
  annotate("text", label = "West\nCoast", x = 19.5, y = -31.2, size = 3.0, angle = 0, colour = "ivory") +
  annotate("text", label = "South\nCoast", x = 23, y = -33.0, size = 3.0, angle = 0, colour = "ivory") +
  annotate("text", label = "East\nCoast", x = 28, y = -31, size = 3.0, angle = 0, colour = "ivory") +
  # South Atlantic Anticyclone
  annotate("text", label = "SOUTH\nATLANTIC\nANTICYCLONE", x = 13.5, y = -33.5, size = 3.0, angle = 0, colour = "ivory") +
  geom_curve(data = cyc_atlantic, aes(x = x, y = y, xend = xend, yend = yend), curvature = 0.2, colour = "ivory",
             arrow = arrow(angle = 40, type = "open", length = unit(0.25,"cm"))) +
  # South Indian Anticyclone
  annotate("text", label = "SOUTH\nINDIAN\nANTICYCLONE", x = 36.5, y = -34.0, size = 3.0, angle = 0, colour = "ivory") +
  geom_curve(data = cyc_indian, aes(x = x, y = y, xend = xend, yend = yend), curvature = 0.2, colour = "ivory",
             arrow = arrow(angle = 40, type = "open", length = unit(0.25,"cm"))) +
  # Westerlies
  annotate("text", label = "WESTERLIES", x = 22.5, y = -37.0, size = 3.0, angle = 0, colour = "ivory") +
  geom_curve(data = westerlies, aes(x = x, y = y, xend = xend, yend = yend), colour = "ivory",
             arrow = arrow(angle = 40, type = "open", length = unit(0.25,"cm")), curvature = -0.01) +
  # Improve on the x and y axis labels
  scale_x_continuous(breaks = seq(15, 35, 5),
                     labels = scales::unit_format("°E", sep = "")) +
  scale_y_continuous(breaks = seq(-35, -30, 5),
                     labels = c("35°S", "30°S")) +
  labs(x = NULL, y = NULL) +
  # Scale bar
  scaleBar(lon = 22.0, lat = -29.5, distanceLon = 200, distanceLat = 50, distanceLegend = 90, dist.unit = "km",
           arrow.length = 100, arrow.distance = 130, arrow.North.size = 3, 
           legend.colour = "ivory", arrow.colour = "ivory", N.colour = "ivory") +
  # Slightly shrink the plotting area
  coord_cartesian(xlim = c(10.5, 39.5), ylim = c(-39.5, -25.5), expand = F) +
  # Use viridis colour scheme
  scale_fill_viridis(name = "Temp.\n(°C)", option = "A", breaks = c(24, 20, 16)) +
  # Adjust the theme
  theme_bw() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"))
# fig_1_bottom

# Convert the figures to grobs
fig_1_top_grob <- ggplotGrob(fig_1_top)
fb_inset_grob <- ggplotGrob(fb_inset)
fig_1_bottom_grob <- ggplotGrob(fig_1_bottom)

# Stick them together
fig_1 <- ggplot() +
  # First set the x and y axis values so we know what the ranges are
  # in order to make it easier to place our facets
  coord_equal(xlim = c(1, 10), ylim = c(1, 10), expand = F) +
  # Then we place our facetsover one another using the coordinates we created
  annotation_custom(fig_1_top_grob,
                    xmin = 1, xmax = 10, ymin = 5.5, ymax = 10) +
  annotation_custom(fb_inset_grob,
                    xmin = 3.5, xmax = 5.5, ymin = 7.2, ymax = 8.8) +
  annotation_custom(fig_1_bottom_grob,
                    xmin = 1, xmax = 10, ymin = 1, ymax = 5.5)
# save
ggsave(plot = fig_1, filename = "graph/fig_1.pdf", height = 8, width = 8)

