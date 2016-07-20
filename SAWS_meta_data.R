# Load packages etc. ------------------------------------------------------
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(FNN)
source("func/metaTemp.R") # Afunction that calculates meta-deta/ stats of time series
source("func/scaleBarFunc.R") # A custom ggplot function that creates a very snazy scale bar
source("setupParams/themes.R") # The ggplot theme used for all figures


# Load data, corrrect date column and visualise data ----------------------

# The site list
sites <- read.csv("setupParams/SAWS_site_list.csv")

# All procurred SAWS data
SAWS_data <- read.csv("data/SAWS/SAWS_data.csv")

SAWS_data$date <- as.Date(as.character(SAWS_data$date), "%d-%B-%Y")

# ggplot(data = SAWS_data, aes(x = date)) +
#   geom_line(aes(y = mx), colour = "red") +
#   geom_line(aes(y = mn), colour = "blue") +
#   facet_wrap(~station)


# Correct mispelled site names -----------------------------------------
levels(SAWS_data$station)
SAWS_data$station[SAWS_data$station == "RIVERSDALE"] <- as.factor("RIVERSDAL")
SAWS_data <- droplevels(SAWS_data)
levels(SAWS_data$station)

# Clean out leading and trailing NA values --------------------------------
SAWS_data_clean <- ddply(SAWS_data, .(station), na.trim)

# ggplot(data = SAWS_data_clean, aes(x = date)) +
#   geom_line(aes(y = mx), colour = "red") +
#   geom_line(aes(y = mn), colour = "blue") +
#   facet_wrap(~station)


# Find row numbers of extra odd bits and remove them ----------------------
test <- droplevels(SAWS_data_clean[SAWS_data_clean$station == "EAST LONDON WO",])
test <- test[order(as.numeric(test$date)),] # Remove rows: 128932:129055 # 123 rows

test <- droplevels(SAWS_data_clean[SAWS_data_clean$station == "PORT ELIZABETH - WK",])
test <- test[order(as.numeric(test$date)),] # Remove rows: 280777:280885 # 108 rows

test <- droplevels(SAWS_data_clean[SAWS_data_clean$station == "S A ASTRONOMICAL OBSERVATORY",])
test <- test[order(as.numeric(test$date)),] # Remove rows: 333198:333288 # 90 rows

SAWS_data_clean <- SAWS_data_clean[-c(128932:129055,280777:280885,333198:333288),] # Remove 324 rows

ggplot(data = SAWS_data_clean, aes(x = date)) + bw_update +
  geom_line(aes(y = mx), colour = "red", alpha = 0.7) +
  geom_line(aes(y = mn), colour = "blue", alpha = 0.7) +
  facet_wrap(~station)
ggsave("graph/SAWS_data.pdf", height = 12, width = 16)

save(SAWS_data_clean, file = "data/SAWS/SAWS_data_clean.Rdata")
write.csv(SAWS_data_clean, file = "data/SAWS/SAWS_data_clean.csv", row.names = FALSE)

# Calculate meta-data -----------------------------------------------------

# Daily minimum stats
wide_mn <- dcast(SAWS_data_clean, date ~ station, value.var = "mn", mean)
wide_mn_zoo <- zoo(wide_mn[,2:length(colnames(wide_mn))], wide_mn$date)
data_summary_mn <- adply(wide_mn_zoo, 2, metaTemp)
names(data_summary_mn) <- c("station", "start_date", "end_date", "length", "temp_months_mn", "na_months_mn", "na_perc_mn", "mean_mn", "sd_mn", "min_mn", "max_mn")

# Daily maximum stats
wide_mx <- dcast(SAWS_data_clean, date ~ station, value.var = "mx", mean)
wide_mx_zoo <- zoo(wide_mx[,2:length(colnames(wide_mx))], wide_mx$date)
data_summary_mx <- adply(wide_mx_zoo, 2, metaTemp)
names(data_summary_mx) <- c("station", "start_date", "end_date", "length", "temp_months_mx", "na_months_mx", "na_perc_mx", "mean_mx", "sd_mx", "min_mx", "max_mx")
    ## Start and end dates for daily min and max were visually inspected and are the same

# Add lon/ lat columns
lat_lon_SAWS <- SAWS_data_clean[2:4] %>% 
  group_by(station) %>% 
  mutate(lon = mean(lon)) %>% 
  mutate(lat = mean(lat)) %>% 
  unique()

# The full meta-data frame
data_summary_SAWS <- cbind(data_summary_mx[c(1:4,7:11)], data_summary_mn[c(7:11)], lat_lon_SAWS[2:3])

# Reorder correctly
data_summary_SAWS  <- data_summary_SAWS [c(1,15:16,2:4,10,5,11,6,12,7,13,8,14,9)]

# Check out Kirstenbosch
kirstenbosch <- droplevels(SAWS_data_clean[SAWS_data_clean$station == "KIRSTENBOSCH",])
summary(lm(mn~date, kirstenbosch))
coef(lm(mn~date, kirstenbosch))[2]*3652.5
summary(lm(mx~date, kirstenbosch))
coef(lm(mx~date, kirstenbosch))[2]*3652.5


# Load and prep SACTN data ------------------------------------------------

load("data/SACTN/SACTNdaily_v4.0.Rdata")
SACTNdaily_v4.0$index <- paste(SACTNdaily_v4.0$site, SACTNdaily_v4.0$src, sep = "_")
SACTNdaily_v4.0$date <- as.Date(as.character(SACTNdaily_v4.0$date))

# Daily maximum stats
wide_SACTN <- dcast(SACTNdaily_v4.0, date ~ site+src, value.var = "temp", mean)
wide_SACTN_zoo <- zoo(wide_SACTN[,2:length(colnames(wide_SACTN))], wide_SACTN$date)
data_summary_SACTN <- adply(wide_SACTN_zoo, 2, metaTemp)
names(data_summary_SACTN) <- c("station", "start_date", "end_date", "length", "temp_months", "na_months", "na_perc", "mean", "sd", "min", "max")
## Start and end dates for daily min and max were visually inspected and are the same

# Load SACTN site list in order to add lon/ lat columns
sites_SACTN <- read.csv("setupParams/site_list_v4.0.csv")

# The full meta-data frame
data_summary_SACTN <- cbind(data_summary_SACTN[1], sites_SACTN[4:5], data_summary_SACTN[c(2:4,7:11)])

# Subset to match the MHW paper
load("~/MHW/data/metaData.Rdata")
MHW_sites <- metaData[metaData$`NA%` <= 10,]; rm(metaData)
MHW_sites <- MHW_sites[MHW_sites$length >= 3650,]
MHW_sites$index <- paste(MHW_sites$site, MHW_sites$src, sep = "_")

data_summary_SACTN <- data_summary_SACTN[data_summary_SACTN$station %in% MHW_sites$index,]


# Plot SACTN MHW time series ----------------------------------------------

MHW_daily <- SACTNdaily_v4.0[SACTNdaily_v4.0$index %in% MHW_sites$index,]

ggplot(data = MHW_daily, aes(x = date)) + bw_update +
  geom_line(aes(y = temp), colour = "darkslateblue", alpha = 0.8) +
  facet_wrap(~index)
ggsave("graph/SACTN_MHW_data.pdf", height = 12, width = 16)

# Save both data summaries ------------------------------------------------

write.csv(data_summary_SAWS, file = "data/SAWS_meta_data.csv", row.names = FALSE)
write.csv(data_summary_SACTN, file = "data/SACTN_meta_data.csv", row.names = FALSE)


# Load the spatial data used for the map --------------------

## Coastline of African Continent
load("graph/africa_coast.RData")

## Borders of African countries
load("graph/africa_borders.Rdata")

## Coastline of Southern Africa
load("graph/south_africa_coast.RData")

## Province borders
load("graph/sa_provinces_new.RData")
# Reduce prvonice border resolution
# sa_provinces_new$index <- 1:12 # Reduce it by 92%
# sa_provinces_new <- droplevels(subset(sa_provinces_new, index == 1))


# Create the map of Africa to be windowed ---------------------------------

# Africa + SA filled in
africa <- ggplot(africa_coast, aes(x = lon, y = lat)) + # Select the main dataset with which to graph
  theme_bw() + # Set the theme to black and white
  coord_equal() + # Forces lon/ lat to be equitably displayed so that the map isn't squished
  geom_polygon(aes(group = group), colour = "black", fill = "grey80") + # Draw the coast
  geom_polygon(data = sa_provinces_new, (aes(group = group))) +
  annotate("text", label = "Africa", x = 16.0, y = 15.0, size = 3) + # Change Africa label size and position
  theme(panel.border = element_rect(colour = "black", size = 0.4), # Creates border
        plot.background = element_blank(), # Makes background transparent
        axis.ticks = element_blank(), # Remove tick marks
        axis.text = element_blank(), # Remove lat/ lon numbers
        axis.title = element_blank(), # Remove lat/ lon labels
        panel.grid.major = element_blank(), # Removes major grid lines
        panel.grid.minor = element_blank()) +# Removes minor grid lines
  coord_map(xlim = c(-20, 53), ylim = c(-36, 38), projection = "mercator") # Constricts view to Africa
africa


# The map of southern Africa ----------------------------------------------

SA <- ggplot() + coord_equal() + theme_bw() +
  # Landmass
  geom_polygon(data = south_africa_coast, aes(x = lon, y = lat, group = group), 
               colour = NA, fill = "grey80") +
  # International borders
  geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), 
            size = 1.0, colour = "black") +
  # Thick coastal border
  geom_polygon(data = south_africa_coast, aes(x = lon, y = lat, group = group),
               size = 1.0, colour = "black", fill = NA) +
  # Scale bar
  scaleBar(lon = 29, lat = -35.8, distanceLon = 200, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
           arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
  # Map plotting limits
  coord_cartesian(xlim = c(14.5, 33.5), ylim = c(-27, -36)) +
  theme(axis.title = element_blank()) # Remove lat/ lon labels)
SA


# Add site list information -----------------------------------------------

map <- SA + 
  # Oceans
  annotate("text", label = "Indian\nOcean", x = 32.60, y = -32.9, size = 4.0, angle = 0) +
  annotate("text", label = "Atlantic\nOcean", x = 15.50, y = -32.9, size = 4.0, angle = 0) +
  # Benguela
  geom_segment(aes(x = 17.2, y = -32.6, xend = 15.2, yend = -29.5),
               arrow = arrow(length = unit(0.4, "cm")), size = 1.0, colour = "grey50") +
  annotate("text", label = "Benguela", x = 16.1, y = -31.5, size = 3.0, angle = 300) +
  # Agulhas
  geom_segment(aes(x = 33, y = -29.5, xend = 29.8, yend = -33.0),
               arrow = arrow(length = unit(0.4, "cm")), size = 1.0, colour = "grey50") +
  annotate("text", label = "Agulhas", x = 31.6, y = -31.6, size = 3.0, angle = 53) +
  # Landmass
  annotate("text", label = "South\nAfrica", x = 24.00, y = -31.00, size = 7, angle = 0) +
  # SAWS stations
  geom_point(data = data_summary_SAWS, aes(lon, lat), shape = 15, size = 3.5, colour = "black") +
  geom_point(data = data_summary_SAWS, aes(lon, lat), shape = 15, size = 2, colour = "cornsilk") +
  # SACTN stations
  geom_point(data = data_summary_SACTN, aes(lon, lat), shape = 17, size = 3.5, colour = "black") +
  geom_point(data = data_summary_SACTN, aes(lon, lat), shape = 17, size = 2, colour = "salmon") +
  # geom_point(data = data_summary, aes(lon, lat, size = length), colour = "white") +
  # scale_colour_grey(breaks = c("new", "old", "thermo"),
  #                   label = c("new", "old", "thermo")) +
  # guides(shape = guide_legend("Type", override.aes = list(size = 2.5, colour = "black"))) +
  labs(title = NULL, x = NULL, y = NULL) #+
# theme(legend.key = element_blank())
map


# Create the faceted map ---------------------------------------------------------

pdf("graph/SA_stations.pdf", width = 8, height = 5, pointsize = 6) # Set PDF dimensions
vp1 <- viewport(x = -0.00, y = 0.05, w = 0.25, h = 0.25, just = c("left", "bottom")) # Africa
vp2 <- viewport(x = 1.0, y = 1.0, w = 1.00, h = 1.00, just = c("right", "top"))  # South Africa
print(map, vp = vp2)
print(africa, vp = vp1)
dev.off()


# Calculate nearest neighbours --------------------------------------------

sites_idx <- as.data.frame(knnx.index(as.matrix(data_summary_SAWS[,3:2]), 
                                       as.matrix(data_summary_SACTN[,2:3]), k = 2))
sites_idx1 <- data.frame(SACTN = droplevels(data_summary_SACTN$station), 
                         SAWS = droplevels(data_summary_SAWS$station[sites_idx$V1]))
sites_idx2 <- data.frame(SACTN = droplevels(data_summary_SACTN$station), 
                         SAWS = droplevels(data_summary_SAWS$station[sites_idx$V2]))


# Calculate number of overlapping days ------------------------------------

calculate.overlap <- function(df){
  data1 <- droplevels(SACTNdaily_v4.0[SACTNdaily_v4.0$index == droplevels(df[1,1]),])
  data2 <- droplevels(SAWS_data_clean[as.character(SAWS_data_clean$station) == droplevels(df[1,2]),])
  data1 <- data1[data1$date %in% data2$date,]
  data2 <- data2[data2$date %in% data1$date,]
  overlap <- length(data2$date)
}

sites_idx1 <- adply(sites_idx1, 1, calculate.overlap)
mean(sites_idx1$V1) # Mean overlap is 5181

sites_idx2 <- adply(sites_idx2, 1, calculate.overlap)
mean(sites_idx2$V1) # Mean overlap is 3425

# Combine and save
overlap <- cbind(sites_idx1, sites_idx2[2:3])
colnames(overlap)[2:5] <- c("SAWS_1", "overlap_1", "SAWS_2", "overlap_2")

write.csv(overlap, file = "data/SACTN_SAWS_overlap.csv")
