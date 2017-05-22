#############################################################################
## This script does:
# 1. Reads SAWS site list and MHW site list;
# 2. Extracts and saves SAWS station closest to MHW stations;
# 3. Creates map to show station locations
#############################################################################

#############################################################################
## DEPENDS ON:
require(FNN); require(ggplot2)
source("func/scaleBarFunc.R")

require(plyr); require(zoo); require(lubridate); require(reshape2); require(maptools); require(sp); require(geosphere); require(PBSmapping); require(mapproj); require(ggplot2); require(gridExtra); require(grid); require(viridis); require(scales)
source("func/earthdist.R"); source("func/metaTemp.R"); source("setupParams/theme.R"); source("func/seq.sites.R")
# "data/insituDaily_v3.4.RData"
#############################################################################

#############################################################################
## USED BY:
# 
#############################################################################

#############################################################################
## CREATES:
# "SAWS_sites.csv"
# "SAWS_sites.pdf"
#############################################################################

#############################################################################
# 1. Reads SAWS cropped site list and MHW site list
SAWS <- read.csv("data/SAWS_meta_data_cropped.csv")
load("~/MHW/data/metaData2.Rdata"); MHW <- metaData2
MHW <- MHW[1:21,]

#############################################################################
# 2. Extracts and saves SAWS station closest to MHW stations

# Extract sites
sites_idx1 <- as.data.frame(knnx.index(as.matrix(SAWS[,2:3]), as.matrix(MHW[,5:4]), k = 1))
sites_idx2 <- SAWS[sites_idx1$V1,]
# sites_idx3 <- sites_idx2[!duplicated(sites_idx2$ID),] # Remove duplicates
sites_idx3 <- data.frame(site = MHW$site, sites_idx2)

## TO DO ##
# Calculate distances between sites here

# Save as .csv
row.names(sites_idx3) <- NULL
write.csv(sites_idx3, file = "data/SACTN_nearest_SAWS_sites", row.names = F)


#############################################################################
# 3. Creates map to show station locations

# Load necessary bits
load("~/Documents/Project/SpeciesGuide/graph/south_africa_coast.RData")
names(south_africa_coast)[1] <- "lon"
load("~/Documents/Project/SpeciesGuide/graph/africa_borders.Rdata")
load("~/Documents/Project/SpeciesGuide/graph/sa_provinces_new.RData")
sa_provinces_new$index <- 1:12 # Reduce it by 92%
sa_provinces_new <- droplevels(subset(sa_provinces_new, index == 1))

# South Africa lon/ lats
latSA <- c(-35.5, -26); lonSA <- c(14, 34)

# Create background map
map <- ggplot() + coord_equal() + theme_bw() +
  # Landmass
  geom_polygon(data = south_africa_coast, aes(x = lon, y = lat, group = group), 
               colour = NA, fill = "grey80") +
  # Province borders
  geom_path(data = sa_provinces_new, aes(x = long, y = lat, group = group),
            size = 0.5, colour = "grey50") +
  # International borders
  geom_path(data = africa_borders, aes(x = lon, y = lat, group = group), 
            size = 1.0, colour = "black") +
  # Thick coastal border
  geom_polygon(data = south_africa_coast, aes(x = lon, y = lat, group = group),
               size = 1.0, colour = "black", fill = NA) +
  scaleBar(lon = 27, lat = -36, distanceLon = 200, distanceLat = 20, distanceLegend = 40, dist.unit = "km",
           arrow.length = 90, arrow.distance = 60, arrow.North.size = 5) +
  # Map plotting limits
  coord_cartesian(xlim = lonSA, ylim = c(-27, -37))

# Sites first extracted
SAWSsites <- map +
  geom_point(data = SAWS, aes(x = lon, y = lat), colour = "black", size = 2, shape = 4, alpha = 0.8) +
  geom_point(data = MHW, aes(x = lon, y = lat), colour = "blue", size = 4, shape = 16, alpha = 0.8) +
  geom_point(data = sites_idx3, aes(x = lon, y = lat), colour = "green", size = 4, shape = 15, alpha = 0.8)
ggsave("~/AHW/SAWSsites.pdf", width = 16, height = 10)

# Show only station IDs
SAWSstations <- map +
  geom_point(data = MHW, aes(x = lon, y = lat), colour = "blue", size = 4, shape = 16, alpha = 0.3) +
  geom_point(data = SAWS, aes(x = lon, y = lat), colour = "black", size = 2, shape = 16, alpha = 0.5) +
  geom_text(data = SAWS, aes(label = ID, x = lon, y = lat), colour = "black", size = 2, alpha = 0.8) +
ggsave("~/AHW/SAWSstations.pdf", width = 24, height = 14)

# Only False Bay
FB <- map +
  geom_point(data = MHW, aes(x = lon, y = lat), colour = "blue", size = 4, shape = 16, alpha = 0.3) +
  geom_point(data = SAWS, aes(x = lon, y = lat), colour = "black", size = 2, shape = 16, alpha = 0.5) +
  geom_text(data = SAWS, aes(label = ID, x = lon, y = lat), colour = "black", size = 2, alpha = 0.8) +
  # Map plotting limits
  coord_cartesian(xlim = c(18.25, 19), ylim = c(-33.7, -34.5))
ggsave("~/AHW/SAWSfalseBay.pdf", width = 10, height = 6)

# Only Algoa Bay
AB <- map +
  geom_point(data = MHW, aes(x = lon, y = lat), colour = "blue", size = 4, shape = 16, alpha = 0.3) +
  geom_point(data = SAWS, aes(x = lon, y = lat), colour = "black", size = 2, shape = 16, alpha = 0.5) +
  geom_text(data = SAWS, aes(label = ID, x = lon, y = lat), colour = "black", size = 2, alpha = 0.8) +
  # Map plotting limits
  coord_cartesian(xlim = c(25.4, 27), ylim = c(-33.5, -34.25))
ggsave("~/AHW/SAWSalgoaBay.pdf", width = 10, height = 6)

# Only Hamburg
HB <- map +
  geom_point(data = MHW, aes(x = lon, y = lat), colour = "blue", size = 4, shape = 16, alpha = 0.3) +
  geom_point(data = SAWS, aes(x = lon, y = lat), colour = "black", size = 2, shape = 16, alpha = 0.5) +
  geom_text(data = SAWS, aes(label = ID, x = lon, y = lat), colour = "black", size = 2, alpha = 0.8) +
  # Map plotting limits
  coord_cartesian(xlim = c(27, 28.5), ylim = c(-32, -34))
ggsave("~/AHW/SAWShamburg.pdf", width = 10, height = 6)

## Using these maps a list of appropriate sites was compiled manually
SAWS2 <- read.csv("~/AHW/SAWS_stations.csv")

# The manually chosen sites
manualSites <- map +
  geom_point(data = SAWS, aes(x = lon, y = lat), colour = "black", size = 2, shape = 4, alpha = 0.8) +
  geom_point(data = MHW, aes(x = lon, y = lat), colour = "blue", size = 4, shape = 16, alpha = 0.8) +
  geom_point(data = SAWS2, aes(x = lon, y = lat), colour = "green", size = 4, shape = 15, alpha = 0.8)
ggsave("~/AHW/SAWSstationsManual.pdf", width = 16, height = 10)
  