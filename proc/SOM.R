#############################################################################
###"proc/SOM.R"
## This script does:

# 1. Load required data
# 2. The complete function for creating synoptic figures

## DEPENDS ON:

library(kohonen)
library(ggplot2)
# library(reshape2)
library(data.table)
library(doMC); registerDoMC(cores = 4)



## USED BY:
# 
## CREATES:
# 
#############################################################################


## Initial tutorial from: http://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/


# 1. Load required data ---------------------------------------------------

file_idx <- dir("data/SOM", full.names = TRUE)

BRAN <- data.frame()
ERA <- data.frame()
for(i in 1:10){
  load(file_idx[i])
  BRAN_temp <- merge(SOM_packet$BRAN_temp, SOM_packet$BRAN_temp_anom, by = c("x", "y"))
  BRAN_temp$type <- NULL 
  BRAN_temp$x <- round(BRAN_temp$x, 1)
  BRAN_temp$y <- round(BRAN_temp$y, 1)
  BRAN_temp <- unique(BRAN_temp)
  
  
}
BRAN_temp$type <- NULL
BRAN_temp$x <- round(BRAN_temp$x, 1)
BRAN_temp$y <- round(BRAN_temp$y, 1)
BRAN_temp$coords <- paste0(BRAN_temp$x, "_" , BRAN_temp$y) # Create an index column of sorts

# Choose SOM training variables
data_train <- BRAN_temp[3]

data_train_matrix <- as.matrix(scale(data_train))
names(data_train_matrix) <- names(data_train)


file_idx <- dir("data/SOM", full.names = TRUE)
# load(file_idx[1])
# BRAN_temp <- SOM_packet$BRAN_temp
BRAN_temp <- data.frame()
# i=2
for(i in 1:10){
  load(file_idx[i])
  BRAN <- SOM_packet$BRAN_temp
  BRAN$x <- round(BRAN$x)
  BRAN$y <- round(BRAN$y)
  BRAN <- BRAN[, .(temp = mean(temp, na.rm = TRUE)),
                   by = .(x,y)]
  BRAN$coords <- paste0(BRAN$x, "_" , BRAN$y)
  BRAN_wide <- t(BRAN$temp)
  # names(BRAN_wide) <- names(BRAN$coords)
  BRAN_temp <- rbind(BRAN_temp, BRAN_wide)
  # colnames(BRAN_temp) <- BRAN$coords
}
colnames(BRAN_temp) <- BRAN$coords
BRAN_temp_matrix <- as.matrix(scale(BRAN_temp))

som_grid <- somgrid(xdim = 2, ydim = 2, topo="hexagonal")

system.time(som_model <- som(BRAN_temp_matrix, 
                             grid=som_grid, 
                             rlen=100, 
                             alpha=c(0.05,0.01), 
                             n.hood = "circular",
                             keep.data = TRUE ))

# Visualise
plot(som_model, type = "changes")
#counts within nodes
plot(som_model, type = "counts", main="Node Counts")
#map quality
plot(som_model, type = "quality", main="Node Quality/Distance")
#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
#code spread
plot(som_model, type = "codes")

# show the WCSS metric for kmeans for different clustering sizes.
# Can be used as a "rough" indicator of the ideal number of clusters
mydata <- som_model$codes
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:3) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:3, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")

# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes)), 2)

# Show the map with different colours for every cluster						  
plot(som_model, type="mapping", bgcol = som_cluster, main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

#show the same plot with the codes instead of just colours
plot(som_model, type="codes", bgcol = som_cluster, main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)


#### Extract nodes and plot #####

test <- som_model$codes[1,]
test <- as.data.frame(test)
# test$coords <- rownames(test)
test$x <- as.numeric(sapply(strsplit(rownames(test), "_"), "[[", 1))
test$y <- as.numeric(sapply(strsplit(rownames(test), "_"), "[[", 2))

ggplot(data = test, aes(x = x, y = y, fill = test)) +
  geom_raster()

# Unscale the results
var_unscaled <- data.frame()
for(i in 1:length(BRAN_temp)){
  var_1 <- as.data.frame(aggregate(as.numeric(BRAN_temp[,i]), by = list(som_model$unit.classif), FUN = mean, simplify = TRUE))
  colnames(var_1) <- c("node", "temp")
  var_1$x <- rep(as.numeric(sapply(strsplit(colnames(BRAN_temp[i]), "_"), "[[", 1)), nrow(var_1))
  var_1$y <- rep(as.numeric(sapply(strsplit(colnames(BRAN_temp[i]), "_"), "[[", 2)))
  var_unscaled <- rbind(var_unscaled, var_1)
}

ggplot(data = var_unscaled, aes(x = x, y = y, fill = temp)) +
  geom_raster() +
  facet_wrap(~node)


