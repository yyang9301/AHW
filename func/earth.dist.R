### Distance functions found at:
# http://r.789695.n4.nabble.com/Geographic-distance-between-lat-long-points-in-R-td3442338.html --
## According to that blogpost, I am using the function gcd.hf using the haversine formula. I wrapped it up in a function called CalcDists so that I can get a distance matrix between N sites.

## Note that the packages "fossil" and "plyr" must be installed for all functions to run

# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian lat/lon using the haversine formula:
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

# Function to calculate matrix of distances between each two sites usng the haversine function, above:
CalcDists <- function(latlongs) {
  name <- list(rownames(latlongs), rownames(latlongs))
  n <- nrow(latlongs)
  z <- matrix(0, n, n, dimnames = name)
  for (i in 1:n) {
    for (j in 1:n) z[i, j] <- gcd.hf(long1 = latlongs[i, 1],lat1 = latlongs[i, 2], 
                                     long2 = latlongs[j, 1], lat2 = latlongs[j,2])
  }
  z <- as.dist(z)
  return(z)
}

# Distances between consecutive pairs of sites in a list. This function requires a data.frame with site in column 1, lon in column 2 and lat in column 3. Beforehand lats and lons in degrees need to be convereted to radians using the haversine function above.
PairsDists <- function(latlongs) {
  n <- nrow(latlongs)
  z <- matrix(0, n, 1, dimnames = list(latlongs[,1]))
  for (i in 1:n) {
    z[i] <- gcd.hf(long1 = latlongs[i, 2], lat1 = latlongs[i, 3],
                   long2 = latlongs[i+1, 2], lat2 = latlongs[i+1,3])
  }
  return(z)
}

# This function finds the distance and bearing between two points
dist.bear <- function(site1, site2){
  dist <- gcd.hf(deg2rad(site2$lon), deg2rad(site2$lat), deg2rad(site1$lon), deg2rad(site1$lat))
  bear <- fossil::earth.bear(site2$lon, site2$lat, site1$lon, site1$lat)
  result <- data.frame(index = paste(site2$site, site1$site, sep = " - "), dist, bear)
  return(result)
}

# This function compares one site against many
# It is intended to be used with ddply to compare many sites against many sites
dist.bear.many <- function(site, df){
  results <- plyr::ddply(df, c("site"), dist.bear, site2 = site)
  results$site <- NULL
  return(results)
}

