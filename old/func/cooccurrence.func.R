#############################################################################
###"func/cooccurrence.func.R"
## This script does:
# 1. Loads all of the functions used by "proc/cooccurrence.R" to calculate co-occurrence between and within datasets
## DEPENDS ON:
load("setupParams/distances_bearings.Rdata")
load("setupParams/SACTN_site_list.Rdata")
load("setupParams/SAWS_site_list.Rdata")
## USED BY:
# "proc/cooccurrence.R"
## CREATES:
# nothing
#############################################################################

# Manually add coast values to SAWS site list
# NB: THese have been decided based on location and are questionable
# Sites around Cape Point could be either coast but are labeled here as "wc" for balance
SAWS_site_list$coast <- c("wc","wc","wc","sc","sc","sc","sc","sc","ec","ec","ec")
SACTN_site_list$coast <- as.character(SACTN_site_list$coast)
# Combine for a central data frame from which the coastal designation of the sites may be extracted
site_list <- rbind(SAWS_site_list[,c(1:3,23)], SACTN_site_list[,c(2,5,6,9)])
## NB: It has been decided that all of these functions must be fed either heat wave or cold spell data frames
## Not data frames containing both types of events

# x <- SACTN_events[SACTN_events$site == levels(as.factor(SACTN_events$site))[5],]
# x <- x[x$type == "MHW",]
# y <- SAWS_events_tmean[SAWS_events_tmean$SACTN == x$site[1],]
# y <- y[y$type == "AHW",]

# Function that takes a dataframe (x) and removes any non-overlapping years found in another dataframe (y)
year.crop <- function(x,y){
  x$yearStrt <- year(x$date_start)
  y_yearStrt <- seq(min(year(y$date_start)), max(year(y$date_start)))
  x <- x[x$yearStrt >= min(y_yearStrt), ]
  x <- x[x$yearStrt <= max(y_yearStrt), ]
  x$yearStrt <- NULL
  return(x)
}

# test_x <- year.crop(x,y)
# test_y <- year.crop(y, test_x)

# Function that finds the size (i.e. percentile) of events (q) in a dataframe (x) and subsets accordingly
size.crop <- function(x,q){ # q == 0.0 = all events, q == 0.5 = half of the events, q == 1.0 = only the largest event
  x <- x[x$int_cum >= quantile(x$int_cum, probs = q),]
}

# test_x <- size.crop(test_x, 0.5)
# test_y <- size.crop(test_y, 0.5)


# Function that labels the percentile of the event (x = int_cum)
# Designed to be run with group_by(site)
percentile.label <- function(x){
  quants <- quantile(x) # Can add the "probs =" argument here to change the percentiles calculated but must change the other labels accordingly
  y <- rep(NA, length(x))
  y[x >= quants[1]] <- 0 # Change these manual labels if you change the quantiles to be calculated
  y[x >= quants[2]] <- 25
  y[x >= quants[3]] <- 50
  y[x >= quants[4]] <- 75
  y[x >= quants[5]] <- 100
  return(y)
}

# x <- filter(SACTN_events, site == levels(as.factor(SACTN_events$site))[1:2], type == "MHW")
# x <- x %>% 
#   group_by(site) %>%
#   mutate(percentile = percentile.label(int_cum))


# Function that takes an event (x) and extracts the closest occurring event from a data frame (y)
event.latest <- function(x,y){
  z <- y[knnx.index(as.matrix(y$date_start), as.matrix(x$date_start), k = 1),]
  latest <- as.numeric(x$date_start)-as.numeric(z$date_start)
  result <- data.frame(x, z, latest, index.2 = paste(x$site[1], z$site[1], sep = " - "))
  result$dist <- distances_bearings$dist[as.character(distances_bearings$index) == as.character(result$index.2)[1]]
  result$bear <- distances_bearings$bear[as.character(distances_bearings$index) == as.character(result$index.2)[1]]
  result$site_coast <- site_list$coast[as.character(site_list$site) == result$site[1]]
  result$site.1_coast <- site_list$coast[as.character(site_list$site) == result$site.1[1]]
  result$coast_index <- paste(result$site_coast, result$site.1_coast, sep = " - ")
  return(result)
}

# x <- filter(SAWS_SACTN_events_tmean, type == "AHW")[180,]
# y <- filter(SACTN_events, site == as.factor(x$SACTN)[1], type == "MHW")


# Function that finds events overlapping within a certain period of time for two dataframes (x, y)
# It is expected that (x) is marine and (y) is atmosphere
# It is also expected that (y) has been subsetted to match (x)
# Meaning x$site == y$SACTN

# x <- SACTN_events[SACTN_events$site == levels(as.factor(SACTN_events$site))[7],]
# y <- SAWS_events_tmean[SAWS_events_tmean$SACTN == x$site[1],]
# x <- x[x$type == "MHW",]
# y <- SAWS_events_tmean[SAWS_events_tmean$SACTN == x$site[1],]
# y <- y[y$type == "AHW",]
# 
# x <- year.crop(x,y)
# y <- year.crop(y,x)
# 
# x <- size.crop(x, 0.5)
# y <- size.crop(y, 0.5)

event.match <- function(x,y){
  z <- ddply(y, .(index), mutate, n = length(index)) # The total number of events being compared
  z$ply_index <- seq(1:length(z$index)) # Allows ddply to analyse each individual SAWS event against the SACTN dataframe
  z$percentile.idx <- min(z$percentile, na.rm = T)
  results <- ddply(z, .(ply_index), event.latest, y = x, .parallel = TRUE)
  return(results)
}


# Function that extracts the correct SAWS sites based on the SACTN site
# It then runs all of the functions created above, calculates and returns the results
# NB: This function draws on an "ahw" variable from outside of this function
# This is generated manually so as to allow a choice of tmean, tmin or tmax

# mhw <- filter(SACTN_events, type == "MHW")
# ahw <- filter(SAWS_events_tmean, type == "AHW")

## NB: Begin testing with these two values for SAWS_SACTN
# x <- filter(SACTN_events, site == levels(as.factor(SACTN_events$site))[1], type == "MHW")
# y <- filter(SAWS_SACTN_events_tmean, type == "AHW")
# compare = "between"

## NB: Begin testing with these two values for SAWS_SAWS
# x <- filter(SAWS_SAWS_events_tmean, site == levels(as.factor(SAWS_SAWS_events_tmean$site))[4], type == "AHW")
# y <- filter(SAWS_SAWS_events_tmean, type == "AHW")
# compare = "within"

cooccurrence <- function(x,y, compare = "between"){
  # 1) Correct data frames depending on comparison being performed 
  # 1.1) Subset SAWS data to match SACTN site if comparing between data frames
  if(compare == "between"){
    y <- filter(y, SACTN == x$site[1])
    # 1.2) Or add an index column if comparing within data frames
    ## NB: It is necessary to add a dummy index column as other functions make use of this
  } else if(compare == "within"){
    x$index <- x$site
    y$index <- y$site
    # Remove identical sites so as not to compare them
    y <- y[(y$index != x$index[1]),]
  } else {
    stop("You must set 'compare' to either 'between' or 'within'.")
  }
  # 2) Crop the years to have the same time span
  x <- year.crop(x,y)
  y <- year.crop(y,x)
  # 3) Add percentile labels
  x$percentile <- percentile.label(x$int_cum)
  y <- y %>% 
    group_by(index) %>%
    mutate(percentile = percentile.label(int_cum))
  # 4) Match events based on percentile labels
  event_00 <- event.match(x,y)
  event_25 <- event.match(filter(x, percentile >= 25), filter(y, percentile >= 25))
  event_50 <- event.match(filter(x, percentile >= 50), filter(y, percentile >= 50))
  event_75 <- event.match(filter(x, percentile >= 75), filter(y, percentile >= 75))
  event_100 <- event.match(filter(x, percentile >= 100), filter(y, percentile >= 100))
  events <- rbind(event_00, event_25, event_50, event_75, event_100)
  return(events)
}

# mhw <- filter(SACTN_events, type == "MHW")
# x <- filter(mhw, site == "Bordjies Deep")
# test <- cooccurrence(x)


# Function to add distance, bearing and coast columns to co-occurrence results
## NB: This is designed to be used with steps 2 and 3 in "proc/cooccurrence.R"


# x <- SACTN_SAWS_hw_tmean_CO ## Tester
add.indices <- function(x){ # Ignore warnings... Upvote hypno toad
  x <- x %>% 
    group_by(index.2) %>% 
    mutate(dist = distances_bearings$dist[as.character(distances_bearings$index) == as.character(index.2)[1]]) %>% 
    mutate(bear = distances_bearings$bear[as.character(distances_bearings$index) == as.character(index.2)[1]])
  x <- x %>% 
    group_by(site) %>%
    mutate(site_coast = site_list$coast[as.character(site_list$site) == site[1]])
  x <- x %>% 
    group_by(site.1) %>%
    mutate(site.1_coast = site_list$coast[as.character(site_list$site) == site.1[1]])
  x <- data.frame(x)
  x$coast_index <- paste(x$site_coast, x$site.1_coast, sep = " - ")
  return(x)
}
# test <- add.indices(SACTN_SAWS_hw_tmean_CO)