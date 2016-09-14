#############################################################################
###"proc/coocurrence.R"
## This script does:
# 1. Load matched data and events
# 2. Compare rates of co-occurrence for matched sites
# 3. Compute other metrics of comparison such as change over distance
# 4. Create figures showing results
# 5. Save
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(stringr)
library(zoo)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(broom)
library(tibble)
library(purrr)
source("setupParams/theme.R")
## USED BY:
# 
## CREATES:
# "data/mhw_CO_5_2.csv"
# "data/mhw_CO_5_2.csv"
#############################################################################


# 1. Load matched data and events -----------------------------------------

# Matched data
load("data/SACTN_SAWS_match.Rdata")

# SAWS events
load("data/SAWS/events/ahw_events_5day_2gap.Rdata")
load("data/SAWS/events/ahw_events_3day_1gap.Rdata")
load("data/SAWS/events/acs_events_5day_2gap.Rdata")
load("data/SAWS/events/acs_events_3day_1gap.Rdata")

# SACTN events
load("data/SACTN/events/mhw_events_5day_2gap.Rdata")
load("data/SACTN/events/mcs_events_5day_2gap.Rdata")


# 2. Compare rates of co-occurrence for matched sites ---------------------

## NB: Adapted from "~/MHW/proc/results2.R"
# dat1 must be the marine data frame
# dat2 is the atmospheric data frame
cooccurrence <- function(dat1, dat2, lag = seq(2,14,4)){
  dat1$site <- as.character(dat1$site)
  dat2$site <- as.character(dat2$site)
  dat3 <- data.frame()
  direction <- c("b","x","a")
  for(i in 1:length(levels(as.factor(dat1$site)))) {
    x1 <- droplevels(subset(dat1, site == levels(as.factor(dat1$site))[i]))
    meta1 <- SACTN_SAWS_match$facet[SACTN_SAWS_match$site == x1$site[1]][1]
    meta1 <- sapply(strsplit(meta1, " - "), "[[", 2)
    x2 <- droplevels(subset(dat2, site == meta1))
    x2$yearStrt <- year(x2$date_start)
    x1 <- x1[x1$yearStrt >= min(x2$yearStrt), ] # Subset x so that dates match up
    x1 <- x1[x1$yearStrt <= max(x2$yearStrt), ]
    if(length(x1$site) == 0){
      x1 <- droplevels(subset(dat1, site == levels(as.factor(dat1$site))[i]))
      meta1 <- SACTN_SAWS_match$facet[SACTN_SAWS_match$site == x1$site[1]][1]
      meta1 <- sapply(strsplit(meta1, " - "), "[[", 2)
      x2 <- droplevels(subset(dat2, site == meta1))
      z <- data.frame(facet = paste(x1$site[1], x2$site[1], sep = " - "),
                      lag = 14, quantile = 0, direction = "x",
                      SACTN = nrow(x1), SAWS = nrow(x2),
                      cooccurrence = 0, proportion = 0)
      dat3 <- rbind(dat3, z)
    } else {
      x2 <- x2[x2$yearStrt >= min(x1$yearStrt), ]
      x2 <- x2[x2$yearStrt <= max(x1$yearStrt), ]
      if(length(x2$site) == 0){
        x1 <- droplevels(subset(dat1, site == levels(as.factor(dat1$site))[i]))
        meta1 <- SACTN_SAWS_match$facet[SACTN_SAWS_match$site == x1$site[1]][1]
        meta1 <- sapply(strsplit(meta1, " - "), "[[", 2)
        x2 <- droplevels(subset(dat2, site == meta1))
        z <- data.frame(facet = paste(x1$site[1], x2$site[1], sep = " - "),
                        lag = 14, quantile = 0, direction = "x",
                        SACTN = nrow(x1), SAWS = nrow(x2),
                        cooccurrence = 0, proportion = 0)
        dat3 <- rbind(dat3, z)
      } else {
        for(j in 1:length(lag)){
          for(k in 1:length(seq(0.0,1,0.25))){
            for(l in 1:length(direction)){
              x1.1 <- x1[x1$intCum >= quantile(x1$intCum, probs = seq(0.0,1,0.25)[k]),]
              x2.1 <- x2[x2$int_cum >= quantile(x2$int_cum, probs = seq(0.0,1,0.25)[k]),]
              y <- 0
              #x3 <- data.frame() # For test purposes to see which events match up
              for(m in 1:nrow(x1.1)) {
                x1.2 <- x1.1$date[m]
                if(direction[l] == "b"){
                  x1.3 <- seq((x1.2 - days(lag[j])), x1.2, 1)
                } else if(direction[l] == "x"){
                  x1.3 <- seq((x1.2 - days(lag[j])), (x1.2 + days(lag[j])), 1)
                } else if (direction[l] == "a") {
                  x1.3 <- seq(x1.2, (x1.2 + days(lag[j])), 1)
                }
                # if(length(x2.1$site) == 0){
                #   y <- y + 0
                # } else {
                x2.2 <- droplevels(subset(x2.1, date_start %in% x1.3))
                y <- y + nrow(x2.2)
                # }
              }
              z <- data.frame(facet = paste(x1$site[1], x2$site[1], sep = " - "),
                              lag = lag[j], quantile = seq(0.0,1,0.25)[k], direction = direction[l],
                              SACTN = nrow(x1.1), SAWS = nrow(x2.1),
                              cooccurrence = y, proportion = y/nrow(x1.1))
              dat3 <- rbind(dat3, z)
            }
          }
        }
      }
    }
  }
  return(dat3)
}

# MHW
system.time(mhw_CO_5_2 <- cooccurrence(mhw_events_5day_2gap, ahw_events_5day_2gap)) # 82 seconds... would be faster without for loops...
write.csv(mhw_CO_5_2, "data/mhw_CO_5_2.csv", row.names = F)
system.time(mhw_CO_3_1 <- cooccurrence(mhw_events_5day_2gap, ahw_events_3day_1gap)) # 89 seconds
write.csv(mhw_CO_3_1, "data/mhw_CO_3_1.csv", row.names = F)
# MCS
system.time(mcs_CO_5_2 <- cooccurrence(mcs_events_5day_2gap, acs_events_5day_2gap)) # 76 seconds
write.csv(mcs_CO_5_2, "data/mcs_CO_5_2.csv", row.names = F)
system.time(mcs_CO_3_1 <- cooccurrence(mcs_events_5day_2gap, acs_events_3day_1gap)) # 84 seconds
write.csv(mcs_CO_3_1, "data/mcs_CO_3_1.csv", row.names = F)


# 3. Compute other metrics of comparison such as change over distance --------


# 4. Create figures showing results ------------------------------------------

#Prepares data for plotting

# mhwCO$site <- factor(mhwCO$site, levels = siteOrder)
mhw_CO_5_2$direction <- factor(mhw_CO_5_2$direction, levels = c("b", "x", "a"))
mhw_CO_5_2$index <- paste(mhw_CO_5_2$lag, mhw_CO_5_2$direction, sep = "_")
mhw_CO_3_1$direction <- factor(mhw_CO_3_1$direction, levels = c("b", "x", "a"))
mhw_CO_3_1$index <- paste(mhw_CO_3_1$lag, mhw_CO_3_1$direction, sep = "_")
# mcsCO$site <- factor(mcsCO$site, levels = siteOrder)
mcs_CO_5_2$direction <- factor(mcs_CO_5_2$direction, levels = c("b", "x", "a"))
mcs_CO_5_2$index <- paste(mcs_CO_5_2$lag, mcs_CO_5_2$direction, sep = "_")
mcs_CO_3_1$direction <- factor(mcs_CO_3_1$direction, levels = c("b", "x", "a"))
mcs_CO_3_1$index <- paste(mcs_CO_3_1$lag, mcs_CO_3_1$direction, sep = "_")

cooccurrenceQuantFigure <- function(dat){
  p2 <- ggplot(data = dat, aes(x = quantile, y = proportion)) + bw_update +
    geom_line(aes(colour = as.factor(index))) + 
    geom_point(aes(colour = as.factor(index))) +
    facet_grid(facet ~ direction) +
    ylab("proportion") + xlab("percentile (%)") #+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  p2
}

mhw_CO_5_2_fig <- cooccurrenceQuantFigure(mhw_CO_5_2)
ggsave("graph/mhw_CO_5_2.pdf", width = 16, height = 30)
mhw_CO_3_1_fig <- cooccurrenceQuantFigure(mhw_CO_3_1)
ggsave("graph/mhw_CO_3_1.pdf", width = 16, height = 30)
mcs_CO_5_2_fig <- cooccurrenceQuantFigure(mcs_CO_5_2)
ggsave("graph/mcs_CO_5_2.pdf", width = 16, height = 30)
mcs_CO_3_1_fig <- cooccurrenceQuantFigure(mcs_CO_3_1)
ggsave("graph/mcs_CO_3_1.pdf", width = 16, height = 30)


# 5. Save -----------------------------------------------------------------


