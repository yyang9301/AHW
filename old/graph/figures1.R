#############################################################################
###"graph/figures.R"
## This script does:
# 1. Load cleaned results
# 2. Create data frames for specific conditions
# 3. Create figures
## DEPENDS ON:
library(doMC); doMC::registerDoMC(cores = 4)
library(stringr)
library(zoo)
library(lubridate)
library(ggplot2)
library(reshape2)
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
# 
#############################################################################


# 1. Load cleaned results -------------------------------------------------

# Heat waves
load("data/hw_tmean_CO.Rdata")
load("data/hw_tmax_CO.Rdata")
load("data/hw_tmin_CO.Rdata")
# Cold-spells
load("data/cs_tmean_CO.Rdata")
load("data/cs_tmax_CO.Rdata")
load("data/cs_tmin_CO.Rdata")


# 2. Create data frames for specific conditions ---------------------------

### Nearest sites
# Function to extract nearest SAWS and SACTN results
nearest <- function(df){
  df1 <- df[df$dist == min(df$dist, na.rm = T),]
}

hw_tmean_nearest <- ddply(hw_tmean_CO, .(SACTN, percentile), nearest)
hw_tmax_nearest <- ddply(hw_tmax_CO, .(SACTN, percentile), nearest)
hw_tmin_nearest <- ddply(hw_tmin_CO, .(SACTN, percentile), nearest)
cs_tmean_nearest <- ddply(cs_tmean_CO, .(SACTN, percentile), nearest)
cs_tmax_nearest <- ddply(cs_tmax_CO, .(SACTN, percentile), nearest)
cs_tmin_nearest <- ddply(cs_tmin_CO, .(SACTN, percentile), nearest)

### Best co-occurrence
# Functions to extract best SAWS and SACTN results when SACTN event occurs before SAWS
df <- filter(hw_tmean_CO, SACTN == levels(as.factor(hw_tmean_CO$SACTN))[3], percentile == 100)
best.b <- function(df){
  df1 <- df[df$b_14 == max(df$b_14, na.rm = T),]
  if(nrow(df1) > 1){
    df1[,c(1,2,14,15,17)] <- NA
    df[,c(6:13)] <- 0
    df1 <- df1[1,]
  }
  return(df1)
}

hw_tmean_best.b <- ddply(hw_tmean_CO, .(SACTN, percentile), best.b)
hw_tmax_best.b <- ddply(hw_tmax_CO, .(SACTN, percentile), best.b)
hw_tmin_best.b <- ddply(hw_tmin_CO, .(SACTN, percentile), best.b)
cs_tmean_best.b <- ddply(cs_tmean_CO, .(SACTN, percentile), best.b)
cs_tmax_best.b <- ddply(cs_tmax_CO, .(SACTN, percentile), best.b)
cs_tmin_best.b <- ddply(cs_tmin_CO, .(SACTN, percentile), best.b)

# Functions to extract best SAWS and SACTN results when SACTN event occurs after SAWS
best.a <- function(df){
  df1 <- df[df$a_14 == max(df$a_14, na.rm = T),]
  if(nrow(df1) > 1){
    df1[,c(1,2,14,15,17)] <- NA
    df[,c(6:13)] <- 0
    df1 <- df1[1,]
  }
  return(df1)
}

hw_tmean_best.a <- ddply(hw_tmean_CO, .(SACTN, percentile), best.a)
hw_tmax_best.a <- ddply(hw_tmax_CO, .(SACTN, percentile), best.a)
hw_tmin_best.a <- ddply(hw_tmin_CO, .(SACTN, percentile), best.a)
cs_tmean_best.a <- ddply(cs_tmean_CO, .(SACTN, percentile), best.a)
cs_tmax_best.a <- ddply(cs_tmax_CO, .(SACTN, percentile), best.a)
cs_tmin_best.a <- ddply(cs_tmin_CO, .(SACTN, percentile), best.a)

# Functions to extract distance and co-occurrence columns
# This is unneccesary as this can be done in ggplot without any other steps

# Best results by coastal comparisons

# Not yet calculated

# 3. Create figures ----------------------------------------------------

# Proportion vs. percentile for nearest sites
# df <- hw_tmean_nearest
cooccurrenceFigure <- function(df){
  p2 <- ggplot(df, aes(x = percentile)) + bw_update +
    geom_line(aes(y = (b_14/n)), colour = "darkorchid1") +
    geom_point(aes(y = (b_14/n)), colour = "darkorchid1") +
    geom_line(aes(y = (a_14/n)), colour = "springgreen") +
    geom_point(aes(y = (a_14/n)), colour = "springgreen") +
    facet_wrap(~index, ncol = 5) +
    ylab("proportion") + xlab("percentile (%)") #+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  p2
}
# Heat waves
hw_tmean_nearest_fig <- cooccurrenceFigure(hw_tmean_nearest)
ggsave("graph/hw_tmean_nearest.pdf", width = 12, height = 8)
hw_tmax_nearest_fig <- cooccurrenceFigure(hw_tmax_nearest)
ggsave("graph/hw_tmax_nearest.pdf", width = 12, height = 8)
hw_tmin_nearest_fig <- cooccurrenceFigure(hw_tmin_nearest)
ggsave("graph/hw_tmin_nearest.pdf", width = 12, height = 8)
# Cold-spells
cs_tmean_nearest_fig <- cooccurrenceFigure(cs_tmean_nearest)
ggsave("graph/cs_tmean_nearest.pdf", width = 12, height = 8)
cs_tmax_nearest_fig <- cooccurrenceFigure(cs_tmax_nearest)
ggsave("graph/cs_tmax_nearest.pdf", width = 12, height = 8)
cs_tmin_nearest_fig <- cooccurrenceFigure(cs_tmin_nearest)
ggsave("graph/cs_tmin_nearest.pdf", width = 12, height = 8)


# Proportion vs. percentile for best matches
# df <- hw_tmean_best.a
cooccurrenceFigure <- function(df){
  p2 <- ggplot(df, aes(x = percentile)) + bw_update +
    geom_line(aes(y = (b_14/n)), colour = "darkorchid1") +
    geom_point(aes(y = (b_14/n)), colour = "darkorchid1") +
    geom_line(aes(y = (a_14/n)), colour = "springgreen") +
    geom_point(aes(y = (a_14/n)), colour = "springgreen") +
    facet_wrap(~SACTN, ncol = 5) +
    ylab("proportion") + xlab("percentile (%)") #+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  p2
}
## Best matches with SACTN before SAWS
# Heat waves
hw_tmean_best.b_fig <- cooccurrenceFigure(hw_tmean_best.b)
ggsave("graph/hw_tmean_best.b.pdf", width = 12, height = 8)
hw_tmax_best.b_fig <- cooccurrenceFigure(hw_tmax_best.b)
ggsave("graph/hw_tmax_best.b.pdf", width = 12, height = 8)
hw_tmin_best.b_fig <- cooccurrenceFigure(hw_tmin_best.b)
ggsave("graph/hw_tmin_best.b.pdf", width = 12, height = 8)
# Cold-spells
cs_tmean_best.b_fig <- cooccurrenceFigure(cs_tmean_best.b)
ggsave("graph/cs_tmean_best.b.pdf", width = 12, height = 8)
cs_tmax_best.b_fig <- cooccurrenceFigure(cs_tmax_best.b)
ggsave("graph/cs_tmax_best.b.pdf", width = 12, height = 8)
cs_tmin_best.b_fig <- cooccurrenceFigure(cs_tmin_best.b)
ggsave("graph/cs_tmin_best.b.pdf", width = 12, height = 8)
## Best matches with SACTN after SAWS
# Heat waves
hw_tmean_best.a_fig <- cooccurrenceFigure(hw_tmean_best.a)
ggsave("graph/hw_tmean_best.a.pdf", width = 12, height = 8)
hw_tmax_best.a_fig <- cooccurrenceFigure(hw_tmax_best.a)
ggsave("graph/hw_tmax_best.a.pdf", width = 12, height = 8)
hw_tmin_best.a_fig <- cooccurrenceFigure(hw_tmin_best.a)
ggsave("graph/hw_tmin_best.a.pdf", width = 12, height = 8)
# Cold-spells
cs_tmean_best.a_fig <- cooccurrenceFigure(cs_tmean_best.a)
ggsave("graph/cs_tmean_best.a.pdf", width = 12, height = 8)
cs_tmax_best.a_fig <- cooccurrenceFigure(cs_tmax_best.a)
ggsave("graph/cs_tmax_best.a.pdf", width = 12, height = 8)
cs_tmin_best.a_fig <- cooccurrenceFigure(cs_tmin_best.a)
ggsave("graph/cs_tmin_best.a.pdf", width = 12, height = 8)

# Proportion vs. distance
df <- hw_tmean_CO
cooccurrenceFigure <- function(df){
  p2 <- ggplot(df[df$percentile == 0,], aes(x = dist)) + bw_update +
    geom_line(aes(y = (b_14/n)), colour = "darkorchid1", alpha = 0.3) +
    geom_point(aes(y = (b_14/n)), colour = "darkorchid1", alpha = 0.3) +
    geom_smooth(aes(x = dist, y = (b_14/n)), colour = "darkorchid1") +
    geom_line(aes(y = (a_14/n)), colour = "springgreen", alpha = 0.3) +
    geom_point(aes(y = (a_14/n)), colour = "springgreen", alpha = 0.3) +
    geom_smooth(aes(x = dist, y = (a_14/n)), colour = "springgreen") +
    # facet_wrap(~SACTN, ncol = 5) +
    ylab("proportion") + xlab("distance (km)") #+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  p2
}
# Heat waves
hw_tmean_dist_fig <- cooccurrenceFigure(hw_tmean_CO)
ggsave("graph/hw_tmean_dist.pdf", width = 12, height = 8)
hw_tmax_dist_fig <- cooccurrenceFigure(hw_tmax_CO)
ggsave("graph/hw_tmax_dist.pdf", width = 12, height = 8)
hw_tmin_dist_fig <- cooccurrenceFigure(hw_tmin_CO)
ggsave("graph/hw_tmin_dist.pdf", width = 12, height = 8)
# Cold-spells
cs_tmean_dist_fig <- cooccurrenceFigure(cs_tmean_CO)
ggsave("graph/cs_tmean_dist.pdf", width = 12, height = 8)
cs_tmax_dist_fig <- cooccurrenceFigure(cs_tmax_CO)
ggsave("graph/cs_tmax_dist.pdf", width = 12, height = 8)
cs_tmin_dist_fig <- cooccurrenceFigure(cs_tmin_CO)
ggsave("graph/cs_tmin_dist.pdf", width = 12, height = 8)

