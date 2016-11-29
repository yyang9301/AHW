## This script does:
# 1. Load libraries and files etc.
# 2. Format data for use with rda()
# 3. Run rda()
# 4. Look at specific linear regressions
# 5. Create figure showing relationships between variables
# 6. Stats for each group
# 7. Difference between Co-ooccurrence and not

# 1. Load libraries and files etc. ----------------------------------------

# 1) Load packages
library(vegan)
library(dplyr)
library(ggplot2)
library(RmarineHeatWaves)

# 2) Load event data
# MHW
load("~/AHW/data/events/SACTN_events.Rdata")
MHW <- filter(SACTN_events, site == "Port Nolloth" & type == "MHW")
# AHW
load("~/AHW/data/events/SAWS_SAWS_events_tmean.Rdata")
AHW <- filter(SAWS_SAWS_events_tmean, site == "Cape Columbine" & type == "AHW")
# Co-occurrence
load("~/AHW/data/cooccurrence/SACTN_SAWS_hw_tmean_CO.Rdata")
hw_tmean_CC_PN <- filter(SACTN_SAWS_hw_tmean_CO, index == "Cape Columbine - Port Nolloth")

# 3) Load climate data
load("~/AHW/data/wind/wind_PN.Rdata")
load("~/AHW/data/SACTN/SACTN_cropped.Rdata")
temp_PN <- filter(SACTN_cropped, site == "Port Nolloth")
load("~/AHW/data/SAWS/homogenised/SAWS_homogenised.Rdata")
temp_CC <- filter(SAWS_homogenised, site == "Cape Columbine")

# There is one massive event in the atmosphere around 1999-12-15
# dates_wide <- seq(as.Date("1998-12-01"), as.Date("2001-01-01"), by = "day")
# dates_event <- seq(as.Date("1999-12-15"), as.Date("2000-01-01"), by = "day")
# ggplot(data = temp_CC[temp_CC$date %in% dates_wide,], aes(x = date, y = temp)) + 
#   geom_line() +
#   geom_polygon(data = temp_CC[temp_CC$date %in% dates_event,], colour = "red", fill = "red")
# ts_CC <- temp_CC[,c(1,2,5)]
# colnames(ts_CC)[3] <- "t"
# ts_CC <- make_whole(ts_CC) # Can't run as this only works for daily data


# 2. Format data for use with rda() ---------------------------------------

# 1) MHW
MHW_start <- filter(MHW, date_start %in% wind_PN$date)
MHW_start_sub <- MHW_start[, c(4,8:23)] # Remove non-numeric are non-relevant variables
wind_MHW <- filter(wind_PN, date %in% (MHW_start$date_start-0)) # Change the number here for lag

# 2) AHW
AHW_start <- filter(AHW, date_start %in% wind_PN$date)
AHW_start_sub <- AHW_start[, c(4,8:23)]
wind_AHW <- filter(wind_PN, date %in% (AHW_start$date_start-0))

# 3) Co-occurrence
CO_start <- filter(hw_tmean_CC_PN, date_start %in% wind_PN$date & latest <=7 & latest >= -7)
CO_start <- CO_start[CO_start$event_no == unique(CO_start$event_no),]
CO_start <- CO_start[1:8,]
CO_start_sub <- CO_start[, c(5,9:24,35,39:54)]
wind_CO_SAWS <- filter(wind_PN, date %in% CO_start$date_start) # The start dates of the AHWs
wind_CO_SACTN <- filter(wind_PN, date %in% CO_start$date_start.1) # The start dates of the MHWs
wind_CO_SACTN <- wind_CO_SACTN[c(1:3,3,4:7),] # Extend as one of the MHWs is paired to two AHWs


# 3. Run rda() ------------------------------------------------------------

## Sources for tutorials
# http://pgugger.al.umces.edu/assets/redundancy-analysis-for-landscape-genetics.pdf
##

# 1) MHW
# RDA
MHW_RDA <- rda(MHW_start_sub ~ speed + bearing, data = wind_MHW, scale = T)
anova(MHW_RDA)
plot(MHW_RDA, scaling = 1)
summary(MHW_RDA)
# CCA
MHW_CCA <- cca(MHW_start_sub ~ speed + bearing, data = wind_MHW, scale = T)
anova(MHW_CCA)
plot(MHW_CCA, scaling = 1)
summary(MHW_CCA)

# 2) AHW
AHW_RDA <- rda(AHW_start_sub ~ speed + bearing, data = wind_AHW, scale = T)
anova(AHW_RDA)
plot(AHW_RDA, scaling = 1)
summary(AHW_RDA)

# 3) CO
# SAWS wind start dates
CO_SAWS_RDA <- rda(CO_start_sub ~ speed + bearing, data = wind_CO_SAWS, scale = T)
anova(CO_SAWS_RDA)
plot(CO_SAWS_RDA)
plot(CO_SAWS_RDA, scaling = 1)
summary(CO_SAWS_RDA)
# SACTN wind start dates
CO_SAWS_RDA <- rda(CO_start_sub ~ speed + bearing, data = wind_CO_SACTN, scale = T)
anova(CO_SAWS_RDA)
plot(CO_SAWS_RDA)
plot(CO_SAWS_RDA, scaling = 1)
summary(CO_SAWS_RDA)
# SAWS wind start dates are a better explanatory variable


# 4. Look at specific linear regressions ----------------------------------

# 1) MHW
summary(lm(MHW_start$int_cum_abs~wind_MHW$speed))
summary(lm(MHW_start$int_cum_abs~wind_MHW$bearing))
summary(lm(MHW_start$int_cum~wind_MHW$speed))
summary(lm(MHW_start$int_max~wind_MHW$speed))
summary(lm(MHW_start$int_max_abs~wind_MHW$speed))

# 2) AHW
summary(lm(AHW_start$int_cum_abs~wind_AHW$speed))
summary(lm(AHW_start$int_cum_abs~wind_AHW$bearing))

# 3) Co-occurrence
summary(lm(CO_start$int_cum_abs.1~wind_CO_SAWS$speed)) # p = 0.01
summary(lm(CO_start$duration.1~wind_CO_SAWS$speed)) # p = 0.01
summary(lm(CO_start$int_max_abs.1~wind_CO_SAWS$bearing)) # p = 0.05
# This last one shows a significant relationship but visually there appears to be little there


# 5. Create figure showing relationships between variables --------

# 1) MHW
wind_MHW <- filter(wind_PN, date %in% (MHW_start$date_start-0)) # Change the number here for lag
summary(lm(MHW_start$int_cum_abs~wind_MHW$speed))
MHW_data <- cbind(MHW_start, wind_MHW)
ggplot(data = MHW_data, aes (x = speed, y = int_cum_abs)) + 
  geom_point() + 
  geom_smooth(colour = "red") + 
  geom_smooth(method = "lm")

# 2) AHW
wind_AHW <- filter(wind_PN, date %in% (AHW_start$date_start-0)) # Change the number here for lag
summary(lm(AHW_start$int_cum_abs~wind_AHW$speed))
AHW_data <- cbind(AHW_start, wind_AHW)
ggplot(data = AHW_data, aes (x = speed, y = int_cum_abs)) + 
  geom_point() + 
  geom_smooth(colour = "red") + 
  geom_smooth(method = "lm")

# 3) CO
# int_cum_abs.1 & speed
wind_CO_SAWS <- filter(wind_PN, date %in% (CO_start$date_start-0)) # Change the number here for lag
summary(lm(CO_start$int_cum_abs.1~wind_CO_SAWS$speed))
CO_data <- cbind(CO_start, wind_CO_SAWS)
ggplot(data = CO_data, aes (x = speed, y = int_cum_abs.1)) + 
  geom_point() + 
  geom_smooth(colour = "red") + 
  geom_smooth(method = "lm")
# duration.1 & speed
wind_CO_SAWS <- filter(wind_PN, date %in% (CO_start$date_start-0)) # Change the number here for lag
summary(lm(CO_start$duration.1~wind_CO_SAWS$speed))
CO_data <- cbind(CO_start, wind_CO_SAWS)
ggplot(data = CO_data, aes (x = speed, y = duration.1)) + 
  geom_point() + 
  geom_smooth(colour = "red") + 
  geom_smooth(method = "lm")
# int_max_abs.1 & bearing
wind_CO_SAWS <- filter(wind_PN, date %in% (CO_start$date_start-0)) # Change the number here for lag
summary(lm(CO_start$int_max_abs.1~wind_CO_SAWS$bearing))
CO_data <- cbind(CO_start, wind_CO_SAWS)
ggplot(data = CO_data, aes (x = bearing, y = int_max_abs.1)) + 
  geom_point() + 
  geom_smooth(colour = "red") + 
  geom_smooth(method = "lm")
# The relationship appears clear, but if one considers the implication of the quantification of wind bearing this is less convincing


# 6. Stats for each group -------------------------------------------------

# 1) MHW
# int_cum_abs
mean(MHW_data$int_cum_abs, na.rm = T) # 108.03
sd(MHW_data$int_cum_abs, na.rm = T) # 37.23
# duration
mean(MHW_data$duration, na.rm = T) # 7.23
sd(MHW_data$duration, na.rm = T) # 2.52
# speed
mean(MHW_data$speed, na.rm = T) # 3.43
sd(MHW_data$speed, na.rm = T) # 3.66
# bearing
mean(MHW_data$bearing, na.rm = T) # 244
sd(MHW_data$bearing, na.rm = T) # 119
mean(MHW_data$bearing[MHW_data$bearing != 0], na.rm = T) # 283
sd(MHW_data$bearing[MHW_data$bearing != 0], na.rm = T) # 70

# 3) AHW
# int_cum_abs
mean(AHW_data$int_cum_abs, na.rm = T) # 85.63
sd(AHW_data$int_cum_abs, na.rm = T) # 48.63
# duration
mean(AHW_data$duration, na.rm = T) # 4.16
sd(AHW_data$duration, na.rm = T) # 2.22
# speed
mean(AHW_data$speed, na.rm = T) # 3.23
sd(AHW_data$speed, na.rm = T) # 3.33
# bearing
mean(AHW_data$bearing, na.rm = T) # 118
sd(AHW_data$bearing, na.rm = T) # 88
mean(AHW_data$bearing[AHW_data$bearing != 0], na.rm = T) # 140
sd(AHW_data$bearing[AHW_data$bearing != 0], na.rm = T) # 78

# 3) CO
## int_cum_abs
# AHW
mean(CO_data$int_cum_abs, na.rm = T) # 72
sd(CO_data$int_cum_abs, na.rm = T) # 16.89
# MHW
mean(CO_data$int_cum_abs.1, na.rm = T) # 114.78
sd(CO_data$int_cum_abs.1, na.rm = T) # 32.08
## duration
# AHW
mean(CO_data$duration, na.rm = T) # 3.38
sd(CO_data$duration, na.rm = T) # 0.74
# AHW
mean(CO_data$duration.1, na.rm = T) # 7.13
sd(CO_data$duration.1, na.rm = T) # 1.96
## speed
# These speed and bearing values are from the start dates of the AHWs
mean(CO_data$speed, na.rm = T) # 3.30
sd(CO_data$speed, na.rm = T) # 5.02
## bearing
mean(CO_data$bearing, na.rm = T) # 131
sd(CO_data$bearing, na.rm = T) # 104
mean(CO_data$bearing[CO_data$bearing != 0], na.rm = T) # 182
sd(CO_data$bearing[CO_data$bearing != 0], na.rm = T) # 72


# 7. Difference between Co-ooccurrence and not ----------------------------

# 1) T-tests

# 1.1) MHW vs. AHW
# int_cum_abs
t.test(MHW_data$int_cum_abs, AHW_data$int_cum_abs, alternative = "two.sided", paired = F) # p = 0.0504
# int_cum_rel_tresh
t.test(MHW_data$int_cum_rel_thresh, AHW_data$int_cum_rel_thresh, alternative = "two.sided", paired = F) # p = 0.174
# int_max_abs
t.test(MHW_data$int_max_abs, AHW_data$int_max_abs, alternative = "two.sided", paired = F) # p < 0.001
# duration
t.test(MHW_data$duration, AHW_data$duration, alternative = "two.sided", paired = F) # p < 0.001
# speed
t.test(MHW_data$speed, AHW_data$speed, alternative = "two.sided", paired = F) # p = 0.837
# bearing
t.test(MHW_data$bearing, AHW_data$bearing, alternative = "two.sided", paired = F) # p < 0.001
t.test(MHW_data$bearing[MHW_data$bearing != 0], AHW_data$bearing[AHW_data$bearing != 0], alternative = "two.sided", paired = F) # p < 0.001

# 1.2) MHW vs. CO-MHW
# int_cum_abs
t.test(MHW_data$int_cum_abs, CO_data$int_cum_abs.1, alternative = "two.sided", paired = F) # p = 0.633
# int_cum_rel_tresh
t.test(MHW_data$int_cum_rel_thresh, CO_data$int_cum_rel_thresh.1, alternative = "two.sided", paired = F) # p = 0.228
# int_max_abs
t.test(MHW_data$int_max_abs, CO_data$int_max_abs.1, alternative = "two.sided", paired = F) # p = 0.098
# duration
t.test(MHW_data$duration, CO_data$duration.1, alternative = "two.sided", paired = F) # p = 0.909
# speed
t.test(MHW_data$speed, CO_data$speed, alternative = "two.sided", paired = F) # p = 0.947
# bearing
t.test(MHW_data$bearing, CO_data$bearing, alternative = "two.sided", paired = F) # p = 0.030
t.test(MHW_data$bearing[MHW_data$bearing != 0], CO_data$bearing[CO_data$bearing != 0], alternative = "two.sided", paired = F) # p = 0.016

# 1.3) AHW vs. CO-AHW
# int_cum_abs
t.test(AHW_data$int_cum_abs, CO_data$int_cum_abs, alternative = "two.sided", paired = F) # p = 0.177
# int_cum_rel_tresh
t.test(AHW_data$int_cum_rel_thresh, CO_data$int_cum_rel_thresh, alternative = "two.sided", paired = F) # p = 0.230
# int_max_abs
t.test(AHW_data$int_max_abs, CO_data$int_max_abs, alternative = "two.sided", paired = F) # p = 0.532
# duration
t.test(AHW_data$duration, CO_data$duration, alternative = "two.sided", paired = F) # p = 0.088
# speed
t.test(AHW_data$speed, CO_data$speed, alternative = "two.sided", paired = F) # p = 0.973
# bearing
t.test(AHW_data$bearing, CO_data$bearing, alternative = "two.sided", paired = F) # p = 0.654
t.test(AHW_data$bearing[AHW_data$bearing != 0], CO_data$bearing[CO_data$bearing != 0], alternative = "two.sided", paired = F) # p = 0.238

# 2) ANOVA

# 2.1) Prep data for use in ANOVA
# MHW
MHW_aov <- cbind(MHW_start_sub, wind_MHW)
MHW_aov$type <- "MHW"
# AHW
AHW_aov <- cbind(AHW_start_sub, wind_AHW)
AHW_aov$type <- "AHW"
# CO-AHW
CO_AHW_aov <- cbind(CO_start_sub[,1:17], wind_CO_SAWS)
CO_AHW_aov$type <- "CO-AHW"
# CO-MHW
CO_MHW_aov <- cbind(CO_start_sub[,18:34], wind_CO_SAWS)
CO_MHW_aov$type <- "CO-MHW"
colnames(CO_MHW_aov) <- colnames(CO_AHW_aov)
# Combine
ALL_aov <- rbind(MHW_aov, AHW_aov, CO_MHW_aov, CO_AHW_aov)

# 2.2) Run ANOVA
summary(aov(int_cum_abs ~ type, data = ALL_aov)) # p = 0.05
summary(aov(int_cum_rel_thresh ~ type, data = ALL_aov)) # p = 0.418
summary(aov(int_max_abs ~ type, data = ALL_aov)) # p < 0.001
summary(aov(duration ~ type, data = ALL_aov)) # p < 0.001
summary(aov(speed ~ type, data = ALL_aov)) # p = 0.998
summary(aov(bearing ~ type, data = ALL_aov)) # p < 0.001
summary(aov(int_cum_abs + duration + bearing ~ type, data = ALL_aov)) # p < 0.001
coef(aov(int_cum_abs + duration + bearing ~ type, data = ALL_aov))
