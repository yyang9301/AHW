###########################################################################
### "5.Text.R"
## This script shows the analyses performed to create any statistics etc. referenced in the text of "LaTeX/Schlegel_et_al.tex"
# 1. Load all libraries and functions used in this script
# 2. Introduction
# 3. Methods
# 4. Results
# 5. Discussion
# 6. Conclussion
#############################################################################



# 1. Load all libraries and functions used in this script -----------------

# library(zoo)
# library(lubridate)
# library(ggplot2)
# library(reshape2)
# library(plyr)
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(broom)
# library(xtable)
# library(tibble)
# library(doMC); doMC::registerDoMC(cores = 4)
source("func/synoptic.func.R")
source("func/som.func.R")


# 2. Introduction ---------------------------------------------------------

# No stats are given in the intro


# 3. Methods --------------------------------------------------------------

## Study area
# The default lon/ lat ranges
wlon <- 10
elon <- 40
nlat <- -25
slat <- -40

## In situ data
# Number of time series in SACTN
load("~/SACTN/metadata/site_list_v4.1.Rdata")
nrow(site_list)

# Mean duration
mean(site_list$length)/365.25 # 19.65

# Exclude time series under 10 years or over 10% NA
site_list <- droplevels(site_list[site_list$NA.perc <= 10, ]) # 50 sites
site_list <- droplevels(site_list[site_list$length >= 3650, ]) # 26 sites
nrow(site_list)

# New mean duration
mean(site_list$length)/365.25 # 22.31

# Number of MHWs screened out by removing any event under 15 days
  # Taken from "2.Model_fitting.R"
# 976 total events
# 129 after screening
976-129 # 847 events removed

# BRAN data period
# "1994-01-01" to "2016-08-31"

# ERA data period
# "1979-01-01" to "2016-12-31"

# Number of events analysed
load("data/SACTN/SACTN_events.Rdata")
nrow(SACTN_events)

# Length between first and last event
range(SACTN_events$date_start)

# Number of time series below 30 years
nrow(filter(site_list, length < 365.25*30)) # 20


# 4. Results --------------------------------------------------------------

# ANOSIM
load("data/som_ANOSIM.Rdata")
som_anosim$signif # p = 0.001


# 5. Discussion -----------------------------------------------------------

lon_sub <- seq(10, 40, by = 1)
lat_sub <- seq(-40, -15, by = 1)

# Isolate the square of recurrent anomlous currents
  # They stand out most in node 3 so we use that
load("data/node_means.Rdata")
strange_square <- node_means %>% 
  filter(node == 3, var %in% c("AVISO/u-anom", "AVISO/v-anom"),
         # x %in% lon_sub, y %in% lat_sub,
         x <= 15, y <= -35) %>% 
  select(-node) %>% 
  dcast(x+y~var) %>% 
  rename(u = 'AVISO/u-anom', v = 'AVISO/v-anom')

ggplot(data = strange_square) +
  geom_segment(aes(x = x, y = y, xend = x + u, yend = y + v),
               arrow = arrow(angle = 20, length = unit(0.1, "cm"), 
                             type = "open"), alpha = 0.7, colour = "black")

# From the above figure it is clear that the strange square is from:
# 12.5 E to 14.5 E
# 35.5 S to 37.5 S

# 6. Conclussion ----------------------------------------------------------


