#############################################################################
###"load/load.SACTN.R"
## This script does:
# 1. Loads the full SACTN_daily_v4.1 data
# 2. Subsets out short time series and those missing too much
# 3. Differentiate between the two Tsitsikamma sites
# 4. Save data and site list
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(plyr)
library(dplyr)
## USED BY:
# Nothing
## CREATES:
# "data/SACTN_cropped.Rdata"
# "setupParams/SACTN_site_list.Rdata"; "~/*/.csv"
#############################################################################


# 1. Loads the full SACTN_daily_v4.1 data ---------------------------------

load("data/SACTN/SACTNdaily_v4.1.Rdata")
load("setupParams/SACTN_site_list_v4.1.Rdata")


# 2. Subsets out short time series and those missing too much -------------

# Use site list to create index of usable sites
site_list <- droplevels(site_list[site_list$NA.perc <= 10, ])
site_list <- droplevels(site_list[site_list$length >= 3650, ])

SACTN_cropped <- droplevels(SACTNdaily_v4.1[SACTNdaily_v4.1$index %in% site_list$index,])
SACTN_site_list <- site_list

# There appear to be three duplicate dates... This takes a few minutes to smooth out
  ## THis has been attended to in the SACTNraw load scripts
# system.time(SACTN_cropped <- ddply(SACTN_cropped, .(site, src, index, date), summarise, temp = mean(temp)))
# 206798 -> 206795

# 3. Differentiate between the two Tsitsikamma sites ----------------------

SACTN_cropped$site <- as.character(SACTN_cropped$site)
SACTN_cropped$site[SACTN_cropped$site == "Tsitsikamma" & SACTN_cropped$src == "SAWS"] <- "Tsitsikamma West"
SACTN_cropped$site[SACTN_cropped$site == "Tsitsikamma" & SACTN_cropped$src == "DEA"] <- "Tsitsikamma East"
SACTN_cropped$site <- as.factor(SACTN_cropped$site)
levels(SACTN_cropped$sie)

SACTN_site_list$site <- as.character(SACTN_site_list$site)
SACTN_site_list$site[SACTN_site_list$site == "Tsitsikamma" & SACTN_site_list$src == "SAWS"] <- "Tsitsikamma West"
SACTN_site_list$site[SACTN_site_list$site == "Tsitsikamma" & SACTN_site_list$src == "DEA"] <- "Tsitsikamma East"
SACTN_site_list$site <- as.factor(SACTN_site_list$site)
levels(SACTN_site_list$site)


# 4. Save data and site list ----------------------------------------------

save(SACTN_cropped, file = "data/SACTN/SACTN_cropped.Rdata")

save(SACTN_site_list, file = "setupParams/SACTN_site_list.Rdata")
write.csv(SACTN_site_list, file = "setupParams/SACTN_site_list.csv")
