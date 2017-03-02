#############################################################################
###"graph/figures3.R"
## This script does:
# 1. Mass produce the synoptic figures
## DEPENDS ON:
source("func/synoptic.fig.R")
## USED BY:
# 
## CREATES:
# Figures similar to Eric's figures in the MHW atlas
#############################################################################


# 1. Mass produce the synoptic figures ------------------------------------

event <- SACTN_events[SACTN_events$duration == min(SACTN_events$duration),][1,] # shortest...
system.time(synoptic.fig(event))

load("data/SOM/Betty's Bay_1.Rdata")
BRAN_temp <- SOM_packet$BRAN_temp

ggplot(data = BRAN_temp, aes(x = x, y = y, fill = temp)) +
  geom_raster()

event <- SACTN_events[SACTN_events$duration == max(SACTN_events$duration),] # longest...
system.time(synoptic.fig(event))
