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

source("func/synoptic.fig.R")

event <- SACTN_events[SACTN_events$duration == min(SACTN_events$duration),][1,] # shortest...
system.time(synoptic.fig(event))
