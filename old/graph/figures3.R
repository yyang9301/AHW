#############################################################################
###"graph/figures.som.R"
## This script does:
# 1. Create list of events to be calculated
# 2. Mass produce the synoptic figures
## DEPENDS ON:
source("func/synoptic.fig.R")
## USED BY:
# 
## CREATES:
# Figures similar to Eric's figures in the MHW atlas
#############################################################################


# 1. Create list of events to be calculated -------------------------------

# length(SACTN_events$event_no) # 946

# Screen out those under 15 days in length
event_list <- filter(SACTN_events, duration >= 15) # 126

# Screen out those occurring before or after reanalysis period
event_list <- filter(event_list, date_start > as.Date("1994-01-01")) # 95
event_list <- filter(event_list, date_stop < as.Date("2016-08-31")) # 95


# 2. Mass produce the synoptic figures ------------------------------------

for(i in 1:nrow(event_list)){
  event <- event_list[i,]
  file_check <- paste0("graph/synoptic/",event$site[1],"_",event$event_no[1],".pdf")
  if(!(file.exists(file_check))){
    system.time(synoptic.fig(event))
  }
}


