#############################################################################
###"graph/SACTN.SAWS.line.R"
## This script does:
# 1. Load matched data and events
# 2. Plot line graph of temperatures and save
## DEPENDS ON:
library(doMC); registerDoMC(cores = 4)
library(ggplot2)
library(stringr)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(tidyr)
library(purrr)
library(broom)
source("setupParams/theme.R")
## USED BY:
# 
## CREATES:
# "graph/match_line.pdf"
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


# 2. Plot line graph of temperatures and save -----------------------------

# Reduce to monthly values
tsALL <- SACTN_SAWS_match
tsALL$date <- floor_date(tsALL$date, "month")
tsALL <- ddply(tsALL, .(facet, date, dataset, R2, R22), summarize,
               temp = mean(temp, na.rm = TRUE))

# Reorder sites for plotting
siteOrder <- c("Port Nolloth", "Sea Point", "Hout Bay", "Kommetjie", "Fish Hoek", "Muizenberg", "Gordons Bay", "Hermanus", "Ystervarkpunt", "Mossel Bay", "Knysna", "Tsitsikamma West", "Storms River Mouth", "Tsitsikamma East", "Pollock Beach", "Humewood", "Hamburg", "Eastern Beach", "Orient Beach", "Nahoon Beach", "Sodwana")

# Change site names for plotting on the side of the facets
newNames1 <- c("1. Port \n Nolloth", "2. Sea \n Point", "3. Hout \n Bay", "4. Kommetjie", "5. Fish \n Hoek", "6. Muizenberg", "7. Gordons \n Bay", "8. Hermanus", "9. Yster- \n varkpunt", "10. Mossel \n Bay", "11. Knysna", "12. Tsitsikamma \n West", "13. Storms River \n Mouth", "14. Tsitsikamma \n East", "15. Pollock \n Beach", "16. Humewood", "17. Hamburg", "18. Eastern \n Beach", "19. Orient \n Beach", "20. Nahoon \n Beach", "21. Sodwana")
# Change site names for plotting on the top of the facets
newNames2 <- c("1. PortNolloth", "2. Sea Point", "3. Hout Bay", "4. Kommetjie", "5. Fish Hoek", "6. Muizenberg", "7. Gordons Bay", "8. Hermanus", "9. Yster varkpunt", "10. Mossel Bay", "11. Knysna", "12. Tsitsikamma West", "13. Storms River Mouth", "14. Tsitsikamma East", "15. Pollock Beach", "16. Humewood", "17. Hamburg", "18. Eastern Beach", "19. Orient Beach", "20. Nahoon Beach", "21. Sodwana")
# Change site names for plotting only numbers
newNames3 <- c("1. ", "2. ", "3. ", "4. ", "5. ", "6. ", "7. ", "8. ", "9. ", "10. ", "11. ", "12. ", "13. ", "14. ", "15. ", "16. ", "17. ", "18. ", "19. ", "20. ", "21. ")

## Create second data.frame for plotting and first add coast labels
# tsALL2 <- data.frame()
# for(i in 1:length(levels(tsALL$site))) {
#   x <- subset(tsALL, site == levels(tsALL$site)[i])
#   if(x$site[1] %in% wc) {
#     x$coast <- "WC"
#   } else if(x$site[1] %in% sc) {
#     x$coast <- "SC"
#   } else if(x$site[1] %in% ec) {
#     x$coast <- "EC"
#   }
#   tsALL2 <- rbind(tsALL2, x)
# }

# Reorder sites
# tsALL2$site <- factor(tsALL2$site, levels = siteOrder)
# tsALL2$index <- paste(tsALL2$site, tsALL2$date, tsALL2$type, sep = "-")
# 
# # Change coast labels for plotting and add x y coords
# tsALL2$coast <- as.factor(tsALL2$coast)
# levels(mhwCoastCO2$coast) <- c("EC", "SC", "WC")
# tsALL2$x <- as.Date("1994-03-01"); tsALL2$y <- 18.5

### Run "proc/results2.R" first to populate the environment with the necessary data... ###
# 
# mhwn$event <- "MHW"
# mhwn$index2 <- paste(mhwn$site, mhwn$month, mhwn$type, sep = "-")
# 
# mcsn$event <- "MCS"
# mcsn$index2 <- paste(mcsn$site, mcsn$month, mcsn$type, sep = "-")
# 
# mhwnSST$event <- "MHW"
# mhwnSST$index2 <- paste(mhwnSST$site, mhwnSST$month, mhwnSST$type, sep = "-")
# 
# mcsnSST$event <- "MCS"
# mcsnSST$index2 <- paste(mcsnSST$site, mcsnSST$month, mcsnSST$type, sep = "-")

# Function to extract temperature during events
# eventTemp <- function(x){
#   events <- data.frame()
#   for(i in 1:nrow(x)){#nrow(x)){
#     x.1 <- droplevels(x[i,])
#     x.1$temp <- tsALL2$temp[tsALL2$index == x.1$index2]
#     events <- rbind(events, x.1)
#   }
#   events <- events[,28:34]
#   return(events)
# }
# 
# mhwn2 <- eventTemp(mhwn)
# mhwn2$site <- factor(mhwn2$site, levels = siteOrder)
# 
# mcsn2 <- eventTemp(mcsn)
# mcsn2$site <- factor(mcsn2$site, levels = siteOrder)
# 
# mhwnSST2 <- eventTemp(mhwnSST)
# mhwnSST2$site <- factor(mhwnSST2$site, levels = siteOrder)
# 
# mcsnSST2 <- eventTemp(mcsnSST)
# mcsnSST2$site <- factor(mcsn2$site, levels = siteOrder)
# 
# # Rename sites
# levels(tsALL2$site) <- newNames2
# levels(mhwn2$site) <- newNames2
# levels(mcsn2$site) <- newNames2
# levels(mhwnSST2$site) <- newNames2
# levels(mcsnSST2$site) <- newNames2

# Add x y coords for plotting R2 values
# resultsR2$site <- factor(resultsR2$site, levels = siteOrder)
# levels(resultsR2$site) <- newNames2
# resultsR2$x <- as.Date("1976-01-01"); resultsR2$y <- 25
# Manually change row order as other methods don't work...
# resultsR2 <- resultsR2[c(15,16,6,9,2,11,3,5,21,10,8,20,18,19,14,7,4,1,13,12,17),]

# Create data frame for plotting R^2 values
R2 <- tsALL[,c(1,5)]
R2 <- unique(R2)
R2$x <- as.Date("1976-01-01"); R2$y <- 25

## The figure
f2 <- ggplot(data = tsALL, aes(x = date, y = temp)) + bw_update +
  # geom_text(data = tsALL, aes(x = x, y = y, label = coast), size = 12, colour = "grey80") +
  geom_text(data = R2, aes(x = x, y = y, label = as.character(R2$R22), group = facet), parse = TRUE, size = 2.6) +
  # geom_line(data = tsALL[tsALL$dataset == "SAWS",], colour = "#41b6c4", alpha = 1.0, show.legend = F) +
  # geom_line(data = tsALL[tsALL$dataset == "SACTN",], #linetype = "dotted",
  #           colour = "#081d58", alpha = 0.8,  show.legend = F) +
  geom_line(aes(colour = dataset), alpha = 0.75) +
  # # SACTN MHW
  # geom_point(data = mhwn2, aes(x = month, y = temp), shape = 21, alpha = 0.9,
  #            colour = "black", fill = "#41b6c4", size = 2.7, show.legend = T) +
  # geom_text(data = mhwn2, aes(x = month, y = temp, label = index), size = 2.6, colour = "black") +
  # # SACTN MCS
  # geom_point(data = mcsn2, aes(x = month, y = temp), shape = 22, alpha = 0.9,
  #            colour = "black", fill = "#41b6c4", size = 2.7, show.legend = T) +
  # geom_text(data = mcsn2, aes(x = month, y = temp, label = index), size = 2.6, colour = "black") +
  # # SAWS MHW
  # geom_point(data = mhwnSST2, aes(x = month, y = temp), shape = 21, alpha = 0.9,
  #            colour = "black", fill = "#081d58", size = 2.7, show.legend = T) +
  # geom_text(data = mhwnSST2, aes(x = month, y = temp, label = index), colour = "white", size = 2.6) +
  # # SAWS MCS
  # geom_point(data = mcsnSST2, aes(x = month, y = temp), shape = 22, alpha = 0.9,
  #            colour = "black", fill = "#081d58", size = 2.7, show.legend = T) +
  # geom_text(data = mcsnSST2, aes(x = month, y = temp, label = index), colour = "white", size = 2.6) +
  # Additional stuff
  scale_y_continuous(breaks = c(15,25)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0.015,0)) +
  ylab(expression(paste("Temperature (", degree~C, ")"))) + xlab("Date") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  facet_wrap(~facet, ncol = 3)
f2
ggsave("graph/match_line.pdf", height = 8, width = 12)
