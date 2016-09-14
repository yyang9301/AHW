# Load packages -----------------------------------------------------------

# Load data ----------------------

load("data/SAWS/SAWS_data_clean.Rdata")
SAWS_data_clean$station <- as.character(SAWS_data_clean$station)

short <- c("")

## Remove very short time series
SAWS_data_clean <- SAWS_data_clean[-c(SAWS_data_clean$station == "GRABOUW"),]

# Change format to match load_acornsat requirements ---------------------------
## NB: load_acornsat.py produces the NetCDF files used by ehfheatwaves.py

ehf <- SAWS_data_clean

## Change date stamp
ehf$date <- as.character(ehf$date)
