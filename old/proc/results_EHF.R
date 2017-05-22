## This script reads the .nc files produced by ehfheatwaves.py

# Load packages -----------------------------------------------------------
library(ncdf4)


# Load ACORNsat nc file as a control --------------------------------------

acorn <- nc_open("python/AUS/ACORNSAT.nc")
print(acorn)
datea <- ncatt_get(acorn, varid = "time")
dateb <- ncvar_get(acorn, varid = "time")
head(dateb)
tail(dateb)
stationa <- ncatt_get(acorn, varid = "stationid")
stationb <- ncvar_get(acorn, varid = "stationid")
head(stationb)
lon <- ncvar_get(acorn, varid = "lon")
lat <- ncvar_get(acorn, varid = "lat")

acorn3 <- nc_open("python/AUS/ACORNSAT3.nc")
print(acorn3)
dateb <- ncvar_get(acorn3, varid = "time")
head(dateb)
tail(dateb)
tmax <- ncvar_get(acorn3, varid = "tmax")

# Load results ------------------------------------------------------------

events <- nc_open("python/ehfheatwaves-station/station_daily_tx90pct.nc")
print(events)
tx90pct <- ncvar_get(events, varid = "tx90pct")
event <- ncvar_get(events, varid = "event")
ends <- ncvar_get(events, varid = "ends")
time <- ncvar_get(events, varid = "time")
station <- ncvar_get(events, varid = "station")

results <- data.frame(station = station, date = time, event = event, ends = ends, tx90pct = tx90pct)

results$station <- "Acorn"
results$date <- seq(as.Date("1910-01-01"), length.out = length(results$date), by = "day")

events <- nc_open("python/ehfheatwaves-station/station_tn90pct.nc")
print(events)



## These results don't seem to show anything...