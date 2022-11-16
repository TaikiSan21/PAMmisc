csv <- '~/../Downloads/CCES_2018_Drift_07_BlueWhale_Log.xls - Detections.csv'
df <- read.csv(csv, stringsAsFactors = F)
library(dplyr)
library(lubridate)
df$UTC <- mdy_hms(df$Start.time)

library(ggplot2)
library(suncalc)
gps <- read.csv('../DriftWatch/GPS_CSV/ADRIFT_007_GPS.csv', stringsAsFactors = FALSE)
gps$UTC <- ymd_hms(gps$UTC)


library(patchwork)
library(PAMpal)
click <- getClickData(readRDS('../../CV4Ecology/rds/PASCAL_Base_AcSt.rds'))
barSpecMD <- plotCallBar(click, type='presence', by='species', presBin='minute', timeBin='day')
barSpecHW <- plotCallBar(click, type='presence', by='species', presBin='hour', timeBin='week')
barDens <- plotCallBar(click, type='density')
barDensSP <- plotCallBar(click, type='density', by='detectorName', timeBin='day')
(barSpecMD + barSpecHW) / (barDens + barDensSP)


gridH <- plotCallGrid(df, unit='hour', type='presence', gps=gps)
gridDens <- plotCallGrid(click, unit='30min', type='density')
grid15 <- plotCallGrid(df, unit='15min', type='presence', gps=gps)
gridDensH <- plotCallGrid(click, unit='hour', type='density')
(gridH + grid15) / (gridDens + gridDensH)

df <- data.frame(UTC = as.POSIXct(runif(1e2, min=0, max=7*24*3600), origin='1970-01-01 00:00:00', tz='UTC'),
                 label = sample(letters[1:3], 1e3, replace=T))
plotCallBar(df, type='density', timeBin='day', presBin='hour', by='label')

plotCallGrid(df, timeBin='hour', type='density')
gps <- data.frame(UTC = as.POSIXct('1970-01-01 00:00:00', tz='UTC'),
                  Latitude=32.4,
                  Longitude = -118)
plotCallGrid(df, gps=gps, timeBin='hour')
