# ship polar plots
pmDir <- 'X:/ANALYSIS/ADRIFT/Sperm whales/Logs/'
pmFiles <- list.files(pmDir, full.names=TRUE, pattern='xlsx?')
source('devel/binDetectionFunctions.R')

shipDir <- 'X:/ANALYSIS/ADRIFT/Soundscape/Ship Detections/logs'
shipFiles <- list.files(shipDir, full.names=TRUE, pattern='xlsx?')

#### Checking for problems with Excel dates lol
out <- vector('list', length=length(shipFiles)) 
for(i in seq_along(out)) {
    cat(i, '\n')
    out[[i]] <- loadTritonLog(shipFiles[i])
}
names(out) <- sapply(out, function(x) unique(x$DriftName))
bind_rows(out)

bind_rows(out) %>% 
    group_by(DriftName) %>% 
    summarise(start=min(UTC, na.rm=TRUE),
              end=max(end, na.rm=TRUE)) %>% 
    View

lapply(out, function(x) {
    is.numeric(x$UTC) | is.numeric(x$end) | any(is.na(x$UTC))
})

shipData <- loadTritonLog(shipFiles)
pmData <- loadTritonLog(pmFiles)
table(pmData$DriftName, pmData$call)
unique(shipData$DriftName) %in% pmData$DriftName

shipAll <- bind_rows(shipData, pmData)
shipAll <- filter(shipAll, grepl('ADRIFT', DriftName))

shipAll <- filter(shipAll,
                  grepl('Ship', call))
table(shipAll$call)
gps <- readRDS('../DriftWatch/AllDeploymentGPS.rds')
gpsAdrift <- filter(gps, grepl('ADRIFT', DriftName))
unique(gpsAdrift$DriftName) %in% shipAll$DriftName
shipBin <- formatBinnedPresence(shipAll, gps=gpsAdrift, bin='hour')

eff <- getDbEffort('../DriftWatch/SPOTGPS_Logger.sqlite3')
eff <- filter(eff, grepl('ADRIFT', DriftName))
shipBinEff <- formatBinnedPresence(shipAll, effort=eff, gps=gps, bin='hour')
table(shipBin$DriftName, is.na(shipBin$Latitude))



eff <- getDbEffort('../DriftWatch/SPOTGPS_Logger.sqlite3', bin='hour')
eff <- filter(eff, grepl('ADRIFT', DriftName))
shipHour <-shipAll %>% 
    formatBinnedPresence(effort=eff, gps=gps, bin='hour')

source('../PAMscapes/devel/reportPlotFunctions.R')
plotShipSR <- function(x, region, season) {
    plotRadialPresence(x, bin='hour', title=paste0(region, '_', season)) +
        scale_fill_viridis_c(limits=c(0, 0.5)) #+
        # scale_y_continuous(limits=c(0, 0.5))
}
shipPlot <- regionSeasonPlot(shipHour, plotShipSR)
ggsave('ShipSameColor.png', shipPlot, width=12, height=12)
plotRadialPresence(shipHour, 'month')$plot

eff <- getDbEffort('../DriftWatch/SPOTGPS_Logger.sqlite3', bin='month')
eff <- filter(eff, grepl('ADRIFT', DriftName))
shipMonth <-shipAll %>% 
    formatBinnedPresence(effort=eff, gps=gps, bin='month')
plotRadialPresence(shipMonth, 'month')$plot
