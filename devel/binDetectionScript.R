source('devel/binDetectionFunctions.R')
# Very brief explanation of functions
logFolder <- '../Data/Plotoff/ADRIFT Blue Whale Logs/'
logFiles <- list.files(logFolder, full.names=TRUE)
# loadDetectionData reads in data and formats it, it is expecting
# times, species id, optionally call type (can be NA), and needs
# to figure out a drift name.
# here source='triton' it knows the format of triton logs, can either be
# a CSV of the "detection" sheet or the .XLS file and it will
# grab the correct sheet
# For figuring out the drift name, if it is reading from a file it
# is expecting the pattern NAME_NUMBER_everything else, then it
# can parse it properly. So ADRIFT_006_whateveroyuwant.csv or
# PASCAL_01_otherstuff.xls will work fine. For other patterns
# you can specify the "driftPattern" argument (this is a slightly
# tricky regular expression thing) or "driftName"
# directly if it can't be parsed from the file name
detData <- loadDetectionData(logFiles[1], source='triton')
str(detData)
gps <- readRDS('../Data/Plotoff/AllDeploymentGPS.rds')
# formatBinnedPresence turns that data into binned hourly presence. This uses
# the GPS data to infer the times we have effort based on the first/last GPS point.
# I can adjust this to be more precise based on other info later. This also
# adds the GPS to the binned data, times without effort have NA values for "species"
# and "call" columns.
binned <- formatBinnedPresence(detData, bin='hour', gps=gps)
# this can then be used to plot yearly detections, this function is
# currently buggy and only works for showing percent style presence data
plotYearlyPresence(binned)

# example reading an XLS triton log
xl <- '../Data/Plotoff/PASCAL_006_BlueWhale_Log.xls'
detXl <- loadDetectionData(xl, source='triton')
binXl <- formatBinnedPresence(detXl, gps=gps)
plotYearlyPresence(binXl)

# example of a csv file. It must have column "UTC", then you can tell it
# the column names containing data for "species" and "call". If there is
# no call type data, "typeCol" can be set to NULL. Also example
# of manually assigning drift name for this one
csvData <- loadDetectionData(logFiles[1], source='csv',
                             speciesCol='Species.Code',
                             typeCol = 'Call',
                             driftName ='ADRIFT_001')
# these should be the same since its the same triton log file
identical(csvData[c('UTC', 'species', 'call', 'DriftName')],
          detData[c('UTC', 'species', 'call', 'DriftName')])

# Example of how you'd do this for a full folder of log files
# and combine them to make one plot. GPS data just needs to have
# coords for all drifts present in the data, this is why we
# need to parse the drift name from the log files so that
# we can do this matching.
allDet <- bind_rows(lapply(logFiles, function(x) {
    loadDetectionData(x, source='triton')
}))
allBinned <- formatBinnedPresence(allDet, gps=gps)

# hopefully this works and looks nice!
plotYearlyPresence(allBinned, title='Combined Bm Hourly Presence')

# Can now use the "by" argument to plot multiple panels as one
# and it will label them. Toy example here relabeling so we have
# only 3 sites (more than 3 looks bad for "by")
allBinned$DeploymentSite[allBinned$DeploymentSite %in%
                             c('HMB', 'SF')] <- 'SFB'
plotYearlyPresence(allBinned, by='DeploymentSite', title='Bm Hourly Presence by Site')
