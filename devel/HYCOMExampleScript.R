# HYCOM download script examples
# Downloading daily HYCOM files for full grids
# and smaller files for modeling segments
#
# First version 2024-01-11

# Change this path to wherever this file lives
source('devel/HYCOMFunctions.R')

# The process for getting environmental data both for full grids
# and for the segment data is the same:
# Step 1: Download data to a folder with "downloadHYCOM"
# Step 2: Match that data to grid/segments with "addEnvParams"

#### Example 1 - Download Daily Files for Full Model Grid ####
pixelSize <- .08
latRange <- c(23, 34.5)
lats <- seq(from=latRange[1], to=latRange[2], by=pixelSize)
lonRange <- c(232.56, 250)
lons <- seq(from=lonRange[1], to=lonRange[2], by=pixelSize)
# this can be either a Date or POSIXct
timeRange <- as.Date(c('2018-10-01', '2018-10-08'))
grid <- data.frame(
    Latitude = rep(lats, length(lons)),
    Longitude = rep(lons, each=length(lats))
)
grid$UTC <- timeRange[1]
grid$UTC[1] <- timeRange[2]
gridFolder <- 'ProjectName_FullGrid'
# add example reading full grid
fullLog <- downloadHYCOM(data=grid, folder=gridFolder, mode='full')

grid <- addEnvParams(grid, folder=gridFolder)
# make CSV for each NC? they come with grid? This part has Qs
# input grid lat/long, create CSV for each .nc in folder
grid <- read.csv('../Data/EKEnv/BAJA_0.08deg_2018-12-01.csv', stringsAsFactors = FALSE)
makeDailyCSV(grid, folder='ProjectName_FullGrid', name='GridTest')
#### Example 2 - Download Smaller Files for Segment Data ####

segFile <- '../Data/EKEnv/BajaStudyArea_Modeling_Segs_forTaiki.csv'
segData <- read.csv(segFile, stringsAsFactors = FALSE)
segData <- head(segData, 100)
segFolder <- 'ProjectName_Segment'
segLog <- downloadHYCOM(data=segData, folder=segFolder, mode='segment', tz='America/Los_Angeles')

segData <- addEnvParams(segData, folder=segFolder, tz='America/Los_Angeles')


# QUESTIONS

# Final format / naming for both? Create daily CSV file for each .nc
# Are you going to have the base grid CSV as input (w/out params)
# or do you want it created from coordinates? AWk part if creating from
# .nc coords is when .04, need to choose



