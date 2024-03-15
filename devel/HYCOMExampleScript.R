# HYCOM download script examples
# Downloading daily HYCOM files for full grids
# and smaller files for modeling segments
#
# 2024-01-11 : First version
# 2024-01-23 : Adding support for matching to specific hour/tzone for grid

# Change this path to wherever this file lives
source('devel/HYCOMFunctions.R')

# The process for getting environmental data both for full grids
# and for the segment data is similar
# Step 1: Download data to a folder with "downloadHYCOM"
# Step 2: Either create daily CSVs for full grids with "makeDailyCSV"
#         or match the environmental data to the segments with
#         "addEnvParams"

#### Example 1 - Download Daily Files for Full Model Grid ####
# Define the range of coordinates for your full grid
latRange <- c(34.44, 51.1)
lonRange <- c(-131.5, -120)
# this can be either a Date or POSIXct. Start with a small date range to test.
timeRange <- as.Date(c('2018-12-01', '2018-12-02'))

rangeDf <- data.frame(
    Latitude = latRange,
    Longitude = lonRange,
    UTC = timeRange)

# this is where to store .nc and .csv files
gridFolder <- 'ProjectName_FullGrid'

# "hour" is the hour of day we want to download data for
# "hourTz" is the timezone of that hour
# So noon local would be hour=12, hourTz='America/Los_Angeles'
# You can just continually re-run this function and it will re-start where
# it either failed or hasn't tried to download that day yet, and it
# will slowly work through the entire range of dates given
fullLog <- downloadHYCOM(data=rangeDf, folder=gridFolder, mode='full', 
                         hour=12, hourTz='America/Los_Angeles')
# Creating the daily CSV files, first load the grid point data
# point this to whatever CSV file has your grid points
grid <- read.csv('../Data/EKEnv/BAJA_0.08deg_2018-12-01.csv', stringsAsFactors = FALSE)

# Then run makeDailyCSV
# this will create the daily CSV files with environmental parameters attached to those grid
# points for each day that you have downloaded a NetCDF file. 
# You can just re-run this, and it will skip over days it has already created CSVs for
makeDailyCSV(grid, folder=gridFolder, name='GridTestHour', hour=12, hourTz='America/Los_Angeles')

#### Example 2 - Download Smaller Files for Segment Data ####

segFile <- '../Data/EKEnv/BajaStudyArea_Modeling_Segs_forTaiki.csv'
segData <- read.csv(segFile, stringsAsFactors = FALSE)
# Folder to store the downloaded segment NetCDF files
segFolder <- 'ProjectName_Segment'
# Like the full grid, this will work throuugh downloading all the data you need
# the "tz" argument is used if the times in your dataset are not in UTC
segLog <- downloadHYCOM(data=segData, folder=segFolder, mode='segment', tz='America/Los_Angeles')
# Once the downloads are all finished, use this to match the downloaded data to 
# your segment dataframe. Then you can save this as a CSV and you're all set!
segData <- addEnvParams(segData, folder=segFolder, tz='America/Los_Angeles')
