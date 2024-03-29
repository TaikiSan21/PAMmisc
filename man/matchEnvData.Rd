% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchEnvData.R
\name{matchEnvData}
\alias{matchEnvData}
\alias{matchEnvData,data.frame-method}
\title{Match Data From an Existing Netcdf File or Download and Match}
\usage{
matchEnvData(
  data,
  nc = NULL,
  var = NULL,
  buffer = c(0, 0, 0),
  FUN = c(mean),
  fileName = NULL,
  progress = TRUE,
  depth = 0,
  ...
)

\S4method{matchEnvData}{data.frame}(
  data,
  nc = NULL,
  var = NULL,
  buffer = c(0, 0, 0),
  FUN = c(mean),
  fileName = NULL,
  progress = TRUE,
  depth = 0,
  ...
)
}
\arguments{
\item{data}{dataframe containing Longitude, Latitude, and UTC to extract matching
variables from the netcdf file}

\item{nc}{name of a netcdf file, ERDDAP dataset id, or an edinfo object}

\item{var}{(optional) vector of variable names}

\item{buffer}{vector of Longitude, Latitude, and Time (seconds) to buffer around
each datapoint. All values within the buffer will be used to report the mean,
median, and standard deviation}

\item{FUN}{a vector or list of functions to apply to the data. Default is to apply
mean, median, and standard deviation calculations}

\item{fileName}{(optional) file name to save downloaded nc file to. If not provided,
then no nc files will be stored, instead small temporary files will be downloaded
and then deleted. This can be much faster, but means that the data will need to be
downloaded again in the future. If \code{fileName} is provided, then the function
will attempt to download a single nc file covering the entire range of your data.
If your data spans a large amount of time and space this can be problematic.}

\item{progress}{logical flag to show progress bar}

\item{depth}{depth values (meters) to use for matching, overrides any \code{Depth} column
in the data or can be used to specify desired depth range when not present in data.
Variables will be summarised over the range of these depth values. \code{NULL}
uses all available depth values}

\item{\dots}{other parameters to pass to \link{ncToData}}
}
\value{
original dataframe with three attached columns for each variable in the netcdf
  file, one for each of mean, median, and standard deviation of all values within the buffer
}
\description{
Extracts all variables from a netcdf file matching Longitude,
  Latitude, and UTC coordinates in given dataframe
}
\examples{
data <- data.frame(Latitude = 32, Longitude = -117,
                   UTC = as.POSIXct('2004-12-31 09:00:00', tz='UTC'))
\dontrun{
# Not run because downloads files
# default calculates mean, median, and standard deviation
matchEnvData(data, nc='jplMURSST41', var=c('analysed_sst', 'analysis_error'))
# get just mean within a buffer around coordinates
matchEnvData(data, nc='jplMURSST41', var=c('analysed_sst', 'analysis_error'),
             FUN = mean, buffer = c(.01, .01, 86400))
}
# Can also work from an existing nc file
nc <- system.file('extdata', 'sst.nc', package='PAMmisc')
matchEnvData(data, nc = nc)
# Using a custom function
meanPlusOne <- function(x) {
  mean(x, na.rm=TRUE) + 1
}
matchEnvData(data, nc=nc, FUN=c(mean, meanPlusOne))


}
\author{
Taiki Sakai \email{taiki.sakai@noaa.gov}
}
