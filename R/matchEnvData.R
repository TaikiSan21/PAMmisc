#' @title Match Data From an Existing Netcdf File or Download and Match
#'
#' @description Extracts all variables from a netcdf file matching Longitude,
#'   Latitude, and UTC coordinates in given dataframe
#'
#' @param data dataframe containing Longitude, Latitude, and UTC to extract matching
#'   variables from the netcdf file
#' @param nc name of a netcdf file, ERDDAP dataset id, or an edinfo object
#' @param var (optional) vector of variable names
#' @param buffer vector of Longitude, Latitude, and Time (seconds) to buffer around
#'   each datapoint. All values within the buffer will be used to report the mean,
#'   median, and standard deviation
#' @param FUN a vector or list of functions to apply to the data. Default is to apply
#'   mean, median, and standard deviation calculations
#' @param fileName (optional) file name to save downloaded nc file to. If not provided,
#'   then no nc files will be stored, instead small temporary files will be downloaded
#'   and then deleted. This can be much faster, but means that the data will need to be
#'   downloaded again in the future. If \code{fileName} is provided, then the function
#'   will attempt to download a single nc file covering the entire range of your data.
#'   If your data spans a large amount of time and space this can be problematic.
#' @param progress logical flag to show progress bar
#' @param \dots other parameters to pass to \link{ncToData}
#'
#' @return original dataframe with three attached columns for each variable in the netcdf
#'   file, one for each of mean, median, and standard deviation of all values within the buffer
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @name matchEnvData
#'
#' @examples
#' data <- data.frame(Latitude = 32, Longitude = -117,
#'                    UTC = as.POSIXct('2000-01-01 00:00:00', tz='UTC'))
#' \dontrun{
#' # Not run because downloads files
#' sstEdi <- getEdinfo()[['jplMURSST41']]
#' sstEdi <- varSelect(sstEdi, TRUE)
#' # default calculates mean, median, and standard deviation
#' matchEnvData(data, sstEdi)
#' # get just mean within a buffer around coordinates
#' matchEnvData(data, sstEdi, FUN = mean, buffer = c(.01, .01, 86400))
#' # Can also work from an existing nc file
#' nc <- downloadEnv(data, sstEdi, buffer = c(.01, .01, 86400))
#' matchEnvData(data, nc = nc)
#' # Using a custom function
#' meanPlusOne <- function(x) {
#'   mean(x, na.rm=TRUE) + 1
#' }
#' matchEnvData(data, nc=nc, FUN=c(mean, meanPlusOne))
#' }
#'
#' @importFrom stats median sd
#' @importFrom methods setGeneric setMethod
#' @export
#'
setGeneric('matchEnvData',
           function(data, nc=NULL, var=NULL, buffer=c(0,0,0), FUN = c(mean, median, sd),
                    fileName = NULL, progress=TRUE, ...) standardGeneric('matchEnvData')
)

#' @rdname matchEnvData
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom rerddap cache_delete
#' @importFrom hoardr hoard
#' @export
#'
setMethod('matchEnvData', 'data.frame',
          function(data, nc=NULL, var=NULL, buffer=c(0,0,0), FUN = c(mean, median, sd),
                   fileName = NULL, progress=TRUE, ...) {
              # First just get an edinfo
              if(is.null(nc)) {
                  nc <- browseEdinfo(var=var)
              }
              if(is.character(nc) &&
                 !file.exists(nc)) {
                  nc <- try(erddapToEdinfo(nc, chooseVars = TRUE))
                  if(inherits(nc, 'try-error')) {
                      stop(paste0(nc, ' must be a valid nc file or erddap dataset id.'))
                  }
              }
              # browser()
              if(is.list(FUN) &&
                 is.null(names(FUN))) {
                  names(FUN) <- as.character(substitute(FUN))[-1]
              } else if(is.function(FUN)) {
                  tmpName <- as.character(substitute(FUN))
                  FUN <- list(FUN)
                  names(FUN) <- tmpName
              }
              # if pointing to an ncfile, just do that
              if(is.character(nc) &&
                 file.exists(nc)) {
                  return(ncToData(data=data, nc=nc, buffer=buffer, FUN=FUN, progress=progress, ...))
              }
              
              if(!inherits(nc, 'edinfo')) {
                  stop(paste0(nc, ' must be a valid nc file or erddap dataset id.'))
              }
              if(is.null(nc$varSelect) ||
                 !any(nc$varSelect)) {
                  nc <- varSelect(nc)
              }
              if(inherits(nc, 'hycomList')) {
                  data$whichHy <- sapply(data$UTC, function(t) {
                      whichHycom(t, nc$list)
                  })
                  naHy <- is.na(data$whichHy)
                  if(any(naHy)) {
                      message(sum(naHy), ' rows could not be matched to a HYCOM dataset, they will',
                              ' be removed from returned result.')
                  }
                  return(bind_rows(lapply(split(data, data$whichHy), function(x) {
                      thisHy <- nc$list[[x$whichHy[1]]]
                      thisHy$varSelect <- nc$varSelect
                      x$whichHy <- NULL
                      matchEnvData(x, nc=thisHy, var, buffer, FUN, fileName, progress, ...)
                  })))
              }
              # no filename provided means dont download all, do smaller
              if(is.null(fileName)) {
                  plan <- planDownload(data, nc, thresh=20)
                  data$DLTEMPID <- 1:nrow(data)
                  result <- vector('list', length = length(unique(plan)))
                  if(progress) {
                      cat('Downloading data...\n')
                      pb <- txtProgressBar(min=0, max = length(result), style=3)
                  }
                  for(i in seq_along(result)) {
                      ncData <- downloadEnv(data=data[plan == unique(plan)[i], ], edinfo = nc, buffer = buffer)
                      #####################################
                      # on.exit(DELETEYOUR PAMMISC TEMP DIR HERE) cache_delete_all from rerddap checkit
                      on.exit({
                          tmpFiles <- list.files(hoard()$cache_path_set('PAMmisc'), full.names=TRUE)
                          unlink(tmpFiles, force=TRUE)
                      })
                      ######################################
                      result[[i]] <- ncToData(data=data[plan == unique(plan)[i], ], nc=ncData, buffer=buffer, FUN=FUN, progress=FALSE, ...)
                      if(progress) {
                          setTxtProgressBar(pb, value = i)
                      }
                  }
                  cat('\n')
                  result <- bind_rows(result)
                  result <- arrange(result, .data$DLTEMPID)
                  result[['DLTEMPID']] <- NULL
                  return(result)
              }

              # file name provided means get big all at once
              if(!grepl('\\.nc$', fileName)) {
                  fileName <- paste0(fileName, '.nc')
              }
              ncData <- downloadEnv(data=data, edinfo = nc, fileName = fileName, buffer = buffer)
              # browser()
              if(length(ncData) > 1) {
                  message('Data crossed the dateline, download split into two files: ',
                             ncData[1], ' and ', ncData[2])
                  oldNames <- colnames(data)
                  colnames(data) <- standardCoordNames(colnames(data))
                  left <- to180(data$Longitude) > 0
                  colnames(data) <- oldNames
                  dataLeft <- data[left, ]
                  dataRight <- data[!left, ]
                  return(bind_rows(ncToData(data=dataLeft, nc=ncData[1], buffer=buffer, FUN=FUN, progress=progress, ...),
                                   ncToData(data=dataRight, nc=ncData[2], buffer=buffer, FUN=FUN, progress=progress, ...))
                  )
              }
              return(ncToData(data=data, nc=ncData, buffer=buffer, FUN=FUN, progress=progress, ...))
          })
