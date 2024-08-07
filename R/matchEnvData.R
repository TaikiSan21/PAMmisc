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
#' @param depth depth values (meters) to use for matching, overrides any \code{Depth} column
#'   in the data or can be used to specify desired depth range when not present in data.
#'   Variables will be summarised over the range of these depth values. \code{NULL}
#'   uses all available depth values
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
#'                    UTC = as.POSIXct('2004-12-31 09:00:00', tz='UTC'))
#' \dontrun{
#' # Not run because downloads files
#' # default calculates mean, median, and standard deviation
#' matchEnvData(data, nc='jplMURSST41', var=c('analysed_sst', 'analysis_error'))
#' # get just mean within a buffer around coordinates
#' matchEnvData(data, nc='jplMURSST41', var=c('analysed_sst', 'analysis_error'),
#'              FUN = mean, buffer = c(.01, .01, 86400))
#' }
#' # Can also work from an existing nc file
#' nc <- system.file('extdata', 'sst.nc', package='PAMmisc')
#' matchEnvData(data, nc = nc)
#' # Using a custom function
#' meanPlusOne <- function(x) {
#'   mean(x, na.rm=TRUE) + 1
#' }
#' matchEnvData(data, nc=nc, FUN=c(mean, meanPlusOne))
#'
#'
#' @importFrom stats median sd
#' @importFrom methods setGeneric setMethod
#' @export
#'
setGeneric('matchEnvData',
           function(data, nc=NULL, var=NULL, buffer=c(0,0,0), FUN = c(mean),
                    fileName = NULL, progress=TRUE, depth=0, ...) standardGeneric('matchEnvData')
)

#' @rdname matchEnvData
#' @importFrom dplyr bind_rows bind_cols
#' @export
#'
setMethod('matchEnvData', 'data.frame',
          function(data, nc=NULL, var=NULL, buffer=c(0,0,0), FUN = c(mean),
                   fileName = NULL, progress=TRUE, depth=0, ...) {
              # First just get an edinfo
              if(is.null(nc)) {
                  nc <- browseEdinfo()
              }
              if(!all(c('Latitude', 'Longitude') %in% standardCoordNames(colnames(data)))) {
                  stop('Data must have columns "Latitude" and "Longitude"')
              }
              if(is.character(nc) &&
                 !file.exists(nc)) {
                  nc <- try(erddapToEdinfo(nc, chooseVars = FALSE))
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
                  return(ncToData(data=data, nc=nc, var=var, buffer=buffer, FUN=FUN, progress=progress, depth=depth, ...))
              }

              if(!inherits(nc, 'edinfo')) {
                  stop(paste0('"nc" must be a valid nc file or erddap dataset id.'))
              }
              if('UTC' %in% names(nc$limits) &&
                 !'UTC' %in% standardCoordNames(colnames(data))) {
                  stop('Data must have column "UTC"')
              }
              if(is.null(nc$varSelect) ||
                 !any(nc$varSelect)) {
                  nc <- varSelect(nc, var)
              }
              # browser()
              if(inherits(nc, 'hycomList')) {
                  whichHy <- sapply(data$UTC, function(t) {
                      whichHycom(t, nc$list)
                  })
                  naHy <- is.na(whichHy)
                  if(any(naHy)) {
                      message(sum(naHy), ' rows could not be matched to a HYCOM dataset, they will',
                              ' be removed from returned result.')
                  }
                  whichHy[naHy] <- -1
                  hys <- unique(whichHy)
                  result <- vector('list', length=length(hys))
                  fixer <- numeric(0)

                  for(i in seq_along(result)) {
                      if(hys[i] == -1) next
                      thisHy <- nc$list[[hys[i]]]
                      thisHy$varSelect <- nc$varSelect
                      result[[i]] <- matchEnvData(data[whichHy == hys[i], ], nc=thisHy, var, buffer, FUN, fileName, progress, depth, ...)
                      fixer <- c(fixer, which(whichHy == hys[i]))
                  }
                  fixer <- sort(fixer, index.return=TRUE)$ix
                  # browser()
                  if(is.data.frame(result[[1]])) {
                      result <- bind_rows(result)
                      return(result[fixer, ])
                  } else if(is.list(result[[1]])) {
                      result <- unlist(result, recursive = FALSE)
                      result <- fillNA(result[fixer], which(naHy))
                      return(result)
                  }
              }
              # no filename provided means dont download all, do smaller
              if(is.null(fileName)) {
                  plan <- as.character(planDownload(data, nc, thresh=20))

                  if(progress) {
                      cat('Downloading data...\n')
                      pb <- txtProgressBar(min=0, max = length(unique(plan)), style=3)
                      pbix <- 0
                  }
                  planFiles <- vector('list', length=length(unique(plan)))
                  names(planFiles) <- unique(plan)
                  for(p in unique(plan)) {
                      if(p == '-1') {
                          if(progress) {
                              pbix <- pbix + 1
                              setTxtProgressBar(pb, value = pbix)
                          }
                          next
                      }
                      thisFile <- fileNameManager(suffix=p)
                      planFiles[[p]] <- downloadEnv(data=data[plan == p, ],fileName = thisFile, edinfo = nc, buffer = buffer, ...)
                      #####################################
                      on.exit({
                          # tmpFiles <- list.files(getTempCacheDir(), full.names=TRUE)
                          # unlink(tmpFiles, force=TRUE)
                          tempDir <- getTempCacheDir()
                          unlink(tempDir, recursive=TRUE, force=TRUE)
                      })
                      # on.exit(DELETEYOUR PAMMISC TEMP DIR HERE) cache_delete_all from rerddap checkit
                      ######################################
                      if(progress) {
                          pbix <- pbix + 1
                          setTxtProgressBar(pb, value = pbix)
                      }
                  }
                  # result <- vector('list', length = nrow(data))
                  # for(i in seq_along(result)) {
                  #     if(plan[i] == '-1') {
                  #         result[[i]] <- data[i, ]
                  #         next
                  #     }
                  #     result[[i]] <- ncToData(data=data[i, ], nc=planFiles[[plan[i]]], var=var, buffer=buffer, FUN=FUN, progress=FALSE, depth=depth, ...)
                  # }
                  data <- split(data, plan)
                  result <- vector('list', length=length(data))
                  for(i in seq_along(result)) {
                      if(names(data)[i] == '-1') {
                          result[[i]] <- data[[i]]
                          next
                      }
                      result[[i]] <- ncToData(data=data[[i]], nc=planFiles[[names(data)[i]]], var=var, buffer=buffer,
                                                        FUN=FUN, progress=FALSE, depth=depth, ...)
                  }
                  if(progress) {
                      cat('\n')
                  }
                  if(is.data.frame(result[[1]])) {
                      result <- bind_rows(result)
                  } else if(is.list(result[[1]])) {
                      result <- unlist(result, recursive = FALSE)
                  }
                  return(result)
              }

              # file name provided means get big all at once
              if(!grepl('\\.nc$', fileName)) {
                  fileName <- paste0(fileName, '.nc')
              }
              ncData <- downloadEnv(data=data, edinfo = nc, fileName = fileName, buffer = buffer, ...)
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
                  return(bind_rows(ncToData(data=dataLeft, nc=ncData[1], var=var, buffer=buffer, FUN=FUN, progress=progress, depth=depth, ...),
                                   ncToData(data=dataRight, nc=ncData[2], var=var, buffer=buffer, FUN=FUN, progress=progress, depth=depth, ...))
                  )
              }
              return(ncToData(data=data, nc=ncData, var=var, buffer=buffer, FUN=FUN, progress=progress, depth=depth, ...))
          })

fillNA <- function(x, ix) {
    if(length(ix) == 0) {
        return(x)
    }
    lessIx <- seq_along(x) < ix[1]
    fillNA(c(x[lessIx], NA, x[!lessIx]), ix[-1])
}

