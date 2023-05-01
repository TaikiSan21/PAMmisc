#' @title Download Environmental Data
#'
#' @description Downloads environmental data matching the coordinates in a set of data
#'
#' @param data Data containing Longitude, Latitude, and UTC to download matching
#'   environmental data for
#' @param edinfo either a edinfo object from \link{getEdinfo} or \link{erddapToEdinfo}
#'   or an ERDDAP dataset ID
#' @param fileName name of the file to save downloaded data. If left as the default
#'   \code{NULL}, data will be saved to a temporary folder
#' @param buffer numeric vector of the amount to buffer the Longitude, Latitude, and
#'   UTC coordinates by
#' @param timeout number of seconds before timeout stops download attempt
#' @param progress logical flag to show download progress
#' @param \dots not used
#'
#' @return if download is successful, invisibly returns the filename. If it fails returns
#'   \code{FALSE}.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return If successful, the file name of downloaded data. If not, returns \code{FALSE}
#'
#' @examples
#'
#' data <- data.frame(Latitude = 32, Longitude = -117,
#'                    UTC = as.POSIXct('2000-01-01 00:00:00', tz='UTC'))
#' \dontrun{
#' # not run because download could take time
#' # download jplMURSST41 dataset
#' edi <- erddapToEdinfo('jplMURSST41')
#' ncFile <- downloadEnv(data, edi, 'sstData.nc')
#'
#' # browse suggested sst datasets, then download
#' edi <- browseEdinfo(var='sst')
#' ncFile <- downloadEnv(data, edi, 'sstData.nc')
#' }
#'
#'
#' @import httr
#' @importFrom rerddap info
#'
#' @export
#'
downloadEnv <- function(data, edinfo, fileName = NULL, buffer = c(0, 0, 0), timeout=120, progress=TRUE, ...) {
    if(is.character(edinfo)) {
        # do some erddap info checking shit and make a URL for it
        # list above needs base, vars, dataset, fileType, source
        info <- suppressMessages(try(info(edinfo)))
        if(inherits(info, 'try-error')) {
            stop('Not a valid erddap dataset')
        }
        edinfo <- erddapToEdinfo(info)
        return(downloadEnv(data, edinfo, fileName, buffer, timeout, progress, ...))
    }
    colnames(data) <- standardCoordNames(colnames(data))
    fileName <- fileNameManager(fileName)
    # check cross dateline, only do something if env is
    crossDateline <- checkDateline(data)
    if(crossDateline) {
        left <- to180(data$Longitude) > 0
        dataLeft <- data[left, ]
        dataRight <- data[!left, ]
        return(c(downloadEnv(dataLeft, edinfo, fileNameManager(fileName, 'leftLong'), buffer, timeout, progress, ...),
                 downloadEnv(dataRight, edinfo, fileNameManager(fileName, 'rightLong'), buffer, timeout, progress, ...)))
    }

    buffer <- bufferToSpacing(buffer, edinfo)
    data <- to180(data, inverse = !edinfo$is180)
    dataBounds <- dataToRanges(data, buffer)
    # dataBounds$Longitude <- to180(dataBounds$Longitude, inverse = !edinfo$is180)
    if(checkDateline(dataBounds)) {
        if(dataBounds$Longitude[1] < edinfo$limits$Longitude[1]) {
            dataBounds$Longitude[1] <- edinfo$limits$Longitude[1]
        }
        if(dataBounds$Longitude[2] > edinfo$limits$Longitude[2]) {
            dataBounds$Longitude[2] <- edinfo$limits$Longitude[2]
        }
    }
    # ensures limits are within range of dataset, first call is just to report number of points outside range
    checkonly <- checkLimits(data, edinfo, replace=TRUE, verbose=TRUE)
    dataBounds <- checkLimits(dataBounds, edinfo, replace=TRUE, verbose=FALSE)


    url <- edinfoToURL(edinfo, ranges=dataBounds)
    # hm shit need some tempdir stuff, either make one in wd with a weird name, or tempdir(),
    # or steal rerddap:::rrcache$cache_path_get() lol

    # FOR FILE NAMES LETS DO "DATASET NAME_NUMBER"
    maxTries <- 2
    nTry <- 1
    while(nTry <= maxTries) {
        if(progress) {
            envData <- try(suppressMessages(GET(url,
                                                # verbose(),
                                                progress(),
                                                timeout(timeout),
                                                write_disk(fileName, overwrite = TRUE))))
        } else {
            envData <- try(suppressMessages(GET(url,
                                                # verbose(),
                                                timeout(timeout),
                                                write_disk(fileName, overwrite = TRUE))))
        }
        if(inherits(envData, 'try-error')) {
            nTry <- nTry + 1
            warning('URL ', url, ' failed to download with message "',
                    envData[1], '", trying again...')
            next
        }
        if(envData$status_code != 200) {
            stop(paste0('URL ', envData$url, ' is invalid, pasting this into a browser may give more information.'))
        }
        # if status code == 200 we good
        return(invisible(fileName))
    }
    FALSE
}
