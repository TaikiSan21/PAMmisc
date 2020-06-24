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
#'
#' @return if download is successful, invisibly returns the filename. If it fails returns
#'   \code{FALSE}.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import httr
#' @importFrom rerddap info
#'
#' @export
#'
downloadEnv <- function(data, edinfo, fileName = NULL, buffer = c(0, 0, 0)) {
    if(is.character(edinfo)) {
        # do some erddap info checking shit and make a URL for it
        # list above needs base, vars, dataset, fileType, source
        info <- suppressMessages(try(rerddap::info(edinfo)))
        if(inherits(info, 'try-error')) {
            stop('Not a valid erddap dataset')
        }
        edinfo <- erddapToEdinfo(info)
        return(downloadEnv(data, edinfo, fileName, buffer))
    }
    fileName <- fileNameManager(fileName)
    buffer <- bufferToSpacing(buffer, edinfo)
    dataBounds <- dataToRanges(data, buffer)
    # ensures limits are within range of dataset, first call is just to report number of points outside range
    checkonly <- checkLimits(data, edinfo, replace=TRUE, quiet=FALSE)
    dataBounds <- checkLimits(dataBounds, edinfo, replace=TRUE, quiet=TRUE)
    dataBounds$Longitude <- to180(dataBounds$Longitude, inverse = !edinfo$is180)

    # check cross dateline, only do something if env is
    crossDateline <- checkDateline(dataBounds)
    if(crossDateline) {
        dataPos <- data[data$Longitude >= 0, ]
        dataNeg <- data[data$Longitude < 0, ]
        return(c(downloadEnv(dataPos, edinfo, fileNameManager(fileName, 'posLong'), buffer),
                 downloadEnv(dataNeg, edinfo, fileNameManager(fileName, 'negLong'), buffer)))
    }

    url <- edinfoToURL(edinfo, ranges=dataBounds)
    # hm shit need some tempdir stuff, either make one in wd with a weird name, or tempdir(),
    # or steal rerddap:::rrcache$cache_path_get() lol

    # FOR FILE NAMES LETS DO "DATASET NAME_NUMBER"
    maxTries <- 3
    nTry <- 1
    while(nTry <= maxTries) {
        envData <- try(suppressMessages(httr::GET(url,
                                                  # verbose(),
                                                  progress(),
                                                  write_disk(fileName, overwrite = TRUE))))
        if(inherits(envData, 'try-error')) {
            nTry <- nTry + 1
            next
        } else {
            return(invisible(fileName))
        }
    }
    FALSE
}
