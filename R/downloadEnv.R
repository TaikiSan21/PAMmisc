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
    colnames(data) <- standardCoordNames(colnames(data))
    fileName <- fileNameManager(fileName)
    # check cross dateline, only do something if env is
    crossDateline <- checkDateline(data)
    if(crossDateline) {
        left <- to180(data$Longitude) > 0
        dataLeft <- data[left, ]
        dataRight <- data[!left, ]
        return(c(downloadEnv(dataLeft, edinfo, fileNameManager(fileName, 'leftLong'), buffer),
                 downloadEnv(dataRight, edinfo, fileNameManager(fileName, 'rightLong'), buffer)))
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
    checkonly <- checkLimits(data, edinfo, replace=TRUE, quiet=FALSE)
    dataBounds <- checkLimits(dataBounds, edinfo, replace=TRUE, quiet=TRUE)


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
        if(envData$status_code == 400) {
            stop(paste0('URL ', envData$url, ' is invalid, pasting this into a browser may give more information.'))
        }
        if(inherits(envData, 'try-error')) {
            nTry <- nTry + 1
            next
        } else {
            return(invisible(fileName))
        }
    }
    FALSE
}
